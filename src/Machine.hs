{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Machine
( Stack
, Machine
, parseAndExecuteQuery
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Data.List
import System.Random
import Text.RawString.QQ

import qualified Data.Text as T

import Exceptions
import Operations

-- | The stack is a simple list of arbitrary-precision integers.
type Stack = [Integer]

-- | The machine uses a stack and StdGen for its state, and produces a list of
-- text as output, or possibly an exception
type Machine a = ExceptT Exception (RWS () [T.Text] (Stack, Int, StdGen)) a

-- | Get only the current stack from a Machine.
getStack :: Machine Stack
getStack = do
    (stack, _, _) <- get
    return stack

-- | Get only the current stack size from a Machine.
getSize :: Machine Int
getSize = do
    (_, size, _) <- get
    return size

-- | Get only the current RNG from a Machine.
getRng :: Machine StdGen
getRng = do
    (_, _, rng) <- get
    return rng

-- | Check that there are enough elements in the stack for the given function,
-- throw ArityException otherwise.
guardArity :: Function -> Machine ()
guardArity f = do
    size <- getSize
    if size >= arity f
    then return ()
    else throwError $ ArityException f (arity f)

-- | Take the top element of the stack and return it.
pop :: Machine Integer
pop = do
    (stack, size, rng) <- get
    case stack of []    -> error "[FATAL] Tried to pop from empty stack!"
                  (a:s) -> put (s, size-1, rng) >> return a

-- | Convenience/optimization for popping two values at once, since there are
-- so many functions with arity 2.
pop2 :: Machine (Integer, Integer)
pop2 = do
    (stack, size, rng) <- get
    case stack of []      -> error "[FATAL] Tried to pop 2 from empty stack!"
                  [_]     -> error "[FATAL] Tried to pop 2 from stack of one!"
                  -- The stack is conceptually left-to-right, but physically
                  -- runs right-to-left so the top of the stack is efficently
                  -- accessible. Therefore we must reverse the order of a and b
                  -- so that non-commutative functions like Div receive their
                  -- arguments in the correct order.
                  (b:a:s) -> put (s, size-2, rng) >> return (a, b)

-- | Look at top element of stack and return it if it exists. Doesn't modify
-- stack.
peek :: Machine (Maybe Integer)
peek = do
    stack <- getStack
    case stack of []    -> return Nothing
                  (x:_) -> return $ Just x

-- | Add an integer to the top of the stack.
push :: Integer -> Machine ()
push x = do
    (stack, size, rng) <- get
    put (x:stack, size+1, rng)

-- | Add a list of integers to the top of the stack. They are pushed in order,
-- meaning that they will be popped in reverse order.
pushMany :: [Integer] -> Machine ()
pushMany = foldr ((>>) . push) $ return ()

-- | Execute a function. Return True if we should keep executing, False
-- otherwise. Does not guard arity (executeFunctionWithArityCheck does this).
executeFunction :: Function -> Machine Bool

executeFunction Add = do
    (a, b) <- pop2
    push $ a + b
    return True

executeFunction Sub = do
    (a, b) <- pop2
    push $ a - b
    return True

executeFunction Mul = do
    (a, b) <- pop2
    push $ a * b
    return True

executeFunction Div = do
    (a, b) <- pop2
    push $ a `div` b
    return True

executeFunction Neg = do
    a <- pop
    push $ negate a
    return True

executeFunction Mod = do
    (a, b) <- pop2
    push $ a `mod` b
    return True

executeFunction Exp = do
    (base, pow) <- pop2
    let args = [("base", base), ("pow", pow)]
    if pow < 0
    then throwError $ DomainException Exp args "pow >= 0"
    else do
        -- Make a list of pow copies of base, then push the product of that list
        push . product $ genericReplicate pow base
        return True

executeFunction Fact = do
    n <- pop
    let args = [("n", n)]
    if n < 0
    then throwError $ DomainException Fact args "n >= 0"
    else do
        push $ product [1 .. n]
        return True

executeFunction Perm = do
    (n, k) <- pop2
    let args = [("n", n), ("k", k)]
    if 0 > k || k > n || n <= 0
    then throwError $ DomainException Perm args "0 <= k <= n and n > 0"
    else do
        -- This cancels out the (n-k)! from n!
        push $ product [(n-k+1) .. n]
        return True

executeFunction Comb = do
    (n, k) <- pop2
    let args = [("n", n), ("k", k)]
    if 0 > k || k > n || n <= 0
    then throwError $ DomainException Comb args "0 <= k <= n and n > 0"
    else do
        -- Sort k and n-k to best optimize the factorials
        let (lo, hi)  = if k < n-k then (k, n-k)
                                   else (n-k, k)
        -- Cancel out hi! from n!
        -- We have to use `div` here but the result has to be an integer anyway
        push $ product [(hi+1) .. n] `div` product [1 .. lo]
        return True

executeFunction Dice = do
    (num, sides) <- pop2
    let args = [("num", num), ("sides", sides)]
    if num <= 0 || sides <= 0
    then throwError $ DomainException Dice args "num > 0 and sides > 0"
    else do
        (stack, size, rng) <- get
        -- Split the RNG in two
        let (ourRng, retRng) = split rng
        -- Use one half to generate the random numbers
        let results = genericTake num $ randomRs (1, sides) ourRng :: [Integer]
        -- Return the other half to the machine state, while also pushing the
        -- sum to the stack
        put (sum results : stack, size+1, retRng)
        return True

executeFunction Pop = pop >> return True

executeFunction Cls = do
    rng <- getRng
    put ([], 0, rng)
    return True

executeFunction Exchg = do
    (a, b) <- pop2
    push a
    push b
    return True

executeFunction Copy = do
    a <- pop
    push a
    push a
    return True

executeFunction Dupe = do
    (a, b) <- pop2
    pushMany $ genericReplicate b a
    return True

executeFunction Print = do
    a <- peek
    case a of Nothing -> tell ["[WARNING] print with empty stack"]
              Just x  -> tell [T.pack $ show x]
    return True

executeFunction Stack = do
    stack <- getStack
    tell [ T.pack $ "[" ++ (unwords . map show . reverse $ stack) ++ "]" ]
    return True

executeFunction Help = tell [help] >> return True
    where help = [r|Type an integer to push it onto the stack
All operators are case-insensitive

Arithmetic:
  n  neg   [... a]   -> [... (-a)]     (additive inverse)
  +  add   [... a b] -> [... (a+b)]    (addition)
  -  sub   [... a b] -> [... (a-b)]    (subtraction)
  *  mul   [... a b] -> [... (a*b)]    (multiplication)
  /  div   [... a b] -> [... (a/b)]    (int division)
  %  mod   [... a b] -> [... (a%b)]    (int modulo)
  ^  exp   [... a b] -> [... (a^b)]    (integer exponent)

Functions:
  !  fact  [... a]   -> [... (a!)]     (factorial)
     perm  [... n k] -> [... (nPk)]    (k-permutations of n)
     comb  [... n k] -> [... (nCk)]    (k-combinations of n)
  d  dice  [... a b] -> [... (aDb)]    (roll b a-sided dice and push sum)

Stack manipulation:
  k  pop   [... a]   -> [...]          (discards top of stack)
  c  cls   [...]     -> []             (empties stack)
  x  exchg [... a b] -> [... b a]      (exchanges top two values)
  y  copy  [... a]   -> [... a a]      (makes two copies of a)
  r  dupe  [... a b] -> [... a a a...] (makes b copies of a)

Output:
  p  print       prints the value on top of the stack (without modifying)
  s  stack       prints the entire stack (left-to-right is bottom-to-top)

'?' or 'help' to review this help message
'q' or 'quit' to quit|]

-- Returning False signals to the main loop that we want to quit
executeFunction Quit = return False

-- | Execute a function, but check its arity before doing so.
executeFunctionWithArityCheck :: Function -> Machine Bool
executeFunctionWithArityCheck f = guardArity f >> executeFunction f

-- | Execute an operation.
executeOp :: Operation -> Machine Bool
executeOp (Push x) = push x >> return True
executeOp (Exec f) = executeFunctionWithArityCheck f

-- | Parse an operation, or throw ReadException if it doesn't exist.
parse :: T.Text -> Machine Operation
parse s = case maybeParseOperation s of
            Just op -> return op
            Nothing -> throwError $ ReadException s

-- | Parse an operation, then execute it.
parseAndExecute :: T.Text -> Machine Bool
parseAndExecute s = parse s >>= executeOp

-- | Machine exception handler. Prints the exception description, and restores
-- the previous state (from argument).
exceptionHandler :: (Stack, Int, StdGen) -> Exception -> Machine Bool
exceptionHandler rb ex = do
    tell [describeException ex]
    put rb
    tell ["Rolled back"]
    return True

-- | Parse an entire query, being sure to handle rollbacks, and to actually
-- stop when the user quits.
parseAndExecuteQuery :: T.Text -> Machine Bool
parseAndExecuteQuery q = let ops = T.words q in do
    rollback <- get
    let doCycle False _ = return False
        doCycle True  s = parseAndExecute s
    let try = do
                 running <- foldM doCycle True ops
                 tell ["Done"]
                 return running
    try `catchError` exceptionHandler rollback
