{-# LANGUAGE OverloadedStrings #-}

module Functions
( Function (..)
, fullName
, shortName
, arity
, maybeParseFunction
) where

import qualified Data.Map.Strict as MS
import qualified Data.Text as T

-- | Functions that operate on the stack.
data Function = Add 
              | Sub 
              | Mul 
              | Div 
              | Neg
              | Mod 
              | Exp 
              | Fact
              | Perm
              | Comb
              | Dice
              | Pop
              | Cls
              | Exchg
              | Copy
              | Dupe
              | Print
              | Stack
              | Help
              | Quit 
    deriving (Enum, Bounded)

-- | Full name of a function.
fullName :: Function -> T.Text
fullName Add   = T.toCaseFold "add"
fullName Sub   = T.toCaseFold "sub"
fullName Mul   = T.toCaseFold "mul"
fullName Div   = T.toCaseFold "div"
fullName Neg   = T.toCaseFold "neg"
fullName Mod   = T.toCaseFold "mod"
fullName Exp   = T.toCaseFold "exp"
fullName Fact  = T.toCaseFold "fact"
fullName Perm  = T.toCaseFold "perm"
fullName Comb  = T.toCaseFold "comb"
fullName Dice  = T.toCaseFold "dice"
fullName Pop   = T.toCaseFold "pop"
fullName Cls   = T.toCaseFold "cls"
fullName Exchg = T.toCaseFold "exchg"
fullName Copy  = T.toCaseFold "copy"
fullName Dupe  = T.toCaseFold "dupe"
fullName Print = T.toCaseFold "print"
fullName Stack = T.toCaseFold "stack"
fullName Help  = T.toCaseFold "help"
fullName Quit  = T.toCaseFold "quit"

-- | Short name of a function. Not every function has a short name.
shortName :: Function -> Maybe T.Text
shortName Add   = Just . T.toCaseFold $ "+"
shortName Sub   = Just . T.toCaseFold $ "-"
shortName Mul   = Just . T.toCaseFold $ "*"
shortName Div   = Just . T.toCaseFold $ "/"
shortName Neg   = Just . T.toCaseFold $ "n"
shortName Mod   = Just . T.toCaseFold $ "%"
shortName Exp   = Just . T.toCaseFold $ "^"
shortName Fact  = Just . T.toCaseFold $ "!"
shortName Perm  = Nothing
shortName Comb  = Nothing
shortName Dice  = Just . T.toCaseFold $ "d"
shortName Pop   = Just . T.toCaseFold $ "k"
shortName Cls   = Just . T.toCaseFold $ "c"
shortName Exchg = Just . T.toCaseFold $ "x"
shortName Copy  = Just . T.toCaseFold $ "y"
shortName Dupe  = Just . T.toCaseFold $ "r"
shortName Print = Just . T.toCaseFold $ "p"
shortName Stack = Just . T.toCaseFold $ "s"
shortName Help  = Just . T.toCaseFold $ "?"
shortName Quit  = Just . T.toCaseFold $ "q"

-- | Arity of a function.
arity :: Function -> Int
arity Add   = 2
arity Sub   = 2
arity Mul   = 2
arity Div   = 2
arity Neg   = 1
arity Mod   = 2
arity Exp   = 2
arity Fact  = 1
arity Perm  = 2
arity Comb  = 2
arity Dice  = 2
arity Pop   = 1
arity Cls   = 0
arity Exchg = 2
arity Copy  = 1
arity Dupe  = 2
arity Print = 0
arity Stack = 0
arity Help  = 0
arity Quit  = 0

-- | A map of function names (full or short) to functions.
funcParseMap :: MS.Map T.Text Function
funcParseMap = MS.fromList $ do
    f <- [minBound..maxBound] -- f :: Function
    case shortName f of Just sh -> [ (fullName f, f), (sh, f) ]
                        Nothing -> [ (fullName f, f)          ]

-- | Attempt to parse a function name to a function. Returns Nothing on failure.
maybeParseFunction :: T.Text -> Maybe Function
maybeParseFunction = (funcParseMap MS.!?) . T.toCaseFold
