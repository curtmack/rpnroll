{-# LANGUAGE OverloadedStrings #-}

module Operations
( Operation (..)
, Function (..) -- re-export for convenience
, fullName      -- re-export for convenience
, arity         -- re-export for convenience
, describeOp
, maybeParseOperation
) where

import Control.Applicative

import qualified Data.Text as T

import Functions

data Operation = Push Integer
               | Exec Function

-- | Canonical form of an operation.
describeOp :: Operation -> T.Text
describeOp (Push x) = T.pack (show x)
describeOp (Exec f) = fullName f

-- | Attempt to parse a value from a Text value. Return Nothing on failure.
readMaybe :: Read a => T.Text -> Maybe a
readMaybe s = case reads (T.unpack s) of 
                [(x, "")] -> Just x
                _         -> Nothing

-- | Attempt to parse an integer and return a Push operation.
parsePush :: T.Text -> Maybe Operation
parsePush s = Push <$> readMaybe s

-- | Attempt to parse a function and return an Exec operation.
parseExec :: T.Text -> Maybe Operation
parseExec s = Exec <$> maybeParseFunction s

-- | Attempt to parse an operation. Returns Nothing on failure.
maybeParseOperation :: T.Text -> Maybe Operation
maybeParseOperation s = parsePush s <|> parseExec s
