{-# LANGUAGE OverloadedStrings #-}

module Exceptions
( Exception (..)
, describeException
) where

import Data.Monoid

import qualified Data.Text as T

import Operations

-- | Machine exceptions.
data Exception = ReadException   T.Text
               | ArityException  Function Int
               | DomainException Function [(T.Text, Integer)] T.Text 

-- | Show the argument list for DomainException.
showArgs :: [(T.Text, Integer)] -> T.Text
showArgs = T.intercalate " " . map showArg
    where showArg (name, v) = "(" <> name <> ": " <> T.pack (show v) <> ")"

-- | Describe an exception.
describeException :: Exception -> T.Text
describeException ex = case ex of
    ReadException x              -> "Unrecognized instruction '"
                                      <> x
                                      <> "'"
    ArityException f n          -> "Not enough values on stack to satisfy '"
                                      <> fullName f
                                      <> "' with arity "
                                      <> T.pack (show n)
    DomainException f args desc -> "Domain error in '"
                                      <> fullName f
                                      <> "' with arguments "
                                      <> showArgs args
                                      <> ": requires "
                                      <> desc
