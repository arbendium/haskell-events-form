{-# LANGUAGE OverloadedStrings #-}
module Util (replaceVariables) where

import Data.Text (Text)
import qualified Data.Text as T

data ReplaceState = Plain | Backslash | Variable Text

replaceVariables :: (Text -> Text) -> Text -> Text
replaceVariables replace text = fst $ T.foldl folder ("", Plain) text
    where
        folder :: (Text, ReplaceState) -> Char -> (Text, ReplaceState)
        folder (out, Plain) '\\' = (out, Backslash)
        folder (out, Plain) '$' = (out, Variable "")
        folder (out, Plain) a = (T.snoc out a, Plain)
        folder (out, Backslash) '$' = (T.snoc out '$', Plain)
        folder (out, Backslash) '\\' = (T.snoc out '\\', Plain)
        folder (out, Backslash) a = (T.snoc (T.snoc out '\\') a, Plain)
        folder (out, Variable v) '$' = (out <> replace v, Plain)
        folder (out, Variable v) a = (out, Variable $ T.snoc v a)
