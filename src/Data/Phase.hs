module Data.Phase
    ( Parsed (..)
    , Evaluated (..)
    )
where

newtype Parsed = Parsed Parsed 

newtype Evaluated = Evaluated Evaluated 