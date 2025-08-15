module Document.Obj
    ( BlockClass (..)
    , Block (..)
    , Obj (..)
    )
where

import Data.Text (Text)

data BlockClass
    = BcParagraph
    | BcFooter
    | BcCode -- TODO(meowesque): Language?
    | BcUnnamed
    deriving (Show)

data Block = Block
    { class_ :: !BlockClass
    , content :: [Obj] 
    }
    deriving (Show)

data Obj
    = OTitle Text
    | OBlock Block
    | OText Text
    deriving (Show)