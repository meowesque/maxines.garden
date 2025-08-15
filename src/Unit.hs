module Unit
    ( UnitParsed (..)
    , Unit
    )
where

import Data.Source (Source)
import Syntax.Obj (Obj)
import Data.Phase (Parsed)

data UnitParsed = UnitParsed 
    { source :: Source 
    , objs :: ![Obj] -- TODO(meowesque): Stricter?
    }

type family Unit a :: * where
    Unit Parsed = UnitParsed 