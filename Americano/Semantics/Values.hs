module Americano.Semantics.Values where

import qualified Data.Map as M

-- VNull ad VRef are internal values.
data Val
  = VNull
  | VInt    { vInt    :: Integer           }
  | VBool   { vBool   :: Bool              }
  | VString { vString :: String            }
  | VArray  { vArray  :: M.Map Integer Loc }
  | VDict   { vDict   :: M.Map Val Loc     }
  | VRef    { vRef    :: Loc               }
  deriving (Show, Eq, Ord)

type Loc = Int
