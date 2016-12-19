module J ( module J
         , module J.Store
         ) where

import J.Store

data J = J { key :: String
           , dst :: String
           }
