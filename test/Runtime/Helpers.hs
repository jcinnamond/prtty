module Runtime.Helpers where

import Data.Vector qualified as V
import Runtime.Internal.Types (Environment (..), emptyStyle)

testEnv :: Environment
testEnv =
    Environment
        { instructions = V.empty
        , pc = 0
        , backMarkers = []
        , lastJump = Nothing
        , height = 25
        , width = 80
        , topMargin = 0
        , leftMargin = 0
        , currentStyle = emptyStyle
        , styleHistory = []
        }