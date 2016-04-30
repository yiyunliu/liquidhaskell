module spec Data.List where

import GHC.Base
import GHC.List
import GHC.Types

groupBy
    :: (a -> a -> GHC.Types.Bool)
    -> [a]
    -> [{v:[a] | len(v) > 0}]

transpose
    :: [[a]]
    -> [{v:[a] | (len v) > 0}]
