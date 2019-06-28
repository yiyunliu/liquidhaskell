module Gradual.Types where


import qualified Language.Haskell.Liquid.UX.Config as C
import Language.Fixpoint.Types
import Language.Haskell.Liquid.Types.LHSymbol
import qualified Data.HashMap.Strict as M

data GConfig = GConfig {gtarget :: String, depth :: Int, pId :: Int, pNumber :: Int}

defConfig :: GConfig
defConfig = GConfig "" 0 0 0 

setPId :: GConfig -> Int -> GConfig
setPId cfg i = cfg {pId = i}

makeGConfig :: C.Config -> GConfig
makeGConfig cfg = defConfig {depth = C.gdepth cfg, gtarget = head $ C.files cfg}



type GSub a = M.HashMap (KVar LHSymbol) (a, Expr LHSymbol)
type GMap a = M.HashMap (KVar LHSymbol) (a, [Expr LHSymbol])
type GSpan  = M.HashMap (KVar LHSymbol) [(KVar LHSymbol, Maybe SrcSpan)] 

toGMap :: [(KVar LHSymbol, (a, [Expr LHSymbol]))] -> GMap a
toGMap = M.fromList 

fromGMap :: GMap a -> [(KVar LHSymbol, (a, [Expr LHSymbol]))]
fromGMap = M.toList


fromGSub :: GSub a -> [(KVar LHSymbol, (a, Expr LHSymbol))]
fromGSub = M.toList


removeInfo :: GMap a -> GMap ()
removeInfo = M.map (\(_,x) -> ((),x))
