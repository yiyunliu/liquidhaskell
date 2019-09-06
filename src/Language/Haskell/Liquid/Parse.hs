{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DeriveDataTypeable        #-}

-- YL : this file is mostly commented out for now. It would only make sense to refactor here
-- when the Types are correctly parameterized

module Language.Haskell.Liquid.Parse
  ( hsSpecificationP
  , specSpecificationP
  , singleSpecP
  , BPspec
  , Pspec(..)
  , parseSymbolToLogic
  )
  where

import           Control.Arrow                          (second)
import           Control.Monad
import           Data.String
import           Data.Void
import           Prelude                                hiding (error)
import           Text.Parsec
import           Text.Parsec.Error                      (newErrorMessage, Message (..))
import           Text.Parsec.Pos

-- import           Text.Parsec.Char                       (newline) 
import qualified Text.Parsec.Token                      as Token
import qualified Data.Text                              as T
import qualified Data.HashMap.Strict                    as M
import qualified Data.HashSet                           as S
-- import           Data.Monoid
import           Data.Data
import qualified Data.Maybe                             as Mb -- (isNothing, fromMaybe)
import           Data.Char                              (isSpace, isAlpha, isUpper, isAlphaNum, isDigit)
import           Data.List                              (foldl', partition)
import           GHC                                    (ModuleName, mkModuleName)
import qualified Text.PrettyPrint.HughesPJ              as PJ 
import           Text.PrettyPrint.HughesPJ.Compat       ((<+>)) 
import           Language.Fixpoint.Types                hiding (panic, SVar, DDecl, DataDecl, DataCtor (..), Error, R, Predicate)
import           Language.Haskell.Liquid.GHC.Misc
import           Language.Haskell.Liquid.Types
import           Language.Haskell.Liquid.Types.LHSymbol
import qualified Language.Fixpoint.Misc                 as Misc      
import qualified Language.Haskell.Liquid.Misc           as Misc
import qualified Language.Haskell.Liquid.Measure        as Measure
import           Language.Fixpoint.Parse                hiding (stringLiteral, dataDeclP, angles, refBindP, refP, refDefP)

import Control.Monad.State

-- import Debug.Trace

--------------------------------------------------------------------------------
-- | Top Level Parsing API -----------------------------------------------------
--------------------------------------------------------------------------------

-- | Used to parse .hs and .lhs files (via ApiAnnotations)

-------------------------------------------------------------------------------
hsSpecificationP :: ModuleName
                 -> [(SourcePos, String)]
                 -> [BPspec]
                 -> Either [Error] (ModName, Measure.BareSpec)
-------------------------------------------------------------------------------
hsSpecificationP modName specComments specQuotes =
  case go ([], []) initPStateWithList $ reverse specComments of
    ([], specs) ->
      Right $ mkSpec (ModName SrcImport modName) (specs ++ specQuotes)
    (errs, _) ->
      Left errs
  where
    go (errs, specs) _ []
      = (reverse errs, reverse specs)
    go (errs, specs) pstate ((pos, specComment):xs)
      = case parseWithError pstate specP pos specComment of
          Left err        -> go (err:errs, specs) pstate xs
          Right (st,spec) -> go (errs,spec:specs) st xs

-- | Used to parse .spec files

initPStateWithList :: PState Void
initPStateWithList
-- YL : use GHC tyswiredin? i'm worried that it might not work due to the wired in defined in LH
  = initPState { empList  = Just (EVar $ FS $ ("GHC.Types.[]" :: FixSymbol))
               , singList = Just (\e -> EApp (EApp (EVar $ FS $ ("GHC.Types.:"  :: FixSymbol)) e) (EVar $ FS $ ("GHC.Types.[]" :: FixSymbol)))
               }

--------------------------------------------------------------------------
specSpecificationP  :: SourceName -> String -> Either Error (ModName, Measure.BareSpec)
--------------------------------------------------------------------------
specSpecificationP f s = mapRight snd $  parseWithError initPStateWithList specificationP (newPos f 1 1) s

specificationP :: Parser Void (ModName, Measure.BareSpec)
specificationP = do 
  reserved "module"
  reserved "spec"
  -- YL : liquid-fixpoint: should return FixSymbol
  FS name   <- symbolP
  reserved "where"
  xs     <- if True then grabs (specP <* whiteSpace) else sepBy specP newline
  return $ mkSpec (ModName SpecImport $ mkModuleName $ symbolString name) xs

-- debugP = grabs (specP <* whiteSpace)

-------------------------------------------------------------------------------
singleSpecP :: SourcePos -> String -> Either Error BPspec
-------------------------------------------------------------------------------
singleSpecP pos = mapRight snd . parseWithError initPStateWithList specP pos

mapRight :: (a -> b) -> Either l a -> Either l b
mapRight f (Right x) = Right $ f x
mapRight _ (Left x)  = Left x

---------------------------------------------------------------------------
parseWithError :: PState Void -> Parser Void a -> SourcePos -> String -> Either Error (PState Void, a)
---------------------------------------------------------------------------
parseWithError pstate parser p s =
  case runState (runParserT doParse 0 (sourceName p) s) pstate of
    (Left e, _)            -> Left  $ parseErrorError e
    (Right (r, "", _), st) -> Right (st, r)
    (Right (_, rem, _), _) -> Left  $ parseErrorError $ remParseError p s rem
  where
    -- See http://stackoverflow.com/questions/16209278/parsec-consume-all-input
    doParse = setPosition p >> remainderP (whiteSpace *> parser <* (whiteSpace >> eof))


---------------------------------------------------------------------------
parseErrorError     :: ParseError -> Error
---------------------------------------------------------------------------
parseErrorError e = ErrParse sp msg e
  where
    pos             = errorPos e
    sp              = sourcePosSrcSpan pos
    msg             = "Error Parsing Specification from:" <+> PJ.text (sourceName pos)

---------------------------------------------------------------------------
remParseError       :: SourcePos -> String -> String -> ParseError
---------------------------------------------------------------------------
remParseError p s r = newErrorMessage msg $ newPos (sourceName p) line col
  where
    msg             = Message "Leftover while parsing"
    (line, col)     = remLineCol p s r

remLineCol             :: SourcePos -> String -> String -> (Int, Int)
remLineCol pos src rem = (line + offLine, col + offCol)
  where
    line               = 1 + srcLine - remLine
    srcLine            = length srcLines
    remLine            = length remLines
    offLine            = sourceLine pos - 1
    col                = 1 + srcCol - remCol
    srcCol             = length $ srcLines !! (line - 1)
    remCol             = length $ head remLines
    offCol             = if line == 1 then sourceColumn pos - 1 else 0
    srcLines           = lines  src
    remLines           = lines  rem



--------------------------------------------------------------------------------
-- Parse to Logic  -------------------------------------------------------------
--------------------------------------------------------------------------------

parseSymbolToLogic :: SourceName -> String -> Either Error LogicMap
parseSymbolToLogic f = mapRight snd . parseWithError initPStateWithList toLogicP (newPos f 1 1)

toLogicP :: Parser Void LogicMap
toLogicP
  -- YL : this is getting messy. Expr Void -> Expr s. huge performance cost?
  = undefined -- toLogicMap <$> many toLogicOneP

toLogicOneP :: Parser Void  (Located FixSymbol, [FixSymbol], Expr Void)
toLogicOneP
  = do reserved "define"
       (x:xs) <- many1 (locParserP symbolP)
       reservedOp "="
       e      <- exprP
       -- YL : address the isomorphism mapping
       undefined -- return (x, val <$> xs, e)


defineP :: Parser Void (Located FixSymbol, FixSymbol)
defineP = do v <- locParserP binderP
             spaces
             reservedOp "="
             spaces
             x <- binderP
             return (v, x)

--------------------------------------------------------------------------------
-- Lexer Tokens ----------------------------------------------------------------
--------------------------------------------------------------------------------

dot :: Parser Void String
dot = Token.dot lexer

angles :: Parser Void a -> Parser Void a
angles = Token.angles lexer

stringLiteral :: Parser Void String
stringLiteral = Token.stringLiteral lexer

--------------------------------------------------------------------------------
-- | BareTypes -----------------------------------------------------------------
--------------------------------------------------------------------------------

{- | [NOTE:BARETYPE-PARSE] Fundamentally, a type is of the form

      comp -> comp -> ... -> comp

So

  bt = comp
     | comp '->' bt

  comp = circle
       | '(' bt ')'

  circle = the ground component of a baretype, sans parens or "->" at the top level

Each 'comp' should have a variable to refer to it,
either a parser-assigned one or given explicitly. e.g.

  xs : [Int]

-}

data ParamComp = PC { _pci :: PcScope
                    , _pct :: BareType }
                    deriving (Show)

data PcScope = PcImplicit FixSymbol
             | PcExplicit FixSymbol
             | PcNoSymbol
             deriving (Eq,Show)

nullPC :: BareType -> ParamComp
nullPC bt = PC PcNoSymbol bt

btP :: Parser Void ParamComp
btP = do
  c@(PC sb _) <- compP
  case sb of
    PcNoSymbol   -> return c
    -- YL : fix
    -- PcImplicit b -> parseFun c b
    -- PcExplicit b -> parseFun c b
  <?> "btP"
  where
    parseFun c@(PC sb t1) b  =
      ((do
            reservedOp "->"
            PC _ t2 <- btP
            return (PC sb (rFun b t1 t2)))
        <|>
         (do
            reservedOp "~>"
            PC _ t2 <- btP
            return (PC sb (rImpF b t1 t2)))
        <|>
         (do
            reservedOp "=>"
            PC _ t2 <- btP
            -- TODO:AZ return an error if s == PcExplicit
            return $ PC sb $ foldr (rFun (AS . LHRefSym $ dummySymbol)) t2 (getClasses t1))
         <|> return c)


compP :: Parser Void ParamComp
compP = circleP <* whiteSpace <|> parens btP <?> "compP"

circleP :: Parser Void ParamComp
circleP
  =  nullPC <$> (reserved "forall" >> bareAllP)
 <|> holePC                                 -- starts with '_'
 <|> namedCircleP                           -- starts with lower
 <|> bareTypeBracesP                        -- starts with '{'
 <|> unnamedCircleP
 <|> anglesCircleP                          -- starts with '<'
 <|> nullPC <$> (dummyP (bbaseP <* spaces)) -- starts with '_' or '[' or '(' or lower or "'" or upper
 <?> "circleP"

anglesCircleP :: Parser Void ParamComp
anglesCircleP
  = angles $ do
      PC sb t <- parens btP
      p       <- monoPredicateP
      return   $ PC sb (t `strengthen` MkUReft mempty p mempty)

holePC :: Parser Void ParamComp
holePC = do
  h <- holeP
  b <- dummyBindP
  return (PC (PcImplicit b) h)

namedCircleP :: Parser Void ParamComp
namedCircleP = do
  lb <- locParserP lowerIdP
  (do _ <- colon
      let (FS b) = val lb
      -- YL : Probably should define some sort of isomorphism between Symbol Void and FixSymbol
      -- or at least FixSymbolic instance for Symbol Void
      PC (PcExplicit b) <$> bareArgP b
    <|> do
      b <- dummyBindP
      -- YL : Fix
      undefined
      -- PC (PcImplicit b) <$> dummyP (lowerIdTail (val lb))
    )

unnamedCircleP :: Parser Void ParamComp
unnamedCircleP = do
  lb <- locParserP dummyBindP
  let b = val lb
  t1 <- bareArgP b
  return $ PC (PcImplicit b) t1

-- ---------------------------------------------------------------------

-- | The top-level parser for "bare" refinement types. If refinements are
-- not supplied, then the default "top" refinement is used.

bareTypeP :: Parser Void BareType
bareTypeP = do
  PC _ v <- btP
  return v

bareTypeBracesP :: Parser Void ParamComp
bareTypeBracesP = undefined -- do
  -- t <-  try (braces (
  --           (try (do
  --              ct <- constraintP
  --              return $ Right ct
  --                    ))
  --          <|>
  --           (do
  --                   x  <- symbolP
  --                   _ <- colon
  --                   -- NOSUBST i  <- freshIntP
  --                   t  <- bbaseP
  --                   reservedOp "|"
  --                   ra <- refasHoleP <* spaces
  --                   -- xi is a unique var based on the name in x.
  --                   -- su replaces any use of x in the balance of the expression with the unique val
  --                   -- NOSUBST let xi = intSymbol x i
  --                   -- NOSUBST let su v = if v == x then xi else v
  --                   return $ Left $ PC (PcExplicit x) $ t (Reft (x, ra)) )
  --           )) <|> try (helper holeOrPredsP) <|> helper predP
  -- case t of
  --   Left l -> return l
  --   Right ct -> do
  --     PC _sb tt <- btP
  --     return $ nullPC $ rrTy ct tt
  -- where
  --   holeOrPredsP
  --     = (reserved "_" >> return hole)
  --    <|> try (pAnd <$> brackets (sepBy predP semi))
  --   helper p = braces $ do
  --     t <- ((RHole . uTop . Reft . ("VV",)) <$> (p <* spaces))
  --     return (Left $ nullPC t)


bareArgP :: FixSymbol -> Parser Void  BareType
bareArgP vvv
  =  refDefP vvv refasHoleP bbaseP    -- starts with '{'
 <|> holeP                            -- starts with '_'
 <|> (dummyP (bbaseP <* spaces))
 <|> parens bareTypeP                 -- starts with '('
                                      -- starts with '_', '[', '(', lower, upper
 <?> "bareArgP"

bareAtomP :: (Parser Void (Expr Void) -> Parser Void (Reft Void -> BareType) -> Parser Void BareType)
          -> Parser Void BareType
bareAtomP ref
  =  ref refasHoleP bbaseP
 <|> holeP
 <|> (dummyP (bbaseP <* spaces))
 <?> "bareAtomP"

bareAtomBindP :: Parser Void BareType
bareAtomBindP = bareAtomP refBindBindP


-- Either
--  { x : t | ra }
-- or
--  { ra }
refBindBindP :: Parser Void (Expr Void)
             -> Parser Void (Reft Void -> BareType)
             -> Parser Void BareType
refBindBindP rp kindP'
  = undefined -- braces (
   --    ((do
   --            x  <- symbolP
   --            _ <- colon
   --            -- NOSUBST i  <- freshIntP
   --            t  <- kindP'
   --            reservedOp "|"
   --            ra <- rp <* spaces
   --            -- xi is a unique var based on the name in x.
   --            -- su replaces any use of x in the balance of the expression with the unique val
   --            -- NOSUBST let xi = intSymbol x i
   --            -- NOSUBST let su v = if v == x then xi else v
   --            return $ {- substa su $ NOSUBST -} t (Reft (x, ra)) ))
   --   <|> ((RHole . uTop . Reft . ("VV",)) <$> (rp <* spaces))
   --   <?> "refBindBindP"
   -- )


refDefP :: FixSymbol
        -> Parser Void (Expr Void)
        -> Parser Void (Reft Void -> BareType)
        -> Parser Void BareType
refDefP vv rp kindP' = undefined -- braces $ do
  -- x       <- optBindP vv
  -- -- NOSUBST i       <- freshIntP
  -- t       <- try (kindP' <* reservedOp "|") <|> return (RHole . uTop) <?> "refDefP"
  -- ra      <- (rp <* spaces)
  -- -- xi is a unique var based on the name in x.
  -- -- su replaces any use of x in the balance of the expression with the unique val
  -- -- NOSUBST let xi   = intSymbol x i
  -- -- NOSUBST let su v = if v == x then xi else v
  -- return   $ {- substa su $ NOSUBST -} t (Reft (x, ra))
  --      -- substa su . t . Reft . (x,) <$> (rp <* spaces))
  --     --  <|> ((RHole . uTop . Reft . ("VV",)) <$> (rp <* spaces))

refP :: Parser Void (Reft Void -> BareType) -> Parser Void BareType
refP = refBindBindP refaP

-- "sym :" or return the devault sym
optBindP :: FixSymbol -> Parser Void FixSymbol
optBindP x = undefined -- try bindP <|> return x

holeP :: Parser Void BareType
holeP    = undefined -- reserved "_" >> spaces >> return (RHole $ uTop $ Reft ("VV", hole))

holeRefP :: Parser Void (Reft Void -> BareType)
holeRefP =  undefined -- reserved "_" >> spaces >> return (RHole . uTop)

-- NOPROP refasHoleP :: Parser Void Expr
-- NOPROP refasHoleP  = try refaP
-- NOPROP          <|> (reserved "_" >> return hole)

refasHoleP :: Parser Void (Expr Void)
refasHoleP
  = undefined -- (reserved "_" >> return hole)
 -- <|> refaP
 -- <?> "refasHoleP"

-- FIXME: the use of `blanks = oneOf " \t"` here is a terrible and fragile hack
-- to avoid parsing:
--
--   foo :: a -> b
--   bar :: a
--
-- as `foo :: a -> b bar`..
bbaseP :: Parser Void (Reft Void -> BareType)
bbaseP
  =  undefined -- holeRefP  -- Starts with '_'
 -- <|> liftM2 bLst (brackets (maybeP bareTypeP)) predicatesP
 -- <|> liftM2 bTup (parens $ sepBy (maybeBind bareTypeP) comma) predicatesP
 -- <|> try parseHelper  -- starts with lower
 -- <|> liftM5 bCon bTyConP stratumP predicatesP (sepBy bareTyArgP blanks) mmonoPredicateP
 --           -- starts with "'" or upper case char
 -- <?> "bbaseP"
 -- where
 --   parseHelper = do
 --     l <- lowerIdP
 --     lowerIdTail l

maybeBind :: Parser Void a -> Parser Void (Maybe FixSymbol, a)
maybeBind p = do {bd <- maybeP' bbindP; ty <- p ; return (bd, ty)}
  where
    maybeP' p = try (Just <$> p)
             <|> return Nothing

lowerIdTail :: FixSymbol -> Parser Void (Reft Void -> BareType)
lowerIdTail l = undefined
     -- (    (liftM2 bAppTy (return $ bTyVar l) (sepBy1 bareTyArgP blanks))
     --  <|> (liftM3 bRVar  (return $ bTyVar l) stratumP monoPredicateP))

bTyConP :: Parser Void BTyCon
bTyConP
  = undefined -- (reservedOp "'" >> (mkPromotedBTyCon <$> locUpperIdP))
 -- <|> mkBTyCon <$> locUpperIdP
 -- <?> "bTyConP"

mkPromotedBTyCon :: Located FixSymbol -> BTyCon
mkPromotedBTyCon x = BTyCon x False True -- (consSym '\'' <$> x) False True

classBTyConP :: Parser Void BTyCon
classBTyConP = undefined -- mkClassBTyCon <$> locUpperIdP

mkClassBTyCon :: Located FixSymbol -> BTyCon
mkClassBTyCon x = BTyCon x True False

stratumP :: Parser Void Strata
stratumP
  = do reservedOp "^"
       bstratumP
 <|> return mempty
 <?> "stratumP"

bstratumP :: Parser Void [Stratum]
bstratumP
  = undefined -- ((:[]) . SVar) <$> symbolP

bbaseNoAppP :: Parser Void (Reft Void -> BareType)
bbaseNoAppP
  = undefined --  holeRefP
 -- <|> liftM2 bLst (brackets (maybeP bareTypeP)) predicatesP
 -- <|> liftM2 bTup (parens $ sepBy (maybeBind bareTypeP) comma) predicatesP
 -- <|> try (liftM5 bCon bTyConP stratumP predicatesP (return []) (return mempty))
 -- <|> liftM3 bRVar (bTyVar <$> lowerIdP) stratumP monoPredicateP
 -- <?> "bbaseNoAppP"

maybeP :: ParsecT s u m a -> ParsecT s u m (Maybe a)
maybeP p = liftM Just p <|> return Nothing

bareTyArgP :: Parser Void BareType
bareTyArgP
  = undefined --  (RExprArg . fmap expr <$> locParserP integer)
 -- <|> try (braces $ RExprArg <$> locParserP exprP)
 -- <|> try bareAtomNoAppP
 -- <|> try (parens bareTypeP)
 -- <?> "bareTyArgP"

bareAtomNoAppP :: Parser Void BareType
bareAtomNoAppP
  =  refP bbaseNoAppP
 <|> (dummyP (bbaseNoAppP <* blanks))
 <?> "bareAtomNoAppP"


constraintP :: Parser Void BareType
constraintP
  = undefined -- do xts <- constraintEnvP
    --    t1  <- bareTypeP
    --    reservedOp "<:"
    --    t2  <- bareTypeP
    --    return $ fromRTypeRep $ RTypeRep [] [] []
    --                                     [] [] []
    --                                     ((val . fst <$> xts) ++ [dummySymbol])
    --                                     (replicate (length xts + 1) mempty)
    --                                     ((snd <$> xts) ++ [t1]) t2

constraintEnvP :: Parser Void [(Located FixSymbol, BareType)]
constraintEnvP
   =  try (do xts <- sepBy tyBindNoLocP comma
              reservedOp "|-"
              return xts)
  <|> return []
  <?> "constraintEnvP"

rrTy :: Monoid r => RType c tv r -> RType c tv r -> RType c tv r
rrTy ct = undefined --  RRTy (xts ++ [(dummySymbol, tr)]) mempty OCons
  -- where
  --   tr   = ty_res trep
  --   xts  = zip (ty_binds trep) (ty_args trep)
  --   trep = toRTypeRep ct

--  "forall <z w> . TYPE"
-- or
--  "forall x y <z :: Nat, w :: Int> . TYPE"
bareAllP :: Parser Void BareType
bareAllP = do
  as <- tyVarDefsP
  vs <- angles inAngles
        <|> (return $ Right [])
  dot
  t <- bareTypeP
  case vs of
    -- Left ss  -> return $ foldr RAllS t ss
    Right ps -> return $ foldr RAllT (foldr RAllP t ps) (makeRTVar <$> as)
  where
    inAngles =
      (
       (try  (Right <$> sepBy  predVarDefP comma))
        <|> ((Left  <$> sepBy1 symbolP     comma))
       )

tyVarDefsP :: Parser Void [BTyVar]
tyVarDefsP
  = undefined -- (parens $ many (bTyVar <$> tyKindVarIdP))
 -- <|> many (bTyVar <$> tyVarIdP)
 -- <?> "tyVarDefsP"

-- TODO:AZ use something from Token instead
tyVarIdP :: Parser Void FixSymbol
tyVarIdP = undefined -- FixSymbol <$> condIdP (lower <|> char '_') alphanums isNotReserved -- (isSmall . head)
  where
    alphanums = S.fromList $ ['a'..'z'] ++ ['0'..'9']

tyKindVarIdP :: Parser Void FixSymbol
tyKindVarIdP = do
   tv <- tyVarIdP
   (  (do reservedOp "::"; _ <- kindP; return tv)
    <|> return tv)

kindP :: Parser Void BareType
kindP = bareAtomBindP

predVarDefsP :: Parser Void [PVar BSort]
predVarDefsP
  =  (angles $ sepBy1 predVarDefP comma)
 <|> return []
 <?> "predVarDefP"

predVarDefP :: Parser Void (PVar BSort)
predVarDefP
  = bPVar <$> predVarIdP <*> dcolon <*> propositionSortP

predVarIdP :: Parser Void FixSymbol
predVarIdP
  = symbol <$> tyVarIdP

bPVar :: FixSymbol -> t -> [(FixSymbol, t1)] -> PVar t1
bPVar p _ xts  = PV p (PVProp τ) dummySymbol τxs
  where
    (_, τ) = safeLast "bPVar last" xts
    τxs    = undefined -- [ (τ, x, EVar x) | (x, τ) <- init xts ]
    safeLast _ xs@(_:_) = last xs
    safeLast msg _      = panic Nothing $ "safeLast with empty list " ++ msg

propositionSortP :: Parser Void [(FixSymbol, BSort)]
propositionSortP = map (Misc.mapSnd toRSort) <$> propositionTypeP

propositionTypeP :: Parser Void [(FixSymbol, BareType)]
propositionTypeP = either parserFail return =<< (mkPropositionType <$> bareTypeP)

mkPropositionType :: BareType -> Either String [(FixSymbol, BareType)]
mkPropositionType t
  | isOk      = undefined -- Right $ zip (ty_binds tRep) (ty_args tRep)
  | otherwise = Left err
  where
    isOk      = isPropBareType (ty_res tRep)
    tRep      = toRTypeRep t
    err       = "Proposition type with non-Bool output: " ++ showpp t

xyP :: Parser Void x -> Parser Void a -> Parser Void y -> Parser Void (x, y)
xyP lP sepP rP = (\x _ y -> (x, y)) <$> lP <*> (spaces >> sepP) <*> rP

dummyBindP :: Parser Void FixSymbol
dummyBindP = tempSymbol "db" <$> freshIntP

isPropBareType :: RType BTyCon t t1 -> Bool
isPropBareType  = isPrimBareType boolConName

isPrimBareType :: FixSymbol -> RType BTyCon t t1 -> Bool
isPrimBareType n (RApp tc [] _ _) = val (btc_tc tc) == n
isPrimBareType _ _                = False

getClasses :: RType BTyCon t t1 -> [RType BTyCon t t1]
getClasses (RApp tc ts ps r)
  | isTuple tc
  = concatMap getClasses ts
  | otherwise
  = [RApp (tc { btc_class = True }) ts ps r]
getClasses t
  = [t]

dummyP ::  Monad m => m (Reft Void -> b) -> m b
dummyP fm
  = fm `ap` return dummyReft

symsP :: (IsString tv, Monoid r)
      => Parser Void [(FixSymbol, RType c tv r)]
symsP
  = undefined -- do reservedOp "\\"
 --       ss <- sepBy symbolP spaces
 --       reservedOp "->"
 --       return $ (, dummyRSort) <$> ss
 -- <|> return []
 -- <?> "symsP"

dummyRSort :: (IsString tv, Monoid r) => RType c tv r
dummyRSort
  = RVar "dummy" mempty

predicatesP :: (IsString tv, Monoid r)
            => Parser Void [Ref (RType c tv r) BareType]
predicatesP
   =  (angles $ sepBy1 predicate1P comma)
  <|> return []
  <?> "predicatesP"

predicate1P :: (IsString tv, Monoid r)
            => Parser Void (Ref (RType c tv r) BareType)
predicate1P
   =  undefined -- try (RProp <$> symsP <*> refP bbaseP)
  -- <|> (rPropP [] . predUReft <$> monoPredicate1P)
  -- <|> (braces $ bRProp <$> symsP' <*> refaP)
  -- <?> "predicate1P"
  --  where
  --   symsP'       = do ss    <- symsP
  --                     fs    <- mapM refreshSym (fst <$> ss)
  --                     return $ zip ss fs
  --   refreshSym s = intSymbol s <$> freshIntP

mmonoPredicateP :: Parser Void Predicate
mmonoPredicateP
   = try (angles $ angles monoPredicate1P)
  <|> return mempty
  <?> "mmonoPredicateP"

monoPredicateP :: Parser Void Predicate
monoPredicateP
   = try (angles monoPredicate1P)
  <|> return mempty
  <?> "monoPredicateP"

monoPredicate1P :: Parser Void Predicate
monoPredicate1P
   =  (reserved "True" >> return mempty)
  <|> (pdVar <$> parens predVarUseP)
  <|> (pdVar <$>        predVarUseP)
  <?> "monoPredicate1P"

predVarUseP :: IsString t
            => Parser Void (PVar t)
predVarUseP
  = undefined -- do (p, xs) <- funArgsP
    --    return   $ PV p (PVProp dummyTyId) dummySymbol [ (dummyTyId, dummySymbol, x) | x <- xs ]

funArgsP :: Parser Void (FixSymbol, [(Expr Void)])
funArgsP  = undefined -- try realP <|> empP <?> "funArgsP"
  -- where
  --   empP  = (,[]) <$> predVarIdP
  --   realP = do (EVar lp, xs) <- splitEApp <$> funAppP
  --              return (lp, xs)

boundP :: Parser Void (Bound (Located BareType) (Expr Void))
boundP = undefined -- do
 --  name   <- locParserP upperIdP
 --  reservedOp "="
 --  vs     <- bvsP
 --  params <- many (parens tyBindP)
 --  args   <- bargsP
 --  body   <- predP
 --  return $ Bound name vs params args body
 -- where
 --    bargsP =     ( do reservedOp "\\"
 --                      xs <- many (parens tyBindP)
 --                      reservedOp  "->"
 --                      return xs
 --                 )
 --           <|> return []
 --           <?> "bargsP"
 --    bvsP   =     ( do reserved "forall"
 --                      xs <- many (locParserP (bTyVar <$> symbolP))
 --                      reservedOp  "."
 --                      return (fmap (`RVar` mempty) <$> xs)
 --                 )
 --           <|> return []


infixGenP :: Assoc -> Parser Void ()
infixGenP assoc = do
   spaces
   p <- maybeDigit
   spaces
   s <- infixIdP
   spaces
   addOperatorP (FInfix p s Nothing assoc)


infixP :: Parser Void ()
infixP = infixGenP AssocLeft

infixlP :: Parser Void ()
infixlP = infixGenP AssocLeft

infixrP :: Parser Void ()
infixrP = infixGenP AssocRight

maybeDigit :: Parser Void (Maybe Int)
maybeDigit
  = try (satisfy isDigit >>= return . Just . read . (:[]))
  <|> return Nothing

------------------------------------------------------------------------
----------------------- Wrapped Constructors ---------------------------
------------------------------------------------------------------------

bRProp :: [((FixSymbol, τ), FixSymbol)]
       -> Expr Void -> Ref τ (RType c BTyVar (UReft (Reft Void)))
bRProp []    _    = undefined -- panic Nothing "Parse.bRProp empty list"
-- bRProp syms' expr = RProp ss $ bRVar (BTV dummyName) mempty mempty r
--   where
--     (ss, (v, _))  = (init syms, last syms)
--     syms          = [(y, s) | ((_, s), y) <- syms']
--     su            = mkSubst [(x, EVar y) | ((x, _), y) <- syms']
--     r             = su `subst` Reft (v, expr)

bRVar :: tv -> Strata -> Predicate -> r -> RType c tv (UReft r)
bRVar α s p r             = RVar α (MkUReft r p s)

bLst :: Maybe (RType BTyCon tv (UReft r))
     -> [RTProp BTyCon tv (UReft r)]
     -> r
     -> RType BTyCon tv (UReft r)
bLst (Just t) rs r        = RApp (mkBTyCon $ dummyLoc listConName) [t] rs (reftUReft r)
bLst (Nothing) rs r       = RApp (mkBTyCon $ dummyLoc listConName) []  rs (reftUReft r)

bTup :: (PPrint r, Reftable Void r, Reftable Void (RType BTyCon BTyVar (UReft r)), Reftable Void (RTProp BTyCon BTyVar (UReft r)))
     => [(Maybe FixSymbol, RType BTyCon BTyVar (UReft r))]
     -> [RTProp BTyCon BTyVar (UReft r)]
     -> r
     -> RType BTyCon BTyVar (UReft r)
bTup [(_,t)] _ r = undefined
--   | isTauto r  = t
--   | otherwise  = t `strengthen` (reftUReft r)
-- bTup ts rs r
--   | all Mb.isNothing (fst <$> ts) || length ts < 2
--   = RApp (mkBTyCon $ dummyLoc tupConName) (snd <$> ts) rs (reftUReft r)
--   | otherwise
--   = RApp (mkBTyCon $ dummyLoc tupConName) ((top . snd) <$> ts) rs' (reftUReft r)
--   where
--     args       = [(Mb.fromMaybe dummySymbol x, mapReft mempty t) | (x,t) <- ts]
--     makeProp i = RProp (take i args) ((snd <$> ts)!!i)
--     rs'        = makeProp <$> [1..(length ts-1)]


-- Temporarily restore this hack benchmarks/esop2013-submission/Array.hs fails
-- w/o it
-- TODO RApp Int [] [p] true should be syntactically different than RApp Int [] [] p
-- bCon b s [RProp _ (RHole r1)] [] _ r = RApp b [] [] $ meet @LHSymbol r1 (MkUReft r mempty s)
bCon :: c
     -> Strata
     -> [RTProp c tv (UReft r)]
     -> [RType c tv (UReft r)]
     -> Predicate
     -> r
     -> RType c tv (UReft r)
bCon b s rs            ts p r = RApp b ts rs $ MkUReft r p s

bAppTy :: (Foldable t, PPrint r, Reftable Void r)
       => tv -> t (RType c tv (UReft r)) -> r -> RType c tv (UReft r)
bAppTy v ts r  = undefined -- ts' `strengthen` reftUReft r
  -- where
  --   ts'        = foldl' (\a b -> RAppTy a b mempty) (RVar v mempty) ts

reftUReft :: r -> UReft r
reftUReft r    = MkUReft r mempty mempty

predUReft :: Monoid r => Predicate -> UReft r
predUReft p    = MkUReft dummyReft p mempty

dummyReft :: Monoid a => a
dummyReft      = mempty

dummyTyId :: IsString a => a
dummyTyId      = ""

------------------------------------------------------------------
--------------------------- Measures -----------------------------
------------------------------------------------------------------

type BPspec = Pspec LocBareType (Located FixSymbol)

data Pspec ty ctor
  = Meas    (Measure ty ctor)                             -- ^ 'measure' definition
  | Assm    (Located FixSymbol, ty)                               -- ^ 'assume' signature (unchecked)
  | Asrt    (Located FixSymbol, ty)                               -- ^ 'assert' signature (checked)
  | LAsrt   (Located FixSymbol, ty)                               -- ^ 'local' assertion -- RJ: what is this
  -- YL : should parameterize then use tff 
  | Asrts   ([Located FixSymbol], (ty, Maybe [Located (Expr Void)]))     -- ^ RJ: what is this
  | Impt    FixSymbol                                        -- ^ 'import' a specification module
  | DDecl   DataDecl                                      -- ^ refined 'data'    declaration 
  | NTDecl  DataDecl                                      -- ^ refined 'newtype' declaration
  | Class   (RClass ty)                                   -- ^ refined 'class' definition
  | CLaws   (RClass ty)                                   -- ^ 'class laws' definition
  | ILaws   (RILaws ty)
  | RInst   (RInstance ty)                                -- ^ refined 'instance' definition
  | Incl    FilePath                                      -- ^ 'include' a path -- TODO: deprecate 
  | Invt    ty                                            -- ^ 'invariant' specification
  | Using  (ty, ty)                                       -- ^ 'using' declaration (for local invariants on a type) 
  | Alias   (Located (RTAlias FixSymbol BareType))           -- ^ 'type' alias declaration  
  | EAlias  (Located (RTAlias FixSymbol (Expr Void)))               -- ^ 'predicate' alias declaration
  | Embed   (Located FixSymbol, FTycon Void, TCArgs)                   -- ^ 'embed' declaration
  | Qualif  (Qualifier Void)                                     -- ^ 'qualif' definition
  | Decr    (Located FixSymbol, [Int])                            -- ^ 'decreasing' annotation -- TODO: deprecate
  | LVars   (Located FixSymbol)                                     -- ^ 'lazyvar' annotation, defer checks to *use* sites
  | Lazy    (Located FixSymbol)                                     -- ^ 'lazy' annotation, skip termination check on binder
  | Insts   (Located FixSymbol, Maybe Int)                        -- ^ 'auto-inst' or 'ple' annotation; use ple locally on binder 
  | HMeas   (Located FixSymbol)                                     -- ^ 'measure' annotation; lift Haskell binder as measure
  | Reflect (Located FixSymbol)                                     -- ^ 'reflect' annotation; reflect Haskell binder as function in logic
  | Inline  (Located FixSymbol)                                     -- ^ 'inline' annotation;  inline (non-recursive) binder as an alias
  | Ignore  (Located FixSymbol)                                     -- ^ 'ignore' annotation; skip all checks inside this binder
  | ASize   (Located FixSymbol)                                     -- ^ 'autosize' annotation; automatically generate size metric for this type
  | HBound  (Located FixSymbol)                                     -- ^ 'bound' annotation; lift Haskell binder as an abstract-refinement "bound"
  | PBound  (Bound ty (Expr Void))                               -- ^ 'bound' definition
  | Pragma  (Located String)                              -- ^ 'LIQUID' pragma, used to save configuration options in source files
  | CMeas   (Measure ty ())                               -- ^ 'class measure' definition
  | IMeas   (Measure ty ctor)                             -- ^ 'instance measure' definition
  | Varia   (Located FixSymbol, [Variance])                       -- ^ 'variance' annotations, marking type constructor params as co-, contra-, or in-variant
  | BFix    ()                                            -- ^ fixity annotation
  | Define  (Located FixSymbol, FixSymbol)                           -- ^ 'define' annotation for specifying aliases c.f. `include-CoreToLogic.lg`
  deriving (Data, Typeable)

instance (PPrint ty, PPrint ctor) => PPrint (Pspec ty ctor) where 
  pprintTidy = ppPspec 

splice :: PJ.Doc -> [PJ.Doc] -> PJ.Doc
splice sep = PJ.hcat . PJ.punctuate sep

ppAsserts :: (PPrint t) => Tidy -> [Located FixSymbol] -> t -> Maybe [Located (Expr Void)] -> PJ.Doc
ppAsserts k lxs t les 
  = PJ.hcat [ splice ", " (pprintTidy k <$> (val <$> lxs))
            , " :: " 
            , pprintTidy k t   
            , ppLes les 
            ]
  where 
    ppLes Nothing    = ""
    ppLes (Just les) = "/" <+> pprintTidy k (val <$> les)

ppPspec :: (PPrint t, PPrint c) => Tidy -> Pspec t c -> PJ.Doc
ppPspec k (Meas m)        
  = "measure" <+> pprintTidy k m 
ppPspec k (Assm (lx, t))  
  = "assume"  <+> pprintTidy k (val lx) <+> "::" <+> pprintTidy k t 
ppPspec k (Asrt (lx, t))  
  = "assert"  <+> pprintTidy k (val lx) <+> "::" <+> pprintTidy k t 
ppPspec k (LAsrt (lx, t)) 
  = "local assert"  <+> pprintTidy k (val lx) <+> "::" <+> pprintTidy k t 
ppPspec k (Asrts (lxs, (t, les))) 
  = ppAsserts k lxs t les
ppPspec k (Impt  x) 
  = "import" <+> pprintTidy k x 
ppPspec k (DDecl d) 
  = pprintTidy k d 
ppPspec k (NTDecl d) 
  = "newtype" <+> pprintTidy k d 
ppPspec _ (Incl f) 
  = "include" <+> "<" PJ.<> PJ.text f PJ.<> ">"
ppPspec k (Invt t)
  = "invariant" <+> pprintTidy k t 
ppPspec k (Using (t1, t2)) 
  = "using" <+> pprintTidy k t1 <+> "as" <+> pprintTidy k t2
ppPspec k (Alias   (Loc _ _ rta)) 
  = "type" <+> pprintTidy k rta 
ppPspec k (EAlias  (Loc _ _ rte)) 
  = "predicate" <+> pprintTidy k rte 
ppPspec k (Embed   (lx, tc, NoArgs)) 
  = "embed" <+> pprintTidy k (val lx)         <+> "as" <+> pprintTidy k tc 
ppPspec k (Embed   (lx, tc, WithArgs)) 
  = "embed" <+> pprintTidy k (val lx) <+> "*" <+> "as" <+> pprintTidy k tc 
ppPspec k (Qualif  q)              
  = pprintTidy k q 
ppPspec k (Decr (lx, ns))        
  = "decreasing" <+> pprintTidy k (val lx) <+> pprintTidy k ns
ppPspec k (LVars   lx) 
  = "lazyvar" <+> pprintTidy k (val lx) 
ppPspec k (Lazy   lx) 
  = "lazy" <+> pprintTidy k (val lx) 
ppPspec k (Insts   (lx, mbN)) 
  = "automatic-instances" <+> pprintTidy k (val lx) <+> maybe "" (("with" <+>) . pprintTidy k) mbN 
ppPspec k (HMeas   lx) 
  = "measure" <+> pprintTidy k (val lx) 
ppPspec k (Reflect lx) 
  = "reflect" <+> pprintTidy k (val lx) 
ppPspec k (Inline  lx) 
  = "inline" <+> pprintTidy k (val lx) 
ppPspec k (Ignore  lx) 
  = "ignore" <+> pprintTidy k (val lx) 
ppPspec k (HBound  lx) 
  = "bound" <+> pprintTidy k (val lx) 
ppPspec k (ASize   lx) 
  = "autosize" <+> pprintTidy k (val lx) 
ppPspec k (PBound  bnd) 
  = pprintTidy k bnd 
ppPspec _ (Pragma  (Loc _ _ s)) 
  = "LIQUID" <+> PJ.text s 
ppPspec k (CMeas   m) 
  = "class measure" <+> pprintTidy k m
ppPspec k (IMeas   m) 
  = "instance  measure" <+> pprintTidy k m
ppPspec k (Class   cls) 
  = pprintTidy k cls 
ppPspec k (CLaws  cls) 
  = pprintTidy k cls 
ppPspec k (RInst   inst) 
  = pprintTidy k inst 
ppPspec k (Varia   (lx, vs))  
  = "data variance" <+> pprintTidy k (val lx) <+> splice " " (pprintTidy k <$> vs) 
ppPspec _ (BFix    _)           -- 
  = "fixity"
ppPspec k (Define  (lx, y))     
  = "define" <+> pprintTidy k (val lx) <+> "=" <+> pprintTidy k y 
ppPspec _ (ILaws {}) 
  = "TBD-INSTANCE-LAWS"


-- | For debugging
{-instance Show (Pspec a b) where
  show (Meas   _) = "Meas"
  show (Assm   _) = "Assm"
  show (Asrt   _) = "Asrt"
  show (LAsrt  _) = "LAsrt"
  show (Asrts  _) = "Asrts"
  show (Impt   _) = "Impt"
  shcl  _) = "DDecl"
  show (NTDecl _) = "NTDecl"
  show (Incl   _) = "Incl"
  show (Invt   _) = "Invt"
  show (Using _) = "Using"
  show (Alias  _) = "Alias"
  show (EAlias _) = "EAlias"
  show (Embed  _) = "Embed"
  show (Qualif _) = "Qualif"
  show (Decr   _) = "Decr"
  show (LVars  _) = "LVars"
  show (Lazy   _) = "Lazy"
  -- show (Axiom  _) = "Axiom"
  show (Insts  _) = "Insts"
  show (Reflect _) = "Reflect"
  show (HMeas  _) = "HMeas"
  show (HBound _) = "HBound"
  show (Inline _) = "Inline"
  show (Pragma _) = "Pragma"
  show (CMeas  _) = "CMeas"
  show (IMeas  _) = "IMeas"
  show (Class  _) = "Class"
  show (Varia  _) = "Varia"
  show (PBound _) = "Bound"
  show (RInst  _) = "RInst"
  show (ASize  _) = "ASize"
  show (BFix   _) = "BFix"
  show (Define _) = "Define"-}

qualifySpec :: FixSymbol -> Spec ty bndr -> Spec ty bndr
qualifySpec name sp = sp { sigs      = [ (tx x, t)  | (x, t)  <- sigs sp]
                         -- , asmSigs   = [ (tx x, t)  | (x, t)  <- asmSigs sp]
                         }
  where
    tx = undefined -- fmap (qualifySymbol name)

mkSpec :: ModName -> [BPspec] -> (ModName, Measure.Spec LocBareType (Located FixSymbol))
mkSpec name xs         = undefined -- (name,) $ qualifySpec (FixSymbol name) Measure.Spec
  -- { Measure.measures   = [m | Meas   m <- xs]
  -- , Measure.asmSigs    = [a | Assm   a <- xs]
  -- , Measure.sigs       = [a | Asrt   a <- xs]
  --                     ++ [(y, t) | Asrts (ys, (t, _)) <- xs, y <- ys]
  -- , Measure.localSigs  = []
  -- , Measure.reflSigs   = []
  -- , Measure.impSigs    = []
  -- , Measure.expSigs    = [] 
  -- , Measure.invariants = [(Nothing, t) | Invt   t <- xs]
  -- , Measure.ialiases   = [t | Using t <- xs]
  -- , Measure.imports    = [i | Impt   i <- xs]
  -- , Measure.dataDecls  = [d | DDecl  d <- xs] ++ [d | NTDecl d <- xs]
  -- , Measure.newtyDecls = [d | NTDecl d <- xs]
  -- , Measure.includes   = [q | Incl   q <- xs]
  -- , Measure.aliases    = [a | Alias  a <- xs]
  -- , Measure.ealiases   = [e | EAlias e <- xs]
  -- , Measure.embeds     = tceFromList [(c, (fTyconSort tc, a)) | Embed (c, tc, a) <- xs]
  -- , Measure.qualifiers = [q | Qualif q <- xs]
  -- , Measure.decr       = [d | Decr d   <- xs]
  -- , Measure.lvars      = S.fromList [d | LVars d  <- xs]
  -- , Measure.autois     = M.fromList [s | Insts s <- xs]
  -- , Measure.pragmas    = [s | Pragma s <- xs]
  -- , Measure.cmeasures  = [m | CMeas  m <- xs]
  -- , Measure.imeasures  = [m | IMeas  m <- xs]
  -- , Measure.classes    = [c | Class  c <- xs]
  -- , Measure.claws      = [c | CLaws  c <- xs]
  -- , Measure.dvariance  = [v | Varia  v <- xs]
  -- , Measure.rinstance  = [i | RInst  i <- xs]
  -- , Measure.ilaws      = [i | ILaws  i <- xs]
  -- , Measure.termexprs  = [(y, es) | Asrts (ys, (_, Just es)) <- xs, y <- ys]
  -- , Measure.lazy       = S.fromList [s | Lazy   s <- xs]
  -- , Measure.bounds     = M.fromList [(bname i, i) | PBound i <- xs]
  -- , Measure.reflects   = S.fromList [s | Reflect s <- xs]
  -- , Measure.hmeas      = S.fromList [s | HMeas  s <- xs]
  -- , Measure.inlines    = S.fromList [s | Inline s <- xs]
  -- , Measure.ignores    = S.fromList [s | Ignore s <- xs]
  -- , Measure.autosize   = S.fromList [s | ASize  s <- xs]
  -- , Measure.hbounds    = S.fromList [s | HBound s <- xs]
  -- , Measure.defs       = M.fromList [d | Define d <- xs]
  -- , Measure.axeqs      = []
  -- }

-- | Parse a single top level liquid specification
specP :: Parser Void BPspec
specP
  =     (fallbackSpecP "assume"     (liftM Assm    tyBindP  ))
    <|> (fallbackSpecP "assert"     (liftM Asrt    tyBindP  ))
    <|> (fallbackSpecP "autosize"   (liftM ASize   asizeP   ))
    <|> (reserved "local"         >> liftM LAsrt   tyBindP  )

    -- TODO: These next two are synonyms, kill one
    <|> (fallbackSpecP "axiomatize" (liftM Reflect axiomP   ))
    <|> (fallbackSpecP "reflect"    (liftM Reflect axiomP   ))

    <|> (fallbackSpecP "measure"    hmeasureP)

    <|> (fallbackSpecP "define"     (liftM Define  defineP  ))
    <|> (reserved "infixl"        >> liftM BFix    infixlP  )
    <|> (reserved "infixr"        >> liftM BFix    infixrP  )
    <|> (reserved "infix"         >> liftM BFix    infixP   )
    <|> (fallbackSpecP "inline"     (liftM Inline  inlineP  ))
    <|> (fallbackSpecP "ignore"     (liftM Ignore  inlineP  ))

    <|> (fallbackSpecP "bound"    (((liftM PBound  boundP   )
                                <|> (liftM HBound  hboundP  ))))
    <|> (reserved "class"
         >> ((reserved "measure"  >> liftM CMeas  cMeasureP )
         <|> (reserved "laws"     >> liftM CLaws  classP)
         <|> liftM Class  classP                            ))
    <|> (reserved "instance"
         >> ((reserved "measure"  >> liftM IMeas  iMeasureP )
         <|> (reserved "laws"     >> liftM ILaws instanceLawP)
         <|> liftM RInst  instanceP ))

    -- <|> (reserved "import"        >> liftM Impt   symbolP   )

    <|> (reserved "data"
        >> ((reserved "variance"  >> liftM Varia  datavarianceP)
                                 <|> liftM DDecl  dataDeclP ))

    <|> (reserved "newtype"       >> liftM NTDecl dataDeclP )
    <|> (reserved "include"       >> liftM Incl   filePathP )
    <|> (fallbackSpecP "invariant"  (liftM Invt   invariantP))
    <|> (reserved "using"         >> liftM Using invaliasP )
    <|> (reserved "type"          >> liftM Alias  aliasP    )

    -- TODO: Next two are basically synonyms
    <|> (fallbackSpecP "predicate"  (liftM EAlias ealiasP   ))
    <|> (fallbackSpecP "expression" (liftM EAlias ealiasP   ))

    <|> (fallbackSpecP "embed"      (liftM Embed  embedP    ))
    <|> (fallbackSpecP "qualif"     (liftM Qualif (qualifierP sortP)))
    <|> (reserved "decrease"      >> liftM Decr   decreaseP )
    <|> (reserved "lazyvar"       >> liftM LVars  lazyVarP  )

    <|> (reserved "lazy"          >> liftM Lazy   lazyVarP  )
    <|> (reserved "ple"           >> liftM Insts autoinstP  )
    <|> (reserved "automatic-instances" >> liftM Insts autoinstP  )
    <|> (reserved "LIQUID"        >> liftM Pragma pragmaP   )
    <|> {- DEFAULT -}                liftM Asrts  tyBindsP
    <?> "specP"

-- | Try the given parser on the tail after matching the reserved word, and if
-- it fails fall back to parsing it as a haskell signature for a function with
-- the same name.
fallbackSpecP :: String -> Parser Void BPspec -> Parser Void BPspec
fallbackSpecP kw p = do
  (Loc l1 l2 _) <- locParserP (reserved kw)
  (p <|> liftM Asrts (tyBindsRemP (Loc l1 l2 (symbol kw)) ))

-- | Same as tyBindsP, except the single initial symbol has already been matched
tyBindsRemP :: Located FixSymbol -> Parser Void ([Located FixSymbol], (Located BareType, Maybe [Located (Expr Void)]))
tyBindsRemP sym = do
  dcolon
  tb <- termBareTypeP
  return ([sym],tb)

pragmaP :: Parser Void (Located String)
pragmaP = locParserP stringLiteral

autoinstP :: Parser Void (Located FixSymbol, Maybe Int)
autoinstP = do x <- locParserP binderP
               spaces
               i <- maybeP (reserved "with" >> integer)
               return (x, fromIntegral <$> i)

lazyVarP :: Parser Void (Located FixSymbol)
lazyVarP = locParserP binderP

axiomP :: Parser Void (Located FixSymbol)
axiomP = locParserP binderP

hboundP :: Parser Void (Located FixSymbol)
hboundP = locParserP binderP

inlineP :: Parser Void (Located FixSymbol)
inlineP = locParserP binderP

asizeP :: Parser Void (Located FixSymbol)
asizeP = locParserP binderP

decreaseP :: Parser Void (Located FixSymbol, [Int])
decreaseP = Misc.mapSnd f <$> liftM2 (,) (locParserP binderP) (spaces >> many integer)
  where
    f     = ((\n -> fromInteger n - 1) <$>)

filePathP     :: Parser Void FilePath
filePathP     = angles $ many1 pathCharP
  where
    pathCharP = choice $ char <$> pathChars
    pathChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['.', '/']

datavarianceP :: Parser Void (Located FixSymbol, [Variance])
datavarianceP = undefined -- liftM2 (,) locUpperIdP (spaces >> many varianceP)

varianceP :: Parser Void Variance
varianceP = (reserved "bivariant"     >> return Bivariant)
        <|> (reserved "invariant"     >> return Invariant)
        <|> (reserved "covariant"     >> return Covariant)
        <|> (reserved "contravariant" >> return Contravariant)
        <?> "Invalid variance annotation\t Use one of bivariant, invariant, covariant, contravariant"

tyBindsP :: Parser Void ([Located FixSymbol], (Located BareType, Maybe [Located (Expr Void)]))
tyBindsP = do 
  (xs, z) <- xyP (sepBy (locParserP binderP) comma) dcolon termBareTypeP
  when (null xs) (parserFail $ "Type signature " ++ show z ++ " must have non-empty list of binders!") 
  return (xs, z)


tyBindNoLocP :: Parser Void (Located FixSymbol, BareType)
tyBindNoLocP = second val <$> tyBindP


tyBindP    :: Parser Void (Located FixSymbol, Located BareType)
tyBindP    = xyP xP dcolon tP
  where
    xP     = locParserP binderP
    tP     = locParserP genBareTypeP

termBareTypeP :: Parser Void (Located BareType, Maybe [Located (Expr Void)])
termBareTypeP = do
  t <- locParserP genBareTypeP
  (termTypeP t
    <|> return (t, Nothing))

termTypeP :: Located BareType ->Parser Void (Located BareType, Maybe [Located (Expr Void)])
termTypeP t
  = do
       reservedOp "/"
       es <- brackets $ sepBy (locParserP exprP) comma
       return (t, Just es)

-- -------------------------------------

invariantP :: Parser Void (Located BareType)
invariantP = locParserP genBareTypeP

invaliasP :: Parser Void (Located BareType, Located BareType)
invaliasP
  = do t  <- locParserP genBareTypeP
       reserved "as"
       ta <- locParserP genBareTypeP
       return (t, ta)

genBareTypeP :: Parser Void BareType
genBareTypeP = bareTypeP

embedP :: Parser Void (Located FixSymbol, FTycon Void, TCArgs)
embedP = do 
  x <- locUpperIdP 
  a <- try (reserved "*" >> return WithArgs) <|> return NoArgs 
  _ <- spaces >> reserved "as"
  t <- fTyConP
  undefined
  -- return (x, t, a)
  --  = xyP locUpperIdP symbolTCArgs (reserved "as") fTyConP


aliasP :: Parser Void (Located (RTAlias FixSymbol BareType))
aliasP  = rtAliasP id     bareTypeP

-- YL : use polymorphic type also seems to work
ealiasP :: Parser Void (Located (RTAlias FixSymbol (Expr s)))
ealiasP = undefined -- try (rtAliasP symbol predP)
      -- <|> rtAliasP symbol exprP
      -- <?> "ealiasP"

rtAliasP :: (FixSymbol -> tv) -> Parser Void ty -> Parser Void (Located (RTAlias tv ty))
rtAliasP f bodyP
  -- TODO:AZ pretty sure that all the 'spaces' can be removed below, given
  --         proper use of reserved and reservedOp now
  = undefined -- do pos  <- getPosition
    --    name <- upperIdP
    --    spaces
    --    args <- sepBy aliasIdP blanks
    --    whiteSpace >> reservedOp "=" >> whiteSpace
    --    body <- bodyP
    --    posE <- getPosition
    --    let (tArgs, vArgs) = partition (isSmall . headSym) args
    --    return $ Loc pos posE (RTA name (f <$> tArgs) vArgs body)

aliasIdP :: Parser Void FixSymbol
aliasIdP = condIdP (letter <|> char '_') alphaNums (isAlpha . head)
           where
             alphaNums = S.fromList $ ['A' .. 'Z'] ++ ['a'..'z'] ++ ['0'..'9']

hmeasureP :: Parser Void BPspec
hmeasureP = do
  b <- locParserP binderP
  spaces
  undefined -- ((do dcolon
  --      ty <- locParserP genBareTypeP
  --      whiteSpace
  --      eqns <- grabs $ measureDefP (rawBodyP <|> tyBodyP ty)
  --      return (Meas $ Measure.mkM b ty eqns MsMeasure mempty))
  --   <|> (return (HMeas b))
  --   )

measureP :: Parser Void (Measure (Located BareType) (Located FixSymbol))
measureP = do 
  (x, ty) <- tyBindP
  whiteSpace
  eqns    <- grabs $ measureDefP (rawBodyP <|> tyBodyP ty)
  undefined
  -- return   $ Measure.mkM x ty eqns MsMeasure mempty

-- | class measure
cMeasureP :: Parser Void (Measure (Located BareType) ())
cMeasureP
  = undefined -- do (x, ty) <- tyBindP
    --    return $ Measure.mkM x ty [] MsClass mempty 

iMeasureP :: Parser Void (Measure (Located BareType) (Located FixSymbol))
iMeasureP = measureP


oneClassArg :: Parser Void [Located BareType]
oneClassArg
  = undefined -- sing <$> locParserP (rit <$> classBTyConP <*> (map val <$> classParams))
  -- where
  --   rit t as    = RApp t ((`RVar` mempty) <$> as) [] mempty
  --   classParams =  (reserved "where" >> return [])
  --              <|> ((:) <$> (fmap bTyVar <$> locLowerIdP) <*> classParams)
  --   sing x      = [x]

instanceLawP :: Parser Void (RILaws (Located BareType))
instanceLawP
  = do l1   <- getPosition
       sups <- supersP
       c    <- classBTyConP
       spaces
       tvs  <- manyTill (locParserP bareTypeP) (try $ reserved "where")
       spaces
       ms   <- grabs eqBinderP
       spaces
       l2   <- getPosition
       undefined
       -- return $ RIL c sups tvs ms (Loc l1 l2 ())
  where
    superP   = locParserP (toRCls <$> bareAtomBindP)
    supersP  = try (((parens (superP `sepBy1` comma)) <|> fmap pure superP)
                       <* reservedOp "=>")
               <|> return []
    toRCls x = x

    eqBinderP = xyP xP (spaces >> string "=" <* spaces) (xP <* spaces)
      
    xP = locParserP binderP
    

instanceP :: Parser Void (RInstance (Located BareType))
instanceP
  = undefined -- do _    <- supersP
  --      c    <- classBTyConP
  --      spaces
  --      tvs  <- (try oneClassArg) <|> (manyTill iargsP (try $ reserved "where"))
  --      ms   <- sepBy riMethodSigP semi
  --      spaces
  --      return $ RI c tvs ms
  -- where
  --   superP   = locParserP (toRCls <$> bareAtomBindP)
  --   supersP  = try (((parens (superP `sepBy1` comma)) <|> fmap pure superP)
  --                      <* reservedOp "=>")
  --              <|> return []
  --   toRCls x = x

  --   iargsP   =   (mkVar . bTyVar <$> tyVarIdP)
  --           <|> (parens $ locParserP $ bareTypeP)


  --   mkVar v  = dummyLoc $ RVar v mempty


riMethodSigP :: Parser Void (Located FixSymbol, RISig (Located BareType))
riMethodSigP
  = try (do reserved "assume"
            (x, t) <- tyBindP
            return (x, RIAssumed t) )
 <|> do (x, t) <- tyBindP
        return (x, RISig t)
 <?> "riMethodSigP"

classP :: Parser Void (RClass (Located BareType))
classP
  = undefined -- do sups <- supersP
  --      c    <- classBTyConP
  --      spaces
  --      tvs  <- manyTill (bTyVar <$> tyVarIdP) (try $ reserved "where")
  --      ms   <- try (grabs tyBindP) -- <|> sepBy tyBindP semi
  --      spaces
  --      return $ RClass c sups tvs ms
  -- where
  --   superP   = locParserP (toRCls <$> bareAtomBindP)
  --   supersP  = try (((parens (superP `sepBy1` comma)) <|> fmap pure superP)
  --                      <* reservedOp "=>")
  --              <|> return []
  --   toRCls x = x

rawBodyP :: Parser Void Body
rawBodyP
  = undefined -- braces $ do
    --   v <- symbolP
    --   reservedOp "|"
    --   p <- predP <* spaces
    --   return $ R v p

tyBodyP :: Located BareType -> Parser Void Body
tyBodyP ty
  = undefined -- case outTy (val ty) of
    --   Just bt | isPropBareType bt
    --             -> P <$> predP
    --   _         -> E <$> exprP
    -- where outTy (RAllT _ t)    = outTy t
    --       outTy (RAllP _ t)    = outTy t
    --       outTy (RImpF _ _ t _)= Just t
    --       outTy (RFun _ _ t _) = Just t
    --       outTy _              = Nothing

locUpperIdP' :: Parser Void (Located FixSymbol)
locUpperIdP' = locParserP upperIdP'

upperIdP' :: Parser Void FixSymbol
upperIdP' = try (symbol <$> condIdP' (isUpper . head))
        <|> (symbol <$> infixCondIdP')

-- TODO:AZ this looks dodgy, rather use reserved, reservedOp
condIdP'  :: (String -> Bool) -> Parser Void FixSymbol
condIdP' f
  = do c  <- letter
       let isAlphaNumOr' c = isAlphaNum c || ('\''== c)
       cs <- many (satisfy isAlphaNumOr')
       blanks
       if f (c:cs) then return (symbol $ T.pack $ c:cs) else parserZero

infixCondIdP' :: Parser Void FixSymbol
infixCondIdP'
  = do sym <- parens $ do
         c1 <- colon
         -- This is the same thing as 'startsVarSymASCII' from ghc-boot-th,
         -- but LH can't use that at the moment since it requires GHC 7.10.
         let isASCIISymbol = (`elem` ("!#$%&*+./<=>?@\\^|~-" :: String))
         ss <- many (satisfy isASCIISymbol)
         c2 <- colon
         return $ symbol $ T.pack $ c1 ++ ss ++ c2
       blanks
       return sym

-- | LHS of the thing being defined
binderP :: Parser Void FixSymbol
binderP    = pwr    <$> parens (idP bad)
         <|> symbol <$> idP badc
  where
    idP p  = many1 (satisfy (not . p))
    badc c = (c == ':') || (c == ',') || bad c
    bad c  = isSpace c || c `elem` ("(,)" :: String)
    pwr s  = symbol $ "(" `mappend` s `mappend` ")"

grabs :: ParsecT s u m a -> ParsecT s u m [a]
grabs p = try (liftM2 (:) p (grabs p))
       <|> return []

measureDefP :: Parser Void Body -> Parser Void (Def (Located BareType) (Located FixSymbol))
measureDefP bodyP
  = undefined -- do mname   <- locParserP symbolP
    --    (c, xs) <- measurePatP
    --    whiteSpace >> reservedOp "=" >> whiteSpace
    --    body    <- bodyP
    --    whiteSpace
    --    let xs'  = (symbol . val) <$> xs
    --    return   $ Def mname (symbol <$> c) Nothing ((, Nothing) <$> xs') body

measurePatP :: Parser Void (Located FixSymbol, [Located FixSymbol])
measurePatP
  =  parens (try conPatP <|> try consPatP <|> nilPatP <|> tupPatP)
 <|> nullaryConPatP
 <?> "measurePatP"

tupPatP :: Parser Void (Located FixSymbol, [Located FixSymbol])
tupPatP  = undefined -- mkTupPat  <$> sepBy1 locLowerIdP comma

conPatP :: Parser Void (Located FixSymbol, [Located FixSymbol])
conPatP  = undefined -- (,)       <$> locParserP dataConNameP <*> sepBy locLowerIdP whiteSpace

consPatP :: IsString a
         => Parser Void (Located a, [Located FixSymbol])
consPatP = undefined -- mkConsPat <$> locLowerIdP  <*> colon <*> locLowerIdP

nilPatP :: IsString a
        => Parser Void (Located a, [t])
nilPatP  = mkNilPat  <$> brackets whiteSpace

nullaryConPatP :: Parser Void (Located FixSymbol, [t])
nullaryConPatP = nilPatP <|> ((,[]) <$> locParserP dataConNameP)
                 <?> "nullaryConPatP"

mkTupPat :: Foldable t => t a -> (Located FixSymbol, t a)
mkTupPat zs     = (tupDataCon (length zs), zs)

mkNilPat :: IsString a => t -> (Located a, [t1])
mkNilPat _      = (dummyLoc "[]", []    )

mkConsPat :: IsString a => t1 -> t -> t1 -> (Located a, [t1])
mkConsPat x _ y = (dummyLoc ":" , [x, y])

tupDataCon :: Int -> Located FixSymbol
tupDataCon n    = dummyLoc $ symbol $ "(" <> replicate (n - 1) ',' <> ")"


-------------------------------------------------------------------------------
--------------------------------- Predicates ----------------------------------
-------------------------------------------------------------------------------

dataConFieldsP :: Parser Void [(FixSymbol, BareType)]
dataConFieldsP
   =  braces (sepBy predTypeDDP comma)
  <|> sepBy dataConFieldP spaces
  <?> "dataConFieldP"

dataConFieldP :: Parser Void (FixSymbol, BareType)
dataConFieldP
   =  parens (try predTypeDDP <|> dbTypeP)
  <|> dbTypeP
  <?> "dataConFieldP"
  where
    dbTypeP = (,) <$> dummyBindP <*> bareTypeP

predTypeDDP :: Parser Void (FixSymbol, BareType)
predTypeDDP = (,) <$> bbindP <*> bareTypeP

bbindP   :: Parser Void FixSymbol
bbindP   = undefined -- lowerIdP <* dcolon

dataConP :: [FixSymbol] -> Parser Void DataCtor
dataConP as = undefined -- do
  -- x   <- locParserP dataConNameP
  -- spaces
  -- xts <- dataConFieldsP
  -- return $ DataCtor x as [] xts Nothing

adtDataConP :: [FixSymbol] -> Parser Void DataCtor
adtDataConP as = undefined -- do
  -- x     <- locParserP dataConNameP
  -- dcolon
  -- tr    <- toRTypeRep <$> bareTypeP
  -- return $ DataCtor x (tRepVars as tr) [] (tRepFields tr) (Just $ ty_res tr)

tRepVars :: FixSymbolic a => [FixSymbol] -> RTypeRep c a r -> [FixSymbol]
tRepVars as tr = case ty_vars tr of 
  [] -> as 
  vs -> symbol . ty_var_value <$> vs 

tRepFields :: RTypeRep c tv r -> [(FixSymbol, RType c tv r)]
tRepFields tr = undefined -- zip (ty_binds tr) (ty_args tr)

dataConNameP :: Parser Void FixSymbol
dataConNameP
  = undefined -- try upperIdP
 -- <|> pwr <$> parens (idP bad)
 -- <?> "dataConNameP"
 --  where
 --     idP p  = many1 (satisfy (not . p))
 --     bad c  = isSpace c || c `elem` ("(,)" :: String)
 --     pwr s  = symbol $ "(" <> s <> ")"

dataSizeP :: Parser Void (Maybe SizeFun)
dataSizeP
  = undefined -- brackets (Just . SymSizeFun <$> locLowerIdP)
  -- <|> return Nothing

dataDeclP :: Parser Void DataDecl
dataDeclP = do
  pos <- getPosition
  x   <- locUpperIdP'
  spaces
  fsize <- dataSizeP
  (dataDeclBodyP pos x fsize <|> return (emptyDecl x pos fsize))

emptyDecl :: Located FixSymbol -> SourcePos -> Maybe SizeFun -> DataDecl
emptyDecl x pos fsize@(Just _)
  = undefined -- DataDecl (DnName x) [] [] [] [] pos fsize Nothing DataUser
emptyDecl x pos _
  = uError (ErrBadData (sourcePosSrcSpan pos) (pprint (val x)) msg)
  where
    msg = "You should specify either a default [size] or one or more fields in the data declaration"

dataDeclBodyP :: SourcePos -> Located FixSymbol -> Maybe SizeFun -> Parser Void DataDecl
dataDeclBodyP pos x fsize = undefined -- do
  -- vanilla    <- null <$> sepBy locUpperIdP blanks
  -- as         <- sepBy noWhere blanks
  -- ps         <- predVarDefsP
  -- (pTy, dcs) <- dataCtorsP as
  -- let dn      = dataDeclName pos x vanilla dcs
  -- whiteSpace
  -- return      $ DataDecl dn as ps [] dcs pos fsize pTy DataUser

dataDeclName :: SourcePos -> Located FixSymbol -> Bool -> [DataCtor] -> DataName
dataDeclName _ x True  _     = undefined -- DnName x               -- vanilla data    declaration
dataDeclName _ _ False (d:_) = DnCon  (dcName d)      -- family instance declaration
dataDeclName p x _  _        = uError (ErrBadData (sourcePosSrcSpan p) (pprint (val x)) msg)
  where
    msg                  = "You should specify at least one data constructor for a family instance"

dataCtorsP :: [FixSymbol] -> Parser Void (Maybe BareType, [DataCtor])
dataCtorsP as = do
  (pTy, dcs) <-     (reservedOp "="     >> ((Nothing, ) <$>                 sepBy (dataConP    as) (reservedOp "|")))
                <|> (reserved   "where" >> ((Nothing, ) <$>                 sepBy (adtDataConP as) (reservedOp "|")))
                <|> (                      ((,)         <$> dataPropTyP <*> sepBy (adtDataConP as) (reservedOp "|")))
  return (pTy, Misc.sortOn (val . dcName) dcs)

noWhere :: Parser Void FixSymbol
noWhere = try $ do
  s <- tyVarIdP
  if s == "where"
    then parserZero
    else return s

dataPropTyP :: Parser Void (Maybe BareType)
dataPropTyP = Just <$> between dcolon (reserved "where") bareTypeP

---------------------------------------------------------------------
-- | Parsing Qualifiers ---------------------------------------------
---------------------------------------------------------------------

fTyConP :: Parser Void (FTycon s)
fTyConP
  =  undefined -- (reserved "int"     >> return intFTyCon)
  -- <|> (reserved "Integer" >> return intFTyCon)
  -- <|> (reserved "Int"     >> return intFTyCon)
  -- <|> (reserved "real"    >> return realFTyCon)
  -- <|> (reserved "bool"    >> return boolFTyCon)
  -- <|> (symbolFTycon      <$> locUpperIdP)
  -- <?> "fTyConP"
