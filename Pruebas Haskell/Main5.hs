--Main with file input and file output, but counting characets, for large files
{-# LANGUAGE 
    ConstraintKinds,
    DataKinds,
    DefaultSignatures,
    DeriveAnyClass,
    DeriveFoldable,
    DeriveFunctor,
    DeriveGeneric,
    DeriveLift,
    DeriveTraversable,
    DerivingStrategies,
    DerivingVia,
    EmptyCase,
    FlexibleContexts,
    FlexibleInstances,
    FunctionalDependencies,
    GADTs,
    GeneralizedNewtypeDeriving,
    LambdaCase,
    MultiParamTypeClasses,
    MultiWayIf,
    NoImplicitPrelude,
    OverloadedStrings,
    PackageImports,
    PartialTypeSignatures,
    PolyKinds,
    Rank2Types,
    RankNTypes,
    RecordWildCards,
    ScopedTypeVariables,
    StandaloneDeriving,
    TemplateHaskell,
    TupleSections,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-}

import DynamicPipeline
import Relude as R
import Relude.Unsafe as U
import GHC.IO.Handle as H
import Data.ByteString as B
import Data.Text as T


--Aixo es la definicio del typus de la pipeline
type DPExample = Source (Channel ( (ByteString,Int) :<+> Eof)) :=> Generator (Channel ((ByteString,Int):<+> Eof)) :=> Sink

input :: [Char]
input = "test4.txt"
--input = "salida.txt"

--SOURCE--
--Aixo es la definicio de la font de la pipeline, utilitza el combinador per generar una font
source' :: Stage (WriteChannel (ByteString,Int) -> DP s ())
source' = withSource @DPExample $ \cout -> unfoldFile' input cout convert

unfoldFile':: FilePath -> WriteChannel (ByteString,Int) -> (ByteString -> (ByteString,Int)) -> DP s ()
unfoldFile' file writeChannel fn =
  liftIO $ R.withFile file ReadMode $ \h -> unfoldM (hGet h 1) fn (H.hIsEOF h) writeChannel

convert :: ByteString -> (ByteString,Int)
convert bs = (bs,0)

makeitSafe :: Maybe ByteString -> ByteString
makeitSafe (Just a) = a
makeitSafe Nothing = ""

--GENERATOR--
generator' :: GeneratorStage DPExample (ByteString,Int) (ByteString,Int) s
generator' =
  let gen = withGenerator @DPExample genAction
  in  mkGenerator gen filterTemp
genAction :: Filter DPExample (ByteString,Int) (ByteString,Int) s 
          -> ReadChannel (ByteString,Int)
          -> WriteChannel (ByteString,Int)
          -> DP s ()
genAction filter' cin cout = 
  --let unfoldFilter = mkUnfoldFilterForAll' (flip push cout) filter' identity cin HNil 
  let unfoldFilter = mkUnfoldFilter makeFilter (decideIfPrint cout) filter' iniFilter cin HNil 
  in void $ unfoldF unfoldFilter

makeFilter:: (ByteString,Int) -> Bool
makeFilter (".",_) = False  
makeFilter (_,0) = True
makeFilter _ = False

decideIfPrint :: WriteChannel (ByteString,Int) -> ((ByteString,Int) -> DP s ())
decideIfPrint c (".",_) = push ("FIN DE FRASE",0) c
decideIfPrint _ (_,0) = return ()
decideIfPrint c a = push a c

iniFilter :: (ByteString,Int) -> (ByteString,Int)
iniFilter (s,_) = (s,1)

--FILTER--
filterTemp :: Filter DPExample (ByteString,Int) (ByteString,Int) s 
filterTemp = mkFilter actorRepeted

actorRepeted :: (ByteString,Int)
             -> ReadChannel (ByteString,Int)
             -> WriteChannel (ByteString,Int)
             -> StateT (ByteString,Int) (DP s) ()
actorRepeted (par,x) rc wc = foldM_ rc $ \(inp, y) -> if inp == "." 
                                                        then get >>= flip push wc >> push (".",0) wc 
                                                        else if inp == par
                                                              then get >>= \(stat,z) -> put (stat,z + 1)
                                                              else push (inp, y) wc
--SINK--
sink' :: Stage (ReadChannel (ByteString,Int) -> DP s ())
--sink' = withSink @DPExample $ flip foldM_ print
sink' = withSink @DPExample $ flip foldM_ (R.appendFileText "output.txt" . showPair)

showPair :: (ByteString,Int) -> Text
showPair ("FIN DE FRASE",x) = "*******************************\n"
showPair (s,x) = decodeUtf8 $ B.concat [s, ": ",  fromString (show x), "\n"]

dp' :: DP s ()
dp' = mkDP @DPExample source' generator' sink'

main :: IO ()
main = runDP dp'