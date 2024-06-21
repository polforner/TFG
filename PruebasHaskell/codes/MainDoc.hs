--Main with array input and console output

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
import Relude hiding (Word) 

type Word = String
type Pair = (Word,Int)
type FilterState = Int
type FilterPar = Word
--Aixo es la definicio del typus de la pipeline
type DPExample = Source (Channel (Word :<+> Pair :<+> Eof)) :=> Generator (Channel (Word :<+> Pair :<+> Eof)) :=> Sink

input :: [String]
input = ["hola","que","tal","hola",".","aixo","es","que","tal",".","hola","hola","hola","hola","."]
--input = ["hola","."]
source' :: Stage (WriteChannel Word -> WriteChannel Pair -> DP s ())
source' = withSource @DPExample fillChannels

fillChannels :: WriteChannel Word
             -> WriteChannel Pair
            -> DP st ()
fillChannels ww wp = unfoldT input ww id

--GENERATOR--
generator' :: GeneratorStage DPExample FilterState FilterPar s
generator' =
  let gen = withGenerator @DPExample genAction
  in  mkGenerator gen filterTemp

genAction :: Filter DPExample FilterState FilterPar s
          -> ReadChannel Word
          -> ReadChannel Pair
          -> WriteChannel Word
          -> WriteChannel Pair
          -> DP s ()
genAction filter' rw rp ww wp = do 
  let unfoldFilter = mkUnfoldFilter' create' filter' iniFilter rw (rp .*. HNil)
  HCons rp' HNil <- unfoldF unfoldFilter
  (|=>|) rp' wp $ id

create' :: Word -> Bool
create' "." = False
create' _ = True

iniFilter :: Word -> FilterState
iniFilter _ = 1

--FILTER--
filterTemp :: Filter DPExample FilterState FilterPar s 
filterTemp = mkFilter actor1

actor1 :: FilterPar
        -> ReadChannel Word
        -> ReadChannel Pair
        -> WriteChannel Word
        -> WriteChannel Pair
        -> StateT FilterState (DP s) ()
actor1 par rw rp ww wp = do
  map_ rp wp id
  foldM_ rw $ \inp -> if inp == "." 
                      then get >>= \x -> push (par,x) wp >> push (".",0) wp
                      else  if inp == par
                            then get >>= \x -> put (x + 1)
                            else push inp ww
  
  

actor2 :: FilterPar
        -> ReadChannel Word
        -> ReadChannel Pair
        -> WriteChannel Word
        -> WriteChannel Pair
        -> StateT FilterState (DP s) ()
actor2 par rw rp ww wp = do
  foldM_ rw $ \inp -> if inp == "." then get >>= \x -> push (par,x) wp else pure ()
  rp |=>| wp $ id

pushResult :: MonadIO m => Word -> Int -> WriteChannel Pair -> m ()
pushResult w state wc = push (w,state) wc

--SINK--
sink' :: Stage (ReadChannel Word -> ReadChannel Pair -> DP s ())
sink' = withSink @DPExample readChannels

readChannels :: ReadChannel Word -> ReadChannel Pair -> DP st ()
readChannels rw rp = do 
  foldM_ rp print

main :: IO ()
main = runDP $ mkDP @DPExample source' generator' sink'