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
import Relude


--Aixo es la definicio del typus de la pipeline
type DPExample = Source (Channel ( ([Char],Int) :<+> Eof)) :=> Generator (Channel (([Char],Int):<+> Eof)) :=> Sink

input :: [[Char]]
input = [
        "hola","que","tal","hola","."
        ,"aixo","es","que","tal","."
        ,"hola","hola","hola","hola","."
        ]
source' :: Stage (WriteChannel ([Char],Int) -> DP s ())
source' = withSource @DPExample $ \cout -> unfoldT input cout makePair

makePair:: [Char] -> ([Char],Int)
makePair s = (s,0)

--GENERATOR--
generator' :: GeneratorStage DPExample ([Char],Int) ([Char],Int) s
generator' =
  let gen = withGenerator @DPExample genAction
  in  mkGenerator gen filterTemp

genAction :: Filter DPExample ([Char],Int) ([Char],Int) s 
          -> ReadChannel ([Char],Int)
          -> WriteChannel ([Char],Int)
          -> DP s ()
genAction filter' cin cout = do
  let unfoldFilter = mkUnfoldFilter makeFilter (decideIfPrint cout) filter' iniFilter cin HNil 
  void $ unfoldF unfoldFilter

makeFilter:: ([Char],Int) -> Bool
makeFilter (".",_) = False  
makeFilter (_,0) = True
makeFilter _ = False

decideIfPrint :: WriteChannel ([Char],Int) -> (([Char],Int) -> DP s ())
decideIfPrint c (".",_) = push ("FIN DE FRASE",0) c
decideIfPrint _ (_,0) = return ()
decideIfPrint c a = push a c

iniFilter :: ([Char],Int) -> ([Char],Int)
iniFilter (s,_) = (s,1)

--FILTER--
filterTemp :: Filter DPExample ([Char],Int) ([Char],Int) s 
filterTemp = mkFilter actorRepeted

actorRepeted :: ([Char],Int)
             -> ReadChannel ([Char],Int)
             -> WriteChannel ([Char],Int)
             -> StateT ([Char],Int) (DP s) ()
actorRepeted (par,x) rc wc = foldM_ rc $ \(inp, y) -> if inp == "." 
                                                        then get >>= flip push wc >> push (".",0) wc 
                                                        else if inp == par
                                                              then get >>= \(stat,z) -> put (stat,z + 1)
                                                              else push (inp, y) wc
--SINK--
sink' :: Stage (ReadChannel ([Char],Int) -> DP s ())
sink' = withSink @DPExample $ flip foldM_ print

main :: IO ()
main = runDP $ mkDP @DPExample source' generator' sink'