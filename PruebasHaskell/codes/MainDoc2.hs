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
type FilterPar = Pair
--Aixo es la definicio del typus de la pipeline
type DPExample = Source (Channel (Pair :<+> Eof)) :=> Generator (Channel (Pair :<+> Eof)) :=> Sink

input :: [Word]
<<<<<<< HEAD:PruebasHaskell/codes/MainDoc2.hs
input = ["dog", "cat",".","dog","dog","dog",".","bird","cat","."]
=======
input = ["hola","que","tal","hola",".","aixo","es","que","tal",".","hola","hola","hola","hola","."]
--input = ["hola","."]
>>>>>>> 74db42ce31a2a93cf11661eae719d5bded94ad41:Pruebas Haskell/MainDoc2.hs

source' :: Stage (WriteChannel Pair -> DP s ())
source' = withSource @DPExample fillChannels

fillChannels :: WriteChannel Pair
            -> DP st ()
fillChannels wp = unfoldT input wp $ \w -> (w,0)

--GENERATOR--
generator' :: GeneratorStage DPExample FilterState FilterPar s
generator' =
  let gen = withGenerator @DPExample genAction
  in  mkGenerator gen filterTemp

genAction :: Filter DPExample FilterState FilterPar s
          -> ReadChannel Pair
          -> WriteChannel Pair
          -> DP s ()
genAction filter' rp wp =
  let unfoldFilter = mkUnfoldFilter create' (decideIfPrint wp) filter' iniFilter rp HNil
  in void $ unfoldF unfoldFilter

create' :: Pair-> Bool
create' (".",_) = False
create' (_,0) = True
create' _ = False

decideIfPrint :: WriteChannel Pair -> Pair -> DP s ()
<<<<<<< HEAD:PruebasHaskell/codes/MainDoc2.hs
decideIfPrint c (".",_) = push (".",0) c
=======
decideIfPrint c (".",_) = push ("FIN DE FRASE",0) c
>>>>>>> 74db42ce31a2a93cf11661eae719d5bded94ad41:Pruebas Haskell/MainDoc2.hs
decideIfPrint _ (_,0) = return ()
decideIfPrint c a = push a c

iniFilter :: Pair -> FilterState
iniFilter _ = 1

filterTemp :: Filter DPExample FilterState FilterPar s 
filterTemp = mkFilter actor1

actor1 :: FilterPar
        -> ReadChannel Pair
        -> WriteChannel Pair
        -> StateT FilterState (DP s) ()
actor1 (par,_) rp wp =
  foldM_ rp $ \(inp,y) -> if inp == "." 
                          --then get >>= \x -> push (par,x) wp >> push (".",0) wp
                          then do 
                            pushState wp $ \x->(par,x)
                            push (".",0) wp
                          else  if inp == par
                                then modify (+1)
                                else push (inp,y) wp

pushState :: WriteChannel a -> (b -> a) -> StateT b (DP s) ()
pushState wp f = get >>= flip push wp . f

--SINK--
sink' :: Stage (ReadChannel Pair -> DP s ())
sink' = withSink @DPExample readChannels

readChannels ::ReadChannel Pair -> DP st ()
<<<<<<< HEAD:PruebasHaskell/codes/MainDoc2.hs
readChannels rp = foldM_ rp printResults

printResults :: Pair -> DP st ()
printResults (".",_) = print "**********************"
printResults (w,n) = print $ w ++ " " ++ show n
=======
readChannels rp = foldM_ rp print
>>>>>>> 74db42ce31a2a93cf11661eae719d5bded94ad41:Pruebas Haskell/MainDoc2.hs

main :: IO ()
main = runDP $ mkDP @DPExample source' generator' sink'