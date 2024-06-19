--Main with file input and file output, but counting characets, for large files.
--With time execution
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
import Data.Time

--Aixo es la definicio del typus de la pipeline
type DPExample = Source (Channel ( (ByteString,Int) :<+> Eof)) :=> Generator (Channel ((ByteString,Int):<+> Eof)) :=> Sink

input :: [Char]
input = "test5.txt"
--input = "salida.txt"

output :: [Char]
output = "output.txt"

--SOURCE--
--Aixo es la definicio de la font de la pipeline, utilitza el combinador per generar una font
source' :: Stage (WriteChannel (ByteString,Int) -> DP s ())
source' = withSource @DPExample $ \cout -> unfoldFile' input cout convert

unfoldFile':: FilePath -> WriteChannel (ByteString,Int) -> (ByteString -> (ByteString,Int)) -> DP s ()
unfoldFile' file writeChannel fn =
  liftIO $ R.withFile file ReadMode $ \h -> unfoldM (hGetchar h) fn (H.hIsEOF h) writeChannel

hGetchar :: Handle -> IO ByteString
hGetchar h = do
  c <- hGet h 1
  if c == "" || c == "." || "a" <= c && c <= "z" 
    then return c 
    else hGetchar h

convert :: ByteString -> (ByteString,Int)
convert bs = (bs,0)

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
decideIfPrint c (".",_) = push (".",0) c
decideIfPrint _ (_,0) = return ()
decideIfPrint c a = push a c

iniFilter :: (ByteString,Int) -> (ByteString,Int)
iniFilter (s,_) = (s,1)

--FILTER--
filterTemp :: Filter DPExample (ByteString,Int) (ByteString,Int) s 
--filterTemp = mkFilter actorRepeted
filterTemp = actor actor2 |>>  actor actor1

actor1 :: (ByteString,Int)
          -> ReadChannel (ByteString,Int)
          -> WriteChannel (ByteString,Int)
          -> StateT (ByteString,Int) (DP s) ()
actor1 (par,x) rc wc = foldM_ rc $ \(inp, y) -> if inp == "." then 
                                                  get >>= flip push wc >> push (".",0) wc
                                                else 
                                                  pure ()

actor2 :: (ByteString,Int)
          -> ReadChannel (ByteString,Int)
          -> WriteChannel (ByteString,Int)
          -> StateT (ByteString,Int) (DP s) ()
actor2 (par,x) rc wc = foldM_ rc $ \(inp, y) -> if inp == par then 
                                                  get >>= \(stat,z) -> put (stat,z + 1)
                                                else 
                                                  push (inp, y) wc    

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
sink' = withSink @DPExample $ flip foldM_ (R.appendFileText output . showPair)

showPair :: (ByteString,Int) -> Text
showPair (".",x) = "*******************************\n"
showPair (s,x) = decodeUtf8 $ B.concat [s, ": ",  fromString (show x), "\n"]

dp' :: DP s ()
dp' = mkDP @DPExample source' generator' sink'

program :: IO ()
program = runDP dp'

timeFunction :: IO () -> IO ()
timeFunction function = do
   startTime <- getCurrentTime
   function
   endTime <- getCurrentTime
   let diff = diffUTCTime endTime startTime
   R.putStrLn $ "Execution Time: " ++ (show diff)

main :: IO ()
main = timeFunction program