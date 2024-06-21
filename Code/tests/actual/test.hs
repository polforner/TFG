{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Concurrent.Async
import           Control.Lens             hiding ((<|))
import           Data.HList
import           Data.List.NonEmpty
import           DynamicPipeline.Channel hiding (mapM_)
import           DynamicPipeline.Flow
import           GHC.TypeLits
import Relude

type family WithFilter (dpDefinition :: Type) (param :: Type) (monadicAction :: Type -> Type) :: Type where
  WithFilter (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> Sink) param monadicAction
                                                    = param -> WithFilter (ChanOutIn inToGen genToOut) param monadicAction
  WithFilter (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> FeedbackChannel toSource  :=> Sink) param monadicAction
                                                    = param -> WithFilter (ChanOutIn inToGen (ChanInIn genToOut toSource)) param monadicAction
  WithFilter (ChanInIn (a :<+> more) ins) param monadicAction       
                                                    = WriteChannel a -> WithFilter (ChanInIn more ins) param monadicAction
  WithFilter (ChanInIn Eof ins) param monadicAction                 
                                                    = WithFilter (ChanIn ins) param monadicAction
  WithFilter (ChanIn (dpDefinition :<+> more)) param monadicAction         
                                                    = WriteChannel dpDefinition -> WithFilter (ChanIn more) param monadicAction
  WithFilter (ChanIn Eof) param monadicAction       = monadicAction ()
  WithFilter (ChanOutIn (dpDefinition :<+> more) ins) param monadicAction  
                                                    = ReadChannel dpDefinition -> WithFilter (ChanOutIn more ins) param monadicAction
  WithFilter (ChanOutIn Eof (ChanInIn ins ins2)) param monadicAction 
                                                    = WithFilter (ChanInIn ins ins2) param monadicAction
  WithFilter (ChanOutIn Eof ins) param m            = WithFilter (ChanIn ins) param m
  WithFilter dpDefinition _ _                       = TypeError
                                                        ( 'Text "Invalid Semantic Semantic for Generator Stage"
                                                          ':$$: 'Text "in the DP Definition '"
                                                          ':<>: 'ShowType dpDefinition
                                                          ':<>: 'Text "'"
                                                          ':$$: 'Text "Language Grammar:"
                                                          ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> Sink"
                                                          ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> FEEDBACK :=> Sink"
                                                          ':$$: 'Text "CHANS    -> Channel CH"
                                                          ':$$: 'Text "FEEDBACK -> FeedbackChannel CH"
                                                          ':$$: 'Text "CH       -> Type :<+> CH | Eof"
                                                          ':$$: 'Text "Example: 'Source (Channel (Int :<+> Int)) :=> Generator (Channel (Int :<+> Int)) :=> Sink'"
                                                        )

newtype DP st a = DP
  { runStage :: IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

runDP :: forall {k} (st :: k) a. DP st a -> IO a
runDP = runStage

data Stage a where
  Stage :: Proxy a -> a -> Stage a

mkStage :: forall a. Proxy a -> a -> Stage a
mkStage = Stage @a


mkStage' :: forall a. a -> Stage a
mkStage' = Stage (Proxy @a)


newtype Actor dpDefinition filterState filterParam monadicAction =
  Actor {  unActor :: Stage (WithFilter dpDefinition filterParam monadicAction) }

newtype Filter dpDefinition filterState filterParam st =
  Filter { unFilter :: NonEmpty (Actor dpDefinition filterState filterParam (Relude.StateT filterState (DP st))) }
  deriving Generic

actor :: forall dpDefinition filterParam filterState st.
         WithFilter dpDefinition filterParam (StateT filterState (DP st))
         -> Actor dpDefinition filterState filterParam (StateT filterState (DP st))
actor = Actor . mkStage' @(WithFilter dpDefinition filterParam (StateT filterState (DP st)))


data UnFoldFilter dpDefinition readElem st filterState filterParam l = 
  UnFoldFilter 
    { _ufSpawnIf :: readElem -> Bool -- ^Given a new Element determine if we need to interpose a new Filter or not
    , _ufOnElem :: readElem -> DP st () -- ^For each element that the Filter is consuming allow to do something outside the filter with that element. For example trace or debug
    , _ufFilter :: Filter dpDefinition filterState filterParam st  -- ^'Filter' Template
    , _ufInitState :: readElem -> filterState -- ^Given the First element in this Filter Instance how to Initiate Internal 'Filter' 'StateT' (Memory)
    , _ufReadChannel :: ReadChannel readElem -- ^Main 'ReadChannel' to feed filter
    , _ufRsChannels :: HList l -- ^'HList' with the rest of the ReadChannels if There are needed or 'HNil' if it only contians 1 read channel
    }
    

unfoldF = loopSpawn
  where
    loopSpawn uf@UnFoldFilter{..} =
      maybe (pure _ufRsChannels) (loopSpawn <=< doOnElem uf) =<< DP (pull _ufReadChannel)

    doOnElem uf@UnFoldFilter{..} elem' = do
      _ufOnElem elem'
      if _ufSpawnIf elem'
        then do
          (reads', writes' :: HList l3) <- getFilterChannels <$> DP (makeChansF @(ChansFilter dpDefinition))
          let hlist = elem' .*. _ufReadChannel .*. (_ufRsChannels `hAppendList` writes')
          void $ runFilter _ufFilter (_ufInitState elem') hlist (_ufReadChannel .*. (_ufRsChannels `hAppendList` writes'))
          return $ uf { _ufReadChannel = hHead reads', _ufRsChannels = hTail reads' }
        else return uf

