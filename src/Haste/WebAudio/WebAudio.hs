module Haste.WebAudio.WebAudio
  ( WebAudio, runWebAudio ) where

import Haste.WebAudio.Internal (WebAudio(..), jsAudioContext)
import Control.Monad.IO.Class (MonadIO, liftIO)

instance Functor WebAudio where
    fmap f (WebAudio a) = WebAudio (fmap f . a)

instance Applicative WebAudio where
    pure = WebAudio . const . pure
    (WebAudio f) <*> (WebAudio a) = WebAudio $ (<*>) <$> f <*> a

instance Monad WebAudio where
    (WebAudio a) >>= f = WebAudio $ (>>=) <$> a <*> flip (unWA . f)
    (WebAudio a) >> (WebAudio b) = WebAudio $ (>>) <$> a <*> b

instance MonadIO WebAudio where
    liftIO = WebAudio . const

runWebAudio :: MonadIO m => WebAudio a -> m a
runWebAudio (WebAudio a) = liftIO $ jsAudioContext >>= a
