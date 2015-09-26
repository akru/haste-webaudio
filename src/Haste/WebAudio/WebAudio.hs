{-# LANGUAGE DeriveFunctor #-}
module Haste.WebAudio.WebAudio
  ( WebAudio(..), runWebAudio ) where

import Haste.WebAudio.Internal
import Control.Monad.IO.Class

-- | Web Audio is a monad
newtype WebAudio a = WebAudio { unWA :: Ctx -> IO a }
  deriving Functor

instance Applicative WebAudio where
    pure x = WebAudio (\_ -> return x)
    (WebAudio f) <*> (WebAudio a) = WebAudio (\c -> f c <*> a c)

instance Monad WebAudio where
    (WebAudio a) >>= f = WebAudio (\c -> do { x <- a c; unWA (f x) c })
    (WebAudio a) >> (WebAudio b) = WebAudio (\c -> a c >> b c)

instance MonadIO WebAudio where
    liftIO x = WebAudio (\_ -> x)

runWebAudio :: WebAudio a -> IO a
runWebAudio (WebAudio w) = jsAudioContext >>= w 
