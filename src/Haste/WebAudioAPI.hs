{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
module Haste.WebAudioAPI
  ( WebAudio, runWebAudio
  , AudioNode, node, connect
  , start, startFrom, stop
  , StdDestination
  , Oscillator, OscillatorType(..)
  , setFreq, setType
  , Analyser
  , setFFTSize
  , FrequencyData, getFrequencyData
  , TimeDomainData, getTimeDomainData
  , Gain, Delay, Convolution
  ) where

import Haste.JSString as JS(concat)
import Haste.Foreign
import Haste.Prim

import Control.Monad.IO.Class
import Data.Char (toLower)
import Data.Int (Int8)

-- | Web Audio API context object
newtype Ctx = Ctx JSAny
  deriving (ToAny, FromAny)
--
-- | Web Audio monad
--
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

--
-- | Web audio nodes
--
class ToAny a => AudioNode a where
    -- | Create new audio node
    node :: WebAudio a
    -- | Connect source node to destionation node 
    connect :: AudioNode b => a -> b -> WebAudio ()
    connect = (liftIO .) . jsConnectNode 
    -- | Disconnect node
    disconnect :: a -> WebAudio ()
    disconnect = liftIO . jsDisconnectNode
    -- | I/O information
    numberOfInputs  :: a -> WebAudio Int
    numberOfInputs  = liftIO . jsNumberOfInputsNode
    numberOfOutputs :: a -> WebAudio Int
    numberOfOutputs = liftIO . jsNumberOfOutputsNode 

newtype StdDestination = StdDestination JSAny
  deriving (ToAny, FromAny)

instance AudioNode StdDestination where
    node = WebAudio jsStdDestionationNode

newtype Oscillator = Oscillator JSAny
  deriving (ToAny, FromAny)

data OscillatorType = Sine | Square | Sawtooth | Triangle
  deriving (Show, Eq, Enum)

instance AudioNode Oscillator where
    node = WebAudio jsOscillatorNode

newtype Gain = Gain JSAny
  deriving (ToAny, FromAny)

instance AudioNode Gain where
    node = WebAudio jsGainNode

newtype Delay = Delay JSAny
  deriving (ToAny, FromAny)

instance AudioNode Delay where
    node = WebAudio jsDelayNode

newtype Convolution = Convolution JSAny
  deriving (ToAny, FromAny)

instance AudioNode Convolution where
    node = WebAudio jsConvolutionNode

newtype Analyser = Analyser JSAny
  deriving (ToAny, FromAny)

instance AudioNode Analyser where
    node = WebAudio jsAnalyserNode

--
-- | Node modificators
--
start :: AudioNode a => a -> WebAudio ()
start = flip startFrom 0

startFrom :: AudioNode a => a -> Int -> WebAudio ()
startFrom = (liftIO .) . jsNodeStart

stop :: AudioNode a => a -> WebAudio ()
stop = liftIO . jsNodeStop

-- | Oscillator
setFreq :: Oscillator -> Int -> WebAudio ()
setFreq = (liftIO .) . jsOscillatorFrequency

setType :: Oscillator -> OscillatorType -> WebAudio ()
setType o = liftIO . jsOscillatorType o . toLower' . show 
  where toLower' (x : xs) = toLower x : xs

-- | Analyser
setFFTSize :: Analyser -> Int -> WebAudio ()
setFFTSize = (liftIO .) . jsAnalyserFFTSize

class FrequencyData a where
    getFrequencyData :: Analyser -> WebAudio [a]

instance FrequencyData Int8 where
    getFrequencyData = liftIO . jsGetByteFrequencyData

instance FrequencyData Int where
    getFrequencyData = liftIO . jsGetByteFrequencyData

instance FrequencyData Float where
    getFrequencyData = liftIO . jsGetFloatFrequencyData

instance FrequencyData Double where
    getFrequencyData = liftIO . jsGetFloatFrequencyData
    
class TimeDomainData a where
    getTimeDomainData :: Analyser -> WebAudio [a] 

instance TimeDomainData Int8 where
    getTimeDomainData = liftIO . jsGetByteTimeDomainData

instance TimeDomainData Int where
    getTimeDomainData = liftIO . jsGetByteTimeDomainData

instance TimeDomainData Float where
    getTimeDomainData = liftIO . jsGetFloatTimeDomainData

instance TimeDomainData Double where
    getTimeDomainData = liftIO . jsGetFloatTimeDomainData

--
-- | Foreign js bindings
--
jsAudioContext :: FromAny a => IO a
jsAudioContext = ffi "(function(){window.AudioContext = window.AudioContext||window.webkitAudioContext;return new AudioContext()})"

jsConnectNode :: (ToAny a, ToAny b) => a -> b -> IO ()
jsConnectNode = ffi "(function(a,b){a.connect(b)})"

jsDisconnectNode :: ToAny a => a -> IO ()
jsDisconnectNode = ffi "(function(a){a.disconnect()})"

jsNumberOfInputsNode :: ToAny a => a -> IO Int
jsNumberOfInputsNode = ffi "(function(a){return a.numberOfInputs})"

jsNumberOfOutputsNode :: ToAny a => a -> IO Int
jsNumberOfOutputsNode = ffi "(function(a){return a.numberOfOutputs})"

jsStdDestionationNode :: (ToAny a, FromAny b) => a -> IO b
jsStdDestionationNode = ffi "(function(g){return g.destination})"

jsOscillatorNode :: (ToAny a, FromAny b) => a -> IO b
jsOscillatorNode = ffi "(function(g){return g.createOscillator()})"

jsGainNode :: (ToAny a, FromAny b) => a -> IO b
jsGainNode = ffi "(function(g){return g.createGain()})"

jsDelayNode :: (ToAny a, FromAny b) => a -> IO b
jsDelayNode = ffi "(function(g){return g.createDelay()})"

jsConvolutionNode :: (ToAny a, FromAny b) => a -> IO b
jsConvolutionNode = ffi "(function(g){return g.createConvolver()})"

jsAnalyserNode :: (ToAny a, FromAny b) => a -> IO b
jsAnalyserNode = ffi "(function(g){return g.createAnalyser()})"

jsNodeStart :: ToAny a => a -> Int -> IO ()
jsNodeStart = ffi "(function(a,b){a.start(b)})"

jsNodeStop :: ToAny a => a -> IO ()
jsNodeStop = ffi "(function(a){a.stop()})"

jsOscillatorFrequency :: ToAny a => a -> Int -> IO ()
jsOscillatorFrequency = ffi "(function(a,b){a.frequency.value=b})"

jsOscillatorType :: ToAny a => a -> String -> IO ()
jsOscillatorType = ffi "(function(a,b){a.type=b})"

jsAnalyserFFTSize :: ToAny a => a -> Int -> IO ()
jsAnalyserFFTSize = ffi "(function(a,b){a.fftSize=b})"

jsGetByteFrequencyData :: (ToAny a, FromAny b) => a -> IO b
jsGetByteFrequencyData = ffi "(function(a){var d=new Uint8Array(a.frequencyBinCount);a.getByteFrequencyData(d);return d})"

jsGetFloatFrequencyData :: (ToAny a, FromAny b) => a -> IO b
jsGetFloatFrequencyData = ffi "(function(a){var d=new Float32Array(a.frequencyBinCount);a.getFloatFrequencyData(d);return d})"

jsGetByteTimeDomainData :: (ToAny a, FromAny b) => a -> IO b
jsGetByteTimeDomainData = ffi "(function(a){var d=new Uint8Array(a.frequencyBinCount);a.getByteTimeDomainData(d);return d})"

jsGetFloatTimeDomainData :: (ToAny a, FromAny b) => a -> IO b
jsGetFloatTimeDomainData = ffi "(function(a){var d=new Float32Array(a.frequencyBinCount);a.getFloatTimeDomainData(d);return d})"
