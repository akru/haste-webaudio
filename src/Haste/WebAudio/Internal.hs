{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Haste.WebAudio.Internal where

import Haste.Foreign

-- | Web Audio context
newtype Ctx = Ctx JSAny
  deriving (ToAny, FromAny)

-- | Web Audio is a monad
newtype WebAudio a = WebAudio { unWA :: Ctx -> IO a }

jsAudioContext :: IO Ctx
jsAudioContext = ffi "(function(){window.AudioContext = window.AudioContext||window.webkitAudioContext;return new AudioContext()})"

jsConnectNode :: (ToAny a, ToAny b) => a -> b -> IO ()
jsConnectNode = ffi "(function(a,b){a.connect(b)})"

jsDisconnectNode :: ToAny a => a -> IO ()
jsDisconnectNode = ffi "(function(a){a.disconnect()})"

jsNumberOfInputsNode :: ToAny a => a -> IO Int
jsNumberOfInputsNode = ffi "(function(a){return a.numberOfInputs})"

jsNumberOfOutputsNode :: ToAny a => a -> IO Int
jsNumberOfOutputsNode = ffi "(function(a){return a.numberOfOutputs})"

newtype StdDestination = StdDestination JSAny
  deriving (ToAny, FromAny)

jsStdDestionationNode :: Ctx -> IO StdDestination
jsStdDestionationNode = ffi "(function(g){return g.destination})"

newtype Oscillator = Oscillator JSAny
  deriving (ToAny, FromAny)

jsOscillatorNode :: Ctx -> IO Oscillator
jsOscillatorNode = ffi "(function(g){return g.createOscillator()})"

newtype Gain = Gain JSAny
  deriving (ToAny, FromAny)

jsGainNode :: Ctx -> IO Gain
jsGainNode = ffi "(function(g){return g.createGain()})"

newtype Delay = Delay JSAny
  deriving (ToAny, FromAny)

jsDelayNode :: Ctx -> IO Delay
jsDelayNode = ffi "(function(g){return g.createDelay()})"

newtype Convolution = Convolution JSAny
  deriving (ToAny, FromAny)

jsConvolutionNode :: Ctx -> IO Convolution
jsConvolutionNode = ffi "(function(g){return g.createConvolver()})"

newtype Analyser = Analyser JSAny
  deriving (ToAny, FromAny)

jsAnalyserNode :: Ctx -> IO Analyser
jsAnalyserNode = ffi "(function(g){return g.createAnalyser()})"

jsNodeStart :: ToAny a => a -> Int -> IO ()
jsNodeStart = ffi "(function(a,b){a.start(b)})"

jsNodeStop :: ToAny a => a -> IO ()
jsNodeStop = ffi "(function(a){a.stop()})"

jsOscillatorFrequency :: Oscillator -> Int -> IO ()
jsOscillatorFrequency = ffi "(function(a,b){a.frequency.value=b})"

jsOscillatorType :: Oscillator -> String -> IO ()
jsOscillatorType = ffi "(function(a,b){a.type=b})"

jsGainValue :: Gain -> Double -> IO ()
jsGainValue = ffi "(function(a,b){a.gain.value=b})"

jsDelayValue :: Delay -> Double -> IO ()
jsDelayValue = ffi "(function(a,b){a.delayTime.value=b})"

jsAnalyserFFTSize :: Analyser -> Int -> IO ()
jsAnalyserFFTSize = ffi "(function(a,b){a.fftSize=b})"

jsGetByteFrequencyData :: FromAny a => Analyser -> IO a
jsGetByteFrequencyData = ffi "(function(a){var d=new Uint8Array(a.frequencyBinCount);a.getByteFrequencyData(d);return d})"

jsGetFloatFrequencyData :: FromAny a => Analyser -> IO a
jsGetFloatFrequencyData = ffi "(function(a){var d=new Float32Array(a.frequencyBinCount);a.getFloatFrequencyData(d);return d})"

jsGetByteTimeDomainData :: FromAny a => Analyser -> IO a
jsGetByteTimeDomainData = ffi "(function(a){var d=new Uint8Array(a.frequencyBinCount);a.getByteTimeDomainData(d);return d})"

jsGetFloatTimeDomainData :: FromAny a => Analyser -> IO a
jsGetFloatTimeDomainData = ffi "(function(a){var d=new Float32Array(a.frequencyBinCount);a.getFloatTimeDomainData(d);return d})"
