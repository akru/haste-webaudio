module Haste.WebAudio.Node 
  ( AudioNode, node, connect
  , start, startFrom, stop
  , StdDestination
  , Oscillator, OscillatorType(..)
  , setFreq, setType
  , Analyser, setFFTSize
  , FrequencyData, getFrequencyData
  , TimeDomainData, getTimeDomainData
  , Gain, Delay, Convolution
  ) where

import Haste.WebAudio.Internal
import Haste.WebAudio.WebAudio
import Control.Monad.IO.Class (liftIO)
import Haste.Foreign (ToAny)
import Data.Char (toLower)
import Data.Int (Int8)

-- | Web Audio Node type class
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

instance AudioNode StdDestination where
    node = WebAudio jsStdDestionationNode

data OscillatorType = Sine | Square | Sawtooth | Triangle
  deriving (Show, Eq, Enum)

instance AudioNode Oscillator where
    node = WebAudio jsOscillatorNode

instance AudioNode Gain where
    node = WebAudio jsGainNode

instance AudioNode Delay where
    node = WebAudio jsDelayNode

instance AudioNode Convolution where
    node = WebAudio jsConvolutionNode

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
