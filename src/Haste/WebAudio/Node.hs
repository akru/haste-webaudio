module Haste.WebAudio.Node 
  ( Node, node, connect
  , node2, node3, node4, node5
  , start, startFrom, stop
  , StdDestination
  , Oscillator, OscillatorType(..)
  , setFreq, getFreq, setType
  , Gain, setGain, getGain
  , Delay, setDelay
  , Analyser, setFFTSize
  , FrequencyData, getFrequencyData
  , TimeDomainData, getTimeDomainData
  , Convolution
  ) where

import Haste.WebAudio.Internal
import Haste.WebAudio.WebAudio ()
import Control.Monad.IO.Class (liftIO)
import Haste.Foreign (ToAny)
import Data.Char (toLower)
import Data.Int (Int8)

-- | Web Audio Node type class
class ToAny a => Node a where
    -- | Create new audio node
    node :: WebAudio a
    -- | Connects two nodes
    connect :: Node b => a -> b -> WebAudio ()
    connect = (liftIO .) . jsConnectNode 
    -- | Disconnects an argument node
    disconnect :: a -> WebAudio ()
    disconnect = liftIO . jsDisconnectNode
    -- | I/O information
    numberOfInputs  :: a -> WebAudio Int
    numberOfInputs  = liftIO . jsNumberOfInputsNode
    numberOfOutputs :: a -> WebAudio Int
    numberOfOutputs = liftIO . jsNumberOfOutputsNode 

instance Node StdDestination where
    node = WebAudio jsStdDestionationNode

data OscillatorType = Sine | Square | Sawtooth | Triangle
  deriving (Show, Eq, Enum)

instance Node Oscillator where
    node = WebAudio jsOscillatorNode

instance Node Gain where
    node = WebAudio jsGainNode

instance Node Delay where
    node = WebAudio jsDelayNode

instance Node Convolution where
    node = WebAudio jsConvolutionNode

instance Node Analyser where
    node = WebAudio jsAnalyserNode

-- | Node generators
node2 :: (Node t1, Node t2) => WebAudio (t1, t2)
node2 = (,) <$> node <*> node

node3 :: (Node t1, Node t2, Node t3)
      => WebAudio (t1, t2, t3)
node3 = (,,) <$> node <*> node <*> node

node4 :: (Node t1, Node t2, Node t3, Node t4)
      => WebAudio (t1, t2, t3, t4)
node4 = (,,,) <$> node <*> node <*> node <*> node

node5 :: (Node t1, Node t2, Node t3, Node t4, Node t5)
      => WebAudio (t1, t2, t3, t4, t5)
node5 = (,,,,) <$> node <*> node <*> node <*> node <*> node

-- | Source node type class
class Node a => Source a where
    startFrom :: a -> Int -> WebAudio ()
    startFrom = (liftIO .) . jsNodeStart

    start :: a -> WebAudio ()
    start = flip startFrom 0

    stop :: a -> WebAudio ()
    stop = liftIO . jsNodeStop

instance Source Oscillator

--
-- | Node modificators
--
-- | Oscillator
setFreq :: Oscillator -> Double -> WebAudio ()
setFreq = (liftIO .) . jsOscillatorFrequency

getFreq :: Oscillator -> WebAudio Double
getFreq = liftIO . jsOscillatorFrequencyGet

setType :: Oscillator -> OscillatorType -> WebAudio ()
setType o = liftIO . jsOscillatorType o . toLower' . show 
  where toLower' (x : xs) = toLower x : xs

-- | Gain
setGain :: Gain -> Double -> WebAudio ()
setGain = (liftIO .) . jsGainValue

getGain :: Gain -> WebAudio Double
getGain = liftIO . jsGainValueGet

-- | Delay
setDelay :: Delay -> Double -> WebAudio ()
setDelay = (liftIO .) . jsDelayValue

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
