{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright : (c) NASA, 2024-2025
-- License   : BSD-style (see the LICENSE file in the distribution)
--
-- Run a Copilot simulation live and allow interacting with it using a
-- websocket.
--
-- This visualizer enables adding new streams to a visualization. To do so, the
-- visualizer needs access to the original spec, and needs to be able to
-- interpret new expressions in the same context as the prior expressions.
--
-- An example of a spec that can be passed as argument to the visualizer
-- follows:
--
-- @
--   spec :: String
--   spec = unlines
--     [ "let temperature :: Stream Word8"
--     , "    temperature = extern \"temperature\" (Just [0, 15, 20, 25, 30])"
--     , ""
--     , "    ctemp :: Stream Float"
--     , "    ctemp = (unsafeCast temperature) * (150.0 / 255.0) - 50.0"
--     , ""
--     , "    trueFalse :: Stream Bool"
--     , "    trueFalse = [True] ++ not trueFalse"
--     , ""
--     , "in do trigger \"heaton\"  (temperature < 18) [arg ctemp, arg (constI16 1), arg trueFalse]"
--     , "      trigger \"heatoff\" (temperature > 21) [arg (constI16 1), arg ctemp]"
--     , "      observer \"temperature\" temperature"
--     , "      observer \"temperature2\" (temperature + 1)"
--     ]
-- @
--
-- The imports are predefined.
module Copilot.Visualize.Live
    ( visualize
    , visualizeWith
    , VisualSettings(..)
    , mkDefaultVisualSettings
    , SimulationSettings(..)
    , mkDefaultSimulationSettings
    )
  where

-- External imports
import           Control.Exception  (SomeException (..), handle)
import           Data.Aeson         (ToJSON (..), encode)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS
import           Prelude            hiding (div, not, (++), (<), (>))
import qualified Prelude
import           Text.Read          (readMaybe)

-- External imports: Copilot
import qualified Copilot.Core                   as Core
import           Copilot.Interpret.Eval         (ShowType (Haskell), eval)
import           Copilot.Language               hiding (interpret, typeOf)
import           Copilot.Visualize.UntypedTrace (AppData, makeTraceEval)

-- Internal imports
import Copilot.Visualize.Dynamic

-- | Start a simulation for an input spec, listening for commands and
-- communicating the status via a websocket.
visualize :: String -> IO ()
visualize = visualizeWith mkDefaultVisualSettings

-- | Start a simulation for an input spec, listening for commands and
-- communicating the status via a websocket.
visualizeWith :: VisualSettings -> String -> IO ()
visualizeWith settings spec = do
  putStrLn $ concat
    [ "WebSocket server starting on port "
    , show (visualSettingsPort settings)
    , "..."
    ]
  WS.runServer
    (visualSettingsHost settings)
    (visualSettingsPort settings)
    (app settings spec)

-- | Settings used to customize the simulation and interaction.
data VisualSettings = VisualSettings
  { visualSettingsHost       :: String
                                -- ^ Host interface to listen to. Use
                                -- "127.0.0.1" to listen at localhost.

  , visualSettingsPort       :: Int
                                -- ^ Port to listen to.

  , visualSettingsSimulation :: SimulationSettings
                                -- ^ Settings for the simulation.
  }

-- | Default settings that simulates 3 steps and listens on localhost at port
-- 9160.
mkDefaultVisualSettings :: VisualSettings
mkDefaultVisualSettings = VisualSettings
  { visualSettingsHost       = "127.0.0.1"
  , visualSettingsPort       = 9160
  , visualSettingsSimulation = mkDefaultSimulationSettings
  }

-- * Server

-- | Server application using web sockets.
app :: VisualSettings -> String -> WS.ServerApp
app settings spec pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ appInit settings spec conn

-- | Initialize the backend.
appInit :: VisualSettings -> String -> WS.Connection -> IO ()
appInit settings spec conn = handle appException $ do
    -- Initialize the simulation.
    simData  <- simInit (visualSettingsSimulation settings) spec

    -- Communicate the current values of the trace, in JSON, via the web
    -- socket.
    let appData = mkAppData (simSteps simData) (simSpec simData)
    let samples = encode $ toJSON appData
    WS.sendTextData conn samples

    -- Start the application loop.
    appMainLoop settings conn simData

  where

    appException :: SomeException -> IO ()
    appException e = do
      putStrLn $ "Error:" Prelude.++ show e
      error $ show e

-- | Run the main application, repeatedly reading commands from a web socket
-- and returning results via the same web socket.
appMainLoop :: VisualSettings
            -> WS.Connection
            -> SimData
            -> IO ()
appMainLoop settings conn simData = do
  -- Read a command from the web socket.
  cmdM <- readMaybe . T.unpack <$> WS.receiveData conn

  -- Run a simulation step, if a command has been received.
  let simulationSettings = visualSettingsSimulation settings
  simData' <- maybe (pure simData) (simStep simulationSettings simData) cmdM

  -- Communicate the current values of the trace, in JSON, via the web socket.
  let appData = mkAppData (simSteps simData') (simSpec simData')
      samples = encode $ toJSON appData
  WS.sendTextData conn samples

  appMainLoop settings conn simData'

-- * Auxiliary functions

-- | Obtain the trace data from a Copilot spec for a number of steps.
mkAppData :: Int -> Core.Spec -> AppData
mkAppData numSteps spec' =
  makeTraceEval numSteps spec' (eval Haskell numSteps spec')
