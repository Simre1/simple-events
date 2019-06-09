module SwitchingExample where

import SimpleEvents
import Text.Read
import Control.Monad
import Control.Arrow

switchingExample :: IO ()
switchingExample = do
  (c1Event, c1Trigger) <- first (fmap ("Channel 1: " ++)) <$> newEvent
  (c2Event, c2Trigger) <- first (fmap ("Channel 2: " ++)) <$> newEvent
  (personSaidEvent, chooseChannelTrigger) <- newDynamic c1Event

  reactimate (switchEvents personSaidEvent) $
    simpleEventHandler putStrLn

  putStrLn "Write \"quit\" to exit and \"Channel 1\" or \"Channel 2\" to switch channels."

  let gatherInput = do
        input <- getLine
        case input of
          "quit" -> pure ()
          "Channel 1" -> triggerEvent chooseChannelTrigger c1Event >> putStrLn "You are now on Channel 1" >> gatherInput
          "Channel 2" -> triggerEvent chooseChannelTrigger c2Event >> putStrLn "You are now on Channel 2" >> gatherInput
          str -> triggerEvent (c1Trigger <> c2Trigger) str >> gatherInput
  gatherInput
