module EventExample where

import SimpleEvents

eventExample :: IO ()
eventExample = do
  (inputEvent, inputTrigger) <- newEvent

  reactimate (("You wrote: " ++) <$> inputEvent) $
    simpleEventHandler putStrLn

  putStrLn "Try to write something!"
  putStrLn "PS: \"quit\" halts the example."

  let gatherInput = do
        t <- getLine
        case t of
          "quit" -> mempty
          str -> triggerEvent inputTrigger str >> gatherInput

  gatherInput
