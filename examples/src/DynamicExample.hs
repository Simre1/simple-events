module DynamicExample where

import SimpleEvents
import Text.Read
import Control.Monad

dynamicExample :: IO ()
dynamicExample = do
  (addNumberEvent, addNumberTrigger) <- newEvent
  sumDynamic <- accumEvents ((+) <$> addNumberEvent) 0

  reactimate (eventFromDynamic sumDynamic) $
    simpleEventHandler $ \sum -> putStrLn $ "The total sum is " ++ show sum ++ "."

  putStrLn "Input some numbers until you have reached 100!"

  let gatherInput = do
        input <- getLine
        case readMaybe input of
          Nothing -> putStrLn "Whoops! That is not a valid number."
          Just num -> triggerEvent addNumberTrigger num
        sum <- readBehavior (behaviorFromDynamic sumDynamic)
        when (sum < 100) gatherInput
  gatherInput
