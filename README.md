# SimpleEvents

**Status: Experimental**

SimpleEvents is a very lightweight library for defining event networks with the only dependencies being base and containers.

Here is a short example if one wishes to try out this library:

```haskell
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
```
