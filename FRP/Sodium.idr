module FRP.Sodium

import Data.IORef
import FRP.Sodium.Reactive
import FRP.Sodium.Vertex
import public FRP.Sodium.Event

import Debug.Trace


export
test : IO ()
test = do runStateT r ini
          pure ()
  where r : Reactive ()
        r = do
          ints <- newEvent {a=Integer}
          let incs = map (+ 1) ints
          listen incs (lift . printLn)
          let evens = filter (\v => (v `mod` 2) == 0) incs
          listen evens (lift . printLn)
          lift $ printLn "EVENTS"
          printEvent ints
          printEvent incs
          printEvent evens
          lift $ printLn "SEND"
          send ints 24
          send ints 41
        ini = MkReactiveState False [] [] [] 0

test2 : IO ()
test2 = do
  ref <- newIORef 2
  writeIORef ref 4
  printLn !(readIORef ref)
