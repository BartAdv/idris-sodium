import FRP.Sodium

import Debug.Trace

-- acc : Event a -> a -> (a -> a -> a) -> Cell a
-- acc delta v0 f = trace "acc" $ value
--   where
--     mutual
--       update : Event a
--       update = snapshot delta value f
--       value : Cell a
--       value = hold update v0

-- accumulator : Reactive ()
-- accumulator = do
--   lift $ printLn "accumulator"
--   delta <- newEvent {a=Integer}
--   lift $ printLn "making acc"
--   let acc' = acc delta 0 (+)
--   lift $ print "Initial sample: "
--   iv <- trace "sampling" $ sample acc'
--   lift $ printLn $ show iv
--   lift $ printLn "sending deltas..."
--   traverse_ (send delta) [1,1,-1,-1,1,1]
--   lift $ print "Acc: "
--   v <- trace "sampling" $ sample acc'
--   lift $ printLn $ show v

testHold : Reactive ()
testHold = do
  lift $ printLn "hold"
  ev <- newEvent {a=Integer}
  cell <- hold ev 1
  lift . printLn $ "Initial sample: " ++ show !(sample cell)
  send ev 2
  lift . printLn $ "Sample: " ++ show !(sample cell)

evTest : Reactive ()
evTest = do
  ints <- newEvent {a=Integer}
  let incs = map (+ 1) ints
  lift $ printLn "listen incs"
  listen incs (\v => lift . printLn $ "inc: " ++ show v)
  let evens = filter (\v => (v `mod` 2) == 0) incs
  lift $ printLn "listen evens"
  listen evens (\v => lift . printLn $ "even: " ++ show v)
  lift $ printLn "EVENTS"
  printEvent ints
  printEvent incs
  printEvent evens
  traverse_ (\v => do lift . printLn $ "Send " ++ show v
                      send ints v) [24, 25, 41]

export
main : IO ()
main = do runStateT testHold ini
          pure ()
       where
        ini = MkReactiveState False [] [] [] 0
