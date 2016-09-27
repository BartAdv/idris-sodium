import FRP.Sodium

export
main : IO ()
main = do runStateT r ini
          pure ()
  where r : Reactive ()
        r = do
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
        ini = MkReactiveState False [] [] [] 0
