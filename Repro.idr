import Debug.Trace

mutual
  record Foo where
    constructor MkFoo
    fee : Integer
    bar : Bar

  record Bar where
    constructor MkBar
    foo : Foo

mkFoo : Integer -> Bar -> Foo
mkFoo = trace "mkFoo" $ MkFoo

mkBar : Foo -> Bar
mkBar = trace "mkBar" $ MkBar

knot : Integer -> Bar
knot i = b
  where
    mutual
      f : Foo
      f = mkFoo i b
      b : Bar
      b = mkBar f

main : IO ()
main = do
  let b = knot 10
  printLn $ show $ fee $ foo $ b
