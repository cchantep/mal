module Main

import Step0_repl

main : IO ()
main = putStrLn "*** Welcome to IDRIS MAL ***\r\n" >>= (\_ => rep)