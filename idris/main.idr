module Main

import Step1_read_print

main : IO ()
main = putStrLn "*** Welcome to IDRIS MAL ***\r\n" >>= (\_ => step1_read_print)
