module Step1_read_print

import Token
import Reader
import Printer

instance Foldable (Either a) where
  foldr f v (Left _)  = v
  foldr f v (Right r) = f r v

public
read : String -> IO (Result Token)
read prompt = do
  _ <- putStr $ prompt
  input <- getLine
  return $ if (input == "") then Left "EOF" else tokenizer input

eval : a -> a
eval a = a

public
print : t -> {default %instance s : Show t} -> IO String
print v = let str = show v in
  putStrLn str >>= (\_ => return str)

private
rep : IO ()
rep = read "user> " >>= (\r =>
  foldl (\_ => \tok => do
    e <- return $ eval tok
    p <- with Step1_read_print (print e)
    rep) (putStrLn "" >>= (\_ => rep)) r)

-- For MAL tests
public
step1_read_print : IO ()
step1_read_print = rep