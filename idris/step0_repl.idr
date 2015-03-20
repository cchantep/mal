module Step0_repl

class ToString a where
  string : a -> String

instance ToString String where
  string s = s

read : String -> IO (Maybe String)
read prompt = do
  _ <- putStr $ prompt
  ln <- getLine
  return $ if (ln == "") then Nothing else Just ln

eval : a -> a
eval a = a

print : t -> {default %instance s : ToString t} -> IO String
print v = let str = string v in
  putStr str >>= (\_ => return str)

private
rep : IO ()
rep = read "user> " >>= (\r =>
  foldl (\_ => \expr => do
    e <- return $ eval expr
    p <- with Step0_repl (print e)
    rep
  ) (return ()) r)

-- For MAL tests
public
step0_repl : IO ()
step0_repl = rep