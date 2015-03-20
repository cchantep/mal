module Reader

import Token

private
whitespace : Char -> Bool
whitespace c = (isSpace c) || (c == ',')

readStr : String -> String -> Result (Token, String)
readStr "" s = Right (StrLiteral (reverse s), "")
readStr bf s =
  let hd = strHead bf in
  let tl = strTail bf in
  if (hd == '"') then Right (StrLiteral (reverse s), tl)
  else readStr tl (strCons hd s)

readSymbol : String -> String -> Result (Token, String)
readSymbol "" s = Right (Symbol (reverse s), "")
readSymbol bf s =
  let hd = strHead bf in
  let tl = strTail bf in
  if (whitespace hd) then Right (Symbol (reverse s), tl)
  else if (hd == ';') then Right (Symbol (reverse s), "")
  else if (hd == ')' || hd == ']' || hd == '}') then -- TODO: Parameter
    Right (Symbol (reverse s), (strCons hd tl))
  else readSymbol tl (strCons hd s)

readOp : String -> String -> Result (Token, String)
readOp "" s = Right (Operator (reverse s), "")
readOp bf s =
  let hd = strHead bf in
  let tl = strTail bf in
  if (whitespace hd) then Right (Symbol (reverse s), tl)
  else if (hd == ';') then Right (Symbol (reverse s), "")  
  else readSymbol tl (strCons hd s)

-- TODO: State monad
tokenize : String -> List Token -> Result (List Token, String)
tokenize "" tokens = Right ((reverse tokens), "")
tokenize st tokens =
  let hd = strHead st in
  if (hd == ')' || hd == ';' || hd == ']') then Right ((reverse tokens), "")
  else let tl = strTail st in
    if (hd == '(') then (tokenize tl []) >>= (\(tks, rem) =>
      tokenize rem ((Expr ('(', ')', tks)) :: tokens))
    else if (hd == '[') then (tokenize tl []) >>= (\(tks, rem) =>
      tokenize rem ((Expr ('[', ']', tks)) :: tokens))
    else if (hd == '{') then (tokenize tl []) >>= (\(tks, rem) =>
      tokenize rem ((Expr ('{', '}', tks)) :: tokens))      
    else if (whitespace hd) then tokenize tl tokens
    else if (hd == '"') then
      readStr tl "" >>= (\(tk, rem) => tokenize rem (tk :: tokens))
    else if (isAlphaNum hd == False) then
      readOp st "" >>= (\(tk, rem) => tokenize rem (tk :: tokens))    
    else readSymbol st "" >>= (\(tk, rem) => tokenize rem (tk :: tokens))
    -- TODO: Number, Keywords

normalize : Bool -> (List Token) -> Result Token
normalize False [] = Left "No token"
normalize False (_ :: _ :: _) = Left "Single token expected"
normalize False [t] = Right t
normalize True ((Expr e) :: Nil) = Right (Expr e)
normalize True ts = Right (Expr ('(', ')', ts))

public
tokenizer : String -> Result Token
tokenizer input =
  let isExpr = (strHead input) == '(' in
  tokenize input Nil >>= (\(tks, _) => normalize isExpr tks)