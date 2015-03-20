module Printer

import Token

mutual
  private
  print_expr : List Token -> String -> String
  print_expr (t::ts) str =
    let s = print_str t in
    print_expr ts (if (str == "") then s else str ++ (strCons ' ' s))
  print_expr [] str = strCons '(' $ str ++ (strCons ')' "")

  private
  print_vect : List Token -> String -> String
  print_vect (t::ts) str =
    let s = print_str t in
    print_vect ts (if (str == "") then s else str ++ (strCons ' ' s))
  print_vect [] str = str

  private
  print_str : Token -> String
  print_str (StrLiteral s) = strCons '"' $ s ++ (strCons '"' "")
  print_str (Symbol s) = s
  print_str (Operator o) = o
  print_str (Expr (p, s, ls)) = print_expr ls ""

instance Show Token where
  show t = print_str t