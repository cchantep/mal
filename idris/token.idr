module Token
-- Common datatypes

data Token = Symbol String | StrLiteral String |
  Operator String | Expr (Char, Char, (List Token))

Validation : Type -> Type -> Type
Validation error valid = Either error valid

Result : Type -> Type
Result valid = Validation String valid