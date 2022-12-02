abst@ype name_flt
typedef name = name_flt

abstype nmap_boxed(a : type)
typedef nmap(a) = nmap_boxed(a)

datatype op1 = Neg | Not

datatype op2 =
  | Add | Sub | Mul | Div
  | Lte | Gte | Lt | Gt | Eq | Neq
  | And | Or

datatype term =
  | Int of int
  | Bool of bool
  | Var of name
  | Op1 of (op1, term)
  | Op2 of (op2, term, term)
  | Fun of (name, name, term)
  | App of (term, term)
  | LetIn of (name, term, term)
  | Ifte of (term, term, term)

datatype value =
  | VInt of int
  | VBool of bool
  | VClo of (name, name, term, nmap(value))