#staload "./name.sats"

datatype op0 =
  | Add0 | Sub0 | Mul0 | Div0 | Neg0
  | Lte0 | Gte0 | Lt0  | Gt0  | Eq0  | Neq0
  | And0 | Or0  | Not0

datatype term0 =
  | I0 of int
  | B0 of bool
  | Var0 of name
  | Uni0 of (op0, term0)
  | Bin0 of (op0, term0, term0)
  | Fun0 of (name, name, term0)
  | App0 of (term0, term0)
  | Let0 of (name, term0, term0)
  | Ifx0 of (term0, term0, term0)

fun fprint_op0(FILEref, op0): void
fun print_op0(op0): void
fun prerr_op0(op0): void

overload fprint with fprint_op0
overload print  with print_op0
overload prerr  with prerr_op0

fun fprint_term0(FILEref, term0): void
fun print_term0(term0): void
fun prerr_term0(term0): void

overload fprint with fprint_term0
overload print  with print_term0
overload prerr  with prerr_term0