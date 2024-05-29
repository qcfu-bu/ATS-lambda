#staload "./bindlib.sats"

datatype op1 =
  | Add1 | Sub1 | Mul1 | Div1 | Neg1 
  | Lte1 | Gte1 | Lt1  | Gt1  | Eq1  | Neq1
  | And1 | Or1  | Not1

datatype term1 =
  | I1 of int
  | B1 of bool
  | Var1 of var_t(term1)
  | Uni1 of (op1, term1)
  | Bin1 of (op1, term1, term1)
  | Fun1 of mbinder(term1, term1)
  | App1 of (term1, term1)
  | Let1 of binder(term1, term1)
  | Ifx1 of (term1, term1, term1)

fun box_term1(term1): box(term1)

fun fprint_opr(FILEref, op1): void
fun print_opr(op1): void
fun prerr_opr(op1): void

overload fprint with fprint_opr
overload print  with print_opr
overload prerr  with prerr_opr

fun fprint_term1(FILEref, term1): void
fun print_term1(term1): void
fun prerr_term1(term1): void

overload fprint with fprint_term1
overload print  with print_term1
overload prerr  with prerr_term1