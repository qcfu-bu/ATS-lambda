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
  | Let1 of (term1, binder(term1, term1))
  | Ifx1 of (term1, term1, term1)

fun fprint_op1(FILEref, op1): void
fun print_op1(op1): void
fun prerr_op1(op1): void

overload fprint with fprint_op1
overload print  with print_op1
overload prerr  with prerr_op1

fun fprint_term1(FILEref, term1): void
fun print_term1(term1): void
fun prerr_term1(term1): void

overload fprint with fprint_term1
overload print  with print_term1
overload prerr  with prerr_term1

fn mk_var(string): var_t(term1)
fn _I1(int): box(term1)
fn _B1(bool): box(term1)
fn _Var1(var_t(term1)): box(term1)
fn _Uni1(op1, box(term1)): box(term1)
fn _Bin1(op1, box(term1), box(term1)): box(term1)
fn _Fun1(box(mbinder(term1,term1))): box(term1)
fn _App1(box(term1), box(term1)): box(term1)
fn _Let1(box(term1), box(binder(term1,term1))): box(term1)
fn _Ifx1(box(term1), box(term1), box(term1)): box(term1)

fun box_term1(term1): box(term1)