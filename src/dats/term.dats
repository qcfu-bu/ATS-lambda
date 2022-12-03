#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/types.sats"
#staload "./../sats/term.sats"
#staload "./../sats/name.sats"
#staload _ = "./../dats/name.dats"

implement fprint_val<op1> = fprint_op1
implement fprint_op1(out, opr) =
  case opr of
  | Neg() => fprint!(out, "Neg")
  | Not() => fprint!(out, "Not")

implement print_op1(opr) = fprint_op1(stdout_ref, opr)
implement prerr_op1(opr) = fprint_op1(stderr_ref, opr)

implement fprint_val<op2> = fprint_op2
implement fprint_op2(out, opr) =
  case opr of
  | Add() => fprint!(out, "Add")
  | Sub() => fprint!(out, "Sub")
  | Mul() => fprint!(out, "Mul")
  | Div() => fprint!(out, "Div")
  | Lte() => fprint!(out, "Lte")
  | Gte() => fprint!(out, "Gte")
  | Lt()  => fprint!(out, "Lt")
  | Gt()  => fprint!(out, "Gt")
  | Eq()  => fprint!(out, "Eq")
  | Neq() => fprint!(out, "Neq")
  | And() => fprint!(out, "And")
  | Or()  => fprint!(out, "Or")

implement print_op2(opr) = fprint_op2(stdout_ref, opr)
implement prerr_op2(opr) = fprint_op2(stderr_ref, opr)

implement fprint_val<term> = fprint_term
implement fprint_term(out, m) =
  case m of
  | Int(i) => 
    fprint!(out, "Int(", i, ")")
  | Bool(b) => 
    fprint!(out, "Bool(", b, ")")
  | Var(x) => 
    fprint!(out, "Var(", x, ")")
  | Op1(opr, m) => 
    fprint!(out, "Op1(", opr, ", ", m, ")")
  | Op2(opr, m, n) => 
    fprint!(out, "Op2(", opr, ", ", m, ", ", n, ")")
  | Fun(f, x, m) => 
    fprint!(out, "Fun(", f, ", ", x, ", ", m, ")")
  | App(m, n) =>
    fprint!(out, "App(", m, ", ", n, ")")
  | LetIn(x, m, n) =>
    fprint!(out, "LetIn(", x, ",", m, ", ", n, ")")
  | Ifte(cond, m, n) =>
    fprint!(out, "Ifte(", cond, ", ", m, ", ", n, ")")

implement print_term(m) = fprint_term(stdout_ref, m)
implement prerr_term(m) = fprint_term(stderr_ref, m)