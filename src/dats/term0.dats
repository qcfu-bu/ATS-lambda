#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/term0.sats"
#staload "./../sats/name.sats"
#staload _ = "./../dats/name.dats"

implement print_op0(opr) = fprint_op0(stdout_ref, opr)
implement prerr_op0(opr) = fprint_op0(stderr_ref, opr)
implement fprint_op0(out, opr) =
  case opr of
  | Add0() => fprint!(out, "Add")
  | Sub0() => fprint!(out, "Sub")
  | Mul0() => fprint!(out, "Mul")
  | Div0() => fprint!(out, "Div")
  | Neg0() => fprint!(out, "Neg")
  | Lte0() => fprint!(out, "Lte")
  | Gte0() => fprint!(out, "Gte")
  | Lt0()  => fprint!(out, "Lt")
  | Gt0()  => fprint!(out, "Gt")
  | Eq0()  => fprint!(out, "Eq")
  | Neq0() => fprint!(out, "Neq")
  | And0() => fprint!(out, "And")
  | Or0()  => fprint!(out, "Or")
  | Not0() => fprint!(out, "Not")

implement fprint_val<op0> = fprint_op0

implement print_term0(m) = fprint_term0(stdout_ref, m)
implement prerr_term0(m) = fprint_term0(stderr_ref, m)
implement fprint_term0(out, m) =
  case m of
  | I0(i)            => fprint!(out, "Int(", i, ")")
  | B0(b)            => fprint!(out, "Bool(", b, ")")
  | Var0(x)          => fprint!(out, "Var(", x, ")")
  | Uni0(opr, m)     => fprint!(out, "Op1(", opr, ", ", m, ")")
  | Bin0(opr, m, n)  => fprint!(out, "Op2(", opr, ", ", m, ", ", n, ")")
  | Fun0(f, x, m)    => fprint!(out, "Fun(", f, ", ", x, ", ", m, ")")
  | App0(m, n)       => fprint!(out, "App(", m, ", ", n, ")")
  | Let0(x, m, n)    => fprint!(out, "LetIn(", x, ",", m, ", ", n, ")")
  | Ifx0(cond, m, n) => fprint!(out, "Ifte(", cond, ", ", m, ", ", n, ")")