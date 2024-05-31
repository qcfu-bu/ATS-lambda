#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/bindlib.sats"
#staload "./../sats/term1.sats"

#staload _ = "./../dats/bindlib.dats"

implement print_op1(opr) = fprint_op1(stdout_ref, opr)
implement prerr_op1(opr) = fprint_op1(stderr_ref, opr)
implement fprint_op1(out, opr) =
  case opr of
  | Add1() => fprint!(out, "+")
  | Sub1() => fprint!(out, "-")
  | Mul1() => fprint!(out, "*")
  | Div1() => fprint!(out, "/")
  | Neg1() => fprint!(out, "-")
  | Lte1() => fprint!(out, "<=")
  | Gte1() => fprint!(out, ">=")
  | Lt1()  => fprint!(out, "<")
  | Gt1()  => fprint!(out, ">")
  | Eq1()  => fprint!(out, "==")
  | Neq1() => fprint!(out, "!=")
  | And1() => fprint!(out, "&&")
  | Or1()  => fprint!(out, "||")
  | Not1() => fprint!(out, "!")

implement fprint_val<op1> = fprint_op1

implement print_term1(m) = fprint_term1(stdout_ref, m)
implement prerr_term1(m) = fprint_term1(stderr_ref, m)
implement fprint_term1(out, m) =
  case m of
  | I1(i) => fprint!(out, i)
  | B1(b) => fprint!(out, b)
  | Var1(x) => fprint!(out, x)
  | Uni1(opr, m) => fprint!(out, opr, m)
  | Bin1(opr, m, n) => fprint!(out, "(", m, " ", opr, " ", n, ")")
  | Fun1(b) => let
      val (xs, m) = unmbind(b)
    in
      fprint!(out, "(fun (", xs, ") -> )")
    end
  | App1(m, n) => fprint!(out, "(", m, " @ ", n, ")")
  | Let1(m, b) => let
      val (x, n) = unbind(b)
    in
      fprint!(out, "let ", x, " = ", m, " in ", n)
    end
  | Ifx1(m, n1, n2) =>
    fprint!(out, "if ", m, " then ", n1, " else ", n2)

implement mk_var(s)        = new_var(lam(x) => Var1(x), s)
implement _I1(i)           = box(I1(i))
implement _B1(b)           = box(B1(b))
implement _Var1(x)         = box_var(x)
implement _Uni1(opr, m)    = box_apply(lam(m) => Uni1(opr, m), m)
implement _Bin1(opr, m, n) = box_apply2(lam(m, n) => Bin1(opr, m, n), m, n)
implement _Fun1(b)         = box_apply(lam(b) => Fun1(b), b)
implement _App1(m, n)      = box_apply2(lam(m, n) => App1(m, n), m, n)
implement _Let1(m, b)      = box_apply2(lam(m, b) => Let1(m, b), m, b)
implement _Ifx1(m, n1, n2) = box_apply3(lam(m, n1, n2) => Ifx1(m, n1, n2), m, n1, n2)

fun lift_term1(m: term1):<cloref1> box(term1) =
  case m of
  | I1(i)           => _I1(i)
  | B1(b)           => _B1(b)
  | Var1(x)         => _Var1(x)
  | Uni1(opr, m)    => _Uni1(opr, lift_term1(m))
  | Bin1(opr, m, n) => _Bin1(opr, lift_term1(m), lift_term1(n))
  | Fun1(b)         => _Fun1(box_mbinder(lift_term1, b))
  | App1(m, n)      => _App1(lift_term1(m), lift_term1(n))
  | Let1(m, b)      => _Let1(lift_term1(m), box_binder(lift_term1, b))
  | Ifx1(m, n1, n2) => _Ifx1(lift_term1(m), lift_term1(n1), lift_term1(n2))

implement box_term1(m) = lift_term1(m)