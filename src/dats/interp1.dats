#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/bindlib.sats"
#staload "./../sats/term1.sats"
#staload "./../sats/interp1.sats"

#staload "./../dats/bindlib.dats"
#staload "./../dats/term1.dats"

implement interp1(m0) = begin
  println!("interp1(", m0, ")");
  case m0 of
  | I1(_) => m0
  | B1(_) => m0
  | Var1(_) => m0
  | Uni1(opr, m) => (
    case (opr, interp1(m)) of
    | (Neg1(), I1(i)) => I1(~i)
    | (Not1(), B1(b)) => B1(~b)
    | (_, _) => $raise Interp1_exn(m0))
  | Bin1(opr, m, n) => (
    case opr of
    | Add1() => (
      case (interp1(m), interp1(n)) of
      | (I1(i), I1(j)) => I1(i + j)
      | (_, _) => $raise Interp1_exn(m0))
    | Sub1() => (
      case (interp1(m), interp1(n)) of
      | (I1(i), I1(j)) => I1(i - j)
      | (_, _) => $raise Interp1_exn(m0))
    | Mul1() => (
      case (interp1(m), interp1(n)) of
      | (I1(i), I1(j)) => I1(i * j)
      | (_, _) => $raise Interp1_exn(m0))
    | Div1() => (
      case (interp1(m), interp1(n)) of
      | (I1(i), I1(j)) => I1(i / j)
      | (_, _) => $raise Interp1_exn(m0))
    | Lte1() => (
      case (interp1(m), interp1(n)) of
      | (I1(i), I1(j)) => B1(i <= j)
      | (_, _) => $raise Interp1_exn(m0))
    | Gte1() => (
      case (interp1(m), interp1(n)) of
      | (I1(i), I1(j)) => B1(i >= j)
      | (_, _) => $raise Interp1_exn(m0))
    | Lt1() => (
      case (interp1(m), interp1(n)) of
      | (I1(i), I1(j)) => B1(i < j)
      | (_, _) => $raise Interp1_exn(m0))
    | Gt1() => (
      case (interp1(m), interp1(n)) of
      | (I1(i), I1(j)) => B1(i > j)
      | (_, _) => $raise Interp1_exn(m0))
    | Eq1() => (
      case (interp1(m), interp1(n)) of
      | (I1(i), I1(j)) => B1(i = j)
      | (_, _) => $raise Interp1_exn(m0))
    | Neq1() => (
      case (interp1(m), interp1(n)) of
      | (I1(i), I1(j)) => B1(i != j)
      | (_, _) => $raise Interp1_exn(m0))
    | And1() => (
      case (interp1(m), interp1(n)) of
      | (B1(i), B1(j)) => B1(i && j)
      | (_, _) => $raise Interp1_exn(m0))
    | Or1() => (
      case (interp1(m), interp1(n)) of
      | (B1(i), B1(j)) => B1(i || j)
      | (_, _) => $raise Interp1_exn(m0))
    | _ => $raise Interp1_exn(m0))
  | Fun1(_) => m0
  | App1(m, n) => (
    case (interp1(m), interp1(n)) of
    | (u as Fun1(b), v) => 
      interp1(msubst(b, array0_make_arrpsz($arrpsz(u, v))))
    | (_, _) => $raise Interp1_exn(m0))
  | Let1(m, b) => interp1(subst(b, interp1(m)))
  | Ifx1(m, n1, n2) => (
    case interp1(m) of
    | B1(true)  => interp1(n1)
    | B1(false) => interp1(n2) 
    | _ => $raise Interp1_exn(m0))
end
