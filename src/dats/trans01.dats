#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/bindlib.sats"
#staload "./../sats/term0.sats"
#staload "./../sats/term1.sats"
#staload "./../sats/name.sats"
#staload "./../sats/nmap.sats"
#staload "./../sats/trans01.sats"

#staload _ = "./../dats/bindlib.dats"
#staload _ = "./../dats/term0.dats"
#staload _ = "./../dats/term1.dats"
#staload _ = "./../dats/name.dats"
#staload _ = "./../dats/nmap.dats"

fn trans_op(opr: op0): op1 =
  case opr of
  | Add0() => Add1
  | Sub0() => Sub1
  | Mul0() => Mul1
  | Div0() => Div1
  | Neg0() => Neg1
  | Lte0() => Lte1
  | Gte0() => Gte1
  | Lt0() => Lt1
  | Gt0() => Gt1
  | Eq0() => Eq1
  | Neq0() => Neq1
  | And0() => And1
  | Or0() => Or1
  | Not0() => Not1

fun trans_term(
  ctx: nmap(var_t(term1)), 
  m: term0
): box(term1) = let
  // val _ = println!("trans_term(", m, ")");
in
  case m of
  | I0(i) => _I1(i)
  | B0(b) => _B1(b)
  | Var0(x) => (
    try 
      // println!("nmap_try_find(", x, ", id: ", id_of_name(x), ")");
      _Var1(nmap_find(ctx, x))
    with 
    | ~NMap_find_exn(x) => (
      // println!("nmap_not_found(", x, ")");
      $raise NMap_find_exn(x)))
  | Uni0(opr, m) => let 
      val opr = trans_op(opr)
      val m = trans_term(ctx, m)
    in
      _Uni1(opr, m)
    end
  | Bin0(opr, m, n) => let
      val opr = trans_op(opr)
      val m = trans_term(ctx, m)
      val n = trans_term(ctx, n)
    in
      _Bin1(opr, m, n)
    end
  | Fun0(x0, y0, m) => let
      val x = mk_var(string_of_name(x0))
      val y = mk_var(string_of_name(y0))
      val ctx = nmap_insert(ctx, x0, x)
      val ctx = nmap_insert(ctx, y0, y)
      // val _ = println!("nmap_insert(", x0, ", id: ", id_of_name(x0), ")")
      // val _ = println!("nmap_insert(", y0, ", id: ", id_of_name(y0), ")")

      // val _ = nmap_find(ctx, x0)
      // val _ = println!("nmap_insert_ok(", x0, ")")
      // val _ = nmap_find(ctx, y0)
      // val _ = println!("nmap_insert_ok(", y0, ")")

      val m = trans_term(ctx, m)
      val xs = array0_make_arrpsz($arrpsz(x, y))
    in
      _Fun1(bind_mvar(xs, m))
    end
  | App0(m, n) => let
      val m = trans_term(ctx, m)
      val n = trans_term(ctx, n)
    in
      _App1(m, n)
    end
  | Let0(x0, m, n) => let
      val x = mk_var(string_of_name(x0))
      val m = trans_term(ctx, m)
      val ctx = nmap_insert(ctx, x0, x)
      val n = trans_term(ctx, n)
    in
      _Let1(m, bind_var(x, n))
    end
  | Ifx0(m, n1, n2) => let
      val m = trans_term(ctx, m)
      val n1 = trans_term(ctx, n1)
      val n2 = trans_term(ctx, n2)
    in
      _Ifx1(m, n1, n2)
    end
end

implement trans01(m) =
  unbox(trans_term(nmap_empty(), m))