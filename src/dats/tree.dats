#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/types.sats"
#staload "./../sats/nmap.sats"
#staload "./../sats/tree.sats"

#staload _ = "./name.dats"
#staload _ = "./nmap.dats"

fun eval(m : term, env : nmap value) : value =
  case m of
  | Int(i) => VInt(i)
  | Bool(b) => VBool(b)
  | Var(x) => find(x, env)
  | Op1(opr, m) => let
      val v = eval(m, env)
    in
      case (opr, v) of
      | (Neg(), VInt(i)) => VInt(~1)
      | (Not(), VBool(b)) => VBool(~b)
      | (_, _) => $raise eval_error()
    end
  | Op2(opr, m, n) => let
      val v1 = eval(m, env)
      val v2 = eval(n, env)
    in
      case opr of
      | Add() => (
        case (v1, v2) of 
        | (VInt(i), VInt(j)) => VInt(i + j)
        | (_, _) => $raise eval_error())
      | Sub() => (
        case (v1, v2) of 
        | (VInt(i), VInt(j)) => VInt(i - j)
        | (_, _) => $raise eval_error())
      | Mul() => (
        case (v1, v2) of 
        | (VInt(i), VInt(j)) => VInt(i * j)
        | (_, _) => $raise eval_error())
      | Div() => (
        case (v1, v2) of 
        | (VInt(i), VInt(j)) => VInt(i / j)
        | (_, _) => $raise eval_error())
      | Lte() => (
        case (v1, v2) of 
        | (VInt(i), VInt(j)) => VBool(i <= j)
        | (_, _) => $raise eval_error())
      | Gte() => (
        case (v1, v2) of 
        | (VInt(i), VInt(j)) => VBool(i >= j)
        | (_, _) => $raise eval_error())
      | Lt() => (
        case (v1, v2) of 
        | (VInt(i), VInt(j)) => VBool(i < j)
        | (_, _) => $raise eval_error())
      | Gt() => (
        case (v1, v2) of 
        | (VInt(i), VInt(j)) => VBool(i > j)
        | (_, _) => $raise eval_error())
      | Eq() => (
        case (v1, v2) of 
        | (VInt(i), VInt(j)) => VBool(i = j)
        | (_, _) => $raise eval_error())
      | Neq() => (
        case (v1, v2) of 
        | (VInt(i), VInt(j)) => VBool(i <> j)
        | (_, _) => $raise eval_error())
      | And() => (
        case (v1, v2) of 
        | (VBool(i), VBool(j)) => VBool(i && j)
        | (_, _) => $raise eval_error())
      | Or() => (
        case (v1, v2) of 
        | (VBool(i), VBool(j)) => VBool(i || j)
        | (_, _) => $raise eval_error())
    end
  | Fun(f, x, m) => VClo(f, x, m, env)
  | App(m, n) => let
      val v1 = eval(m, env)
      val v2 = eval(n, env)
    in
      case v1 of
      | VClo(f, x, m, env') =>
        eval(m, add(x, v2, add(f, v1, env')))
      | _ => $raise eval_error()
    end
  | LetIn(x, m, n) => let
      val v = eval(m, env)
    in
      eval(n, add(x, v, env))
    end
  | Ifte(cond, m, n) => let
      val v = eval(cond, env)
    in
      case v of
      | VBool(true) => eval(m, env)
      | VBool(false) => eval(n, env)
      | _ => $raise eval_error()
    end

implement run_tree(m) = eval(m, empty())