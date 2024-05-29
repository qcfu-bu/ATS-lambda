// header
#include "share/atspre_staload.hats"

// statics
#staload "./../sats/types.sats"
#staload "./../sats/value.sats"
#staload "./../sats/tree.sats"
#staload "./../sats/parsec.sats"
#staload "./../sats/parser.sats"
#staload "./../sats/term.sats"

#staload "./../sats/bindlib.sats"

// dynamics
#staload _ = "./../dats/bindlib.dats"
#staload _ = "./../dats/value.dats"
#staload _ = "./../dats/tree.dats"
#staload _ = "./../dats/parsec.dats"
#staload _ = "./../dats/parser.dats"
#staload _ = "./../dats/term.dats"

#staload _ = "./../dats/bindlib.dats"

// initialize
#dynload "./../dats/name.dats"
#dynload "./../dats/parsec.dats"
#dynload "./../dats/bindlib.dats"


datatype expr =
  | Var of var_t(expr)
  | Lam of binder(expr, expr)
  | App of (expr, expr)

fun mk_var(s): var_t(expr) = new_var(lam(x) => Var(x), s)
fun _Var(x: var_t(expr)): box(expr) = box_var(x)
fun _App(m: box(expr), n: box(expr)): box(expr) =
  box_apply2(lam(m, n) => App(m, n), m, n)
fun _Lam(b: box(binder(expr, expr))): box(expr) =
  box_apply(lam(b) => Lam(b), b)


extern fun fprint_expr(out: FILEref, m: expr): void
extern fun print_expr(m: expr): void
extern fun prerr_expr(m: expr): void
overload fprint with fprint_expr
overload print with print_expr
overload prerr with prerr_expr

implement fprint_val<expr>(out, m) = fprint_expr(out, m)
implement print_expr(m) = fprint_expr(stdout_ref, m)
implement prerr_expr(m) = fprint_expr(stderr_ref, m)

implement fprint_expr(out: FILEref, m: expr): void =
  case m of
  | Var(x) => fprint!(out, x)
  | Lam(b) => let
      val (x, m) = unbind(b)
    in
      fprintln!(out, "Lam(", x, ", ", m, ")")
    end
  | App(m, n) => fprint!(out, "App(", m, ", ", n, ")")


// implement main0(argc, argv) = 
//   if 2 <= argc then let 
//     val fp = fileref_open_exn(argv[1], file_mode_r) 
//     val buf = streamize_fileref_char(fp)
//   in
//     case run_parser(seqr(ws(), term_parser()), buf) of
//     | ~Some_vt m => begin
//       println!("ParseOk(", m, ")");
//       println!("TreeWalk(", run_tree(m),")") 
//     end
//     | ~None_vt _ => println!("ParseError")
//   end else println!("name of file expected")


implement main0() = let
  val x = mk_var("x") 
  val y = mk_var("y") 
  val m = _Lam(bind_var(x, _App(_Var(x), _Var(y))))
  val m = unbox(m)
in 
  println!(m)
end
