#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/parsec.sats"
#staload "./../sats/parser.sats"
#staload "./../sats/name.sats"
#staload "./../sats/term0.sats"
#staload _ = "./parsec.dats"
#staload _ = "./name.dats"

local
  fun reserved(): List(string) = $list(
    "fun", "let", "rec", "in", "if", 
    "then", "else", "true", "false")
in
  implement int_parser() =
    bind(nat(), lam(x) => let
      val n = x.value
    in
      seql(return(I0(n)), ws())
    end)

  implement bool_parser() =
    alt(const(kw("true"), B0(true)),
        const(kw("false"), B0(false)))

  implement var_parser() = 
    bind(satisfy(is_alpha), lam(c: Char) =>
    bind(many(alt(satisfy(is_alphanum), alt(char('_'), char('\'')))), lam(cs)=> let
      val cs = list_vt2t(list_map<Char><charNZ>(list_cons(c, cs)))
      val str = string_make_list(cs)
      val str = strnptr2string(str)
      implement list_exists$pred<string>(x) = x = str
    in
      if list_exists<string>(reserved()) then
        fail()
      else
        seql(return(str), ws())
    end))
  where {
    implement list_map$fopr<Char><charNZ>(c) = let
      val c = g1ofg0(c.value)
    in
      if isneqz(c) then
        c
      else
        $raise parser_error()
    end
  }

  implement fun_parser() =
    bind(kw("fun"), lam(_) =>
    bind(many1(var_parser()), lam(xs) =>
    bind(kw("=>"), lam(_) =>
    bind(term_parser(), lam(m) => let
      val xs = list_vt2t(list_reverse(xs))
      val m = list_foldleft<term0><string>(xs, m)
    in
      return m
    end)))) 
  where {
    implement list_foldleft$fopr<term0><string>(acc, x) =
      Fun0(mk_name(""), mk_name(x), acc)
  }

  implement letin_parser() =
    bind(kw("let"), lam(_) =>
    bind(var_parser(), lam(x) =>
    bind(many(var_parser()), lam(args) =>
    bind(kw(":="), lam(_) =>
    bind(term_parser(), lam(m) => 
    bind(kw("in"), lam(_) =>
    bind(term_parser(), lam(n) => 
    let
      val args = list_vt2t(list_reverse(args))
      val m = list_foldleft<term0><string>(args, m)
    in
      return (Let0(mk_name(x), m, n))
    end)))))))
  where {
    implement list_foldleft$fopr<term0><string>(acc, x) =
      Fun0(mk_name(""), mk_name(x), acc)
  }

  implement letrec_parser() =
    bind(kw("let"), lam(_) =>
    bind(kw("rec"), lam(_) =>
    bind(var_parser(), lam(f) =>
    bind(var_parser(), lam(x) =>
    bind(many(var_parser()), lam(args) =>
    bind(kw(":="), lam(_) =>
    bind(term_parser(), lam(m) => 
    bind(kw("in"), lam(_) =>
    bind(term_parser(), lam(n) => 
    let
      val f = mk_name(f)
      val args = list_vt2t(list_reverse(args))
      val m = list_foldleft<term0><string>(args, m)
    in
      return (Let0(f, Fun0(f, mk_name(x), m), n))
    end)))))))))
  where {
    implement list_foldleft$fopr<term0><string>(acc, x) =
      Fun0(mk_name(""), mk_name(x), acc)
  }

  implement ifte_parser() =
    bind(kw("if"), lam(_) =>
    bind(term_parser(), lam(cond) =>
    bind(kw("then"), lam(_) =>
    bind(term_parser(), lam(m) =>
    bind(kw("else"), lam(_) =>
    bind(term_parser(), lam(n) =>
    return (Ifx0(cond, m, n))))))))

  implement term0_parser(): parser(term0) =
    bind(return(unit()), lam(_) =>
    choice($list{parser(term0)}(
      int_parser(),
      map(var_parser(), lam(x) => Var0 (mk_name x)),
      fun_parser(),
      letrec_parser(),
      letin_parser(),
      ifte_parser(),
      parens(term_parser()))))

  implement term1_parser() = let
    val opr : parser((term0, term0) -<cloref1> term0) = 
      return (lam(m, n) =<cloref1> App0(m, n))
  in
    chainl(term0_parser(), opr)
  end

  implement term2_parser() = let
    val opr : parser(term0 -> term0) = 
      choice($list(
        const(kw("-"), lam(m) => Uni0(Neg0, m)),
        const(kw("!"), lam(m) => Uni0(Not0, m)),
        return(lam(m) => m)))
  in
    bind(opr, lam(f)=>
    bind(term1_parser(), lam(m) =>
    return(f m)))
  end

  implement term3_parser() = let
    val opr : parser((term0, term0) -<cloref1> term0) = 
      alt(const(kw("*"), lam(m, n) =<cloref1> Bin0(Mul0, m, n)),
          const(kw("/"), lam(m, n) =<cloref1> Bin0(Div0, m, n)))
  in
    chainl(term2_parser(), opr)
  end

  implement term4_parser() = let
    val opr : parser((term0, term0) -<cloref1> term0) = 
      alt(const(kw("+"), lam(m, n) =<cloref1> Bin0(Add0, m, n)),
          const(kw("-"), lam(m, n) =<cloref1> Bin0(Sub0, m, n)))
  in
    chainl(term3_parser(), opr)
  end

  implement term5_parser() = let
    val opr : parser((term0, term0) -<cloref1> term0) = 
      choice($list(
        const(kw("<="), lam(m, n) =<cloref1> Bin0(Lte0, m, n)),
        const(kw(">="), lam(m, n) =<cloref1> Bin0(Gte0, m, n)),
        const(kw("<"), lam(m, n) =<cloref1> Bin0(Lt0, m, n)),
        const(kw(">"), lam(m, n) =<cloref1> Bin0(Gt0, m, n))))
  in
    chainl(term4_parser(), opr)
  end

  implement term6_parser() = let
    val opr : parser((term0, term0) -<cloref1> term0) = 
      alt(const(kw("=="), lam(m, n) =<cloref1> Bin0(Eq0, m, n)),
          const(kw("!="), lam(m, n) =<cloref1> Bin0(Neq0, m, n)))
  in
    chainl(term5_parser(), opr)
  end

  implement term7_parser() = let
    val opr : parser((term0, term0) -<cloref1> term0) = 
      alt(const(kw("&&"), lam(m, n) =<cloref1> Bin0(And0, m, n)),
          const(kw("||"), lam(m, n) =<cloref1> Bin0(Or0, m, n)))
  in
    chainl(term6_parser(), opr)
  end

  implement term_parser() = 
    bind(return(unit()), lam(_) => term7_parser())
end