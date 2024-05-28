#include "share/atspre_staload.hats"
#staload "./../sats/parsec.sats"
#staload UN = "prelude/SATS/unsafe.sats"

implement is_lower_case(c) = 'a' <= c && c <= 'z'
implement is_upper_case(c) = 'A' <= c && c <= 'Z'
implement is_alpha(c) = is_lower_case(c) || is_upper_case(c)
implement is_digit(c) = '0' <= c && c <= '9'
implement is_alphanum(c) = is_alpha(c) || is_digit(c)
implement is_blank(c) =
  case c of
  | ' ' => true
  | '\n' => true
  | '\r' => true
  | '\t' => true
  | _ => false

local
  vtypedef buffer = stream(char)
  datavtype Result_vt(a:type) =
    | Ok_vt of (a, buffer)
    | Err_vt

  assume parser_boxed(a) =
    lazy(buffer -<cloref1> Result_vt(a))
in
  implement run_parser(p, buf) =
    case (!p)(stream_vt2t(buf)) of
    | ~Ok_vt(a, buf) => 
      if stream_is_nil(buf) then
        Some_vt(a)
      else
        None_vt()
    | ~Err_vt() => None_vt()

  implement return(x) =
    $delay(lam(buf) => Ok_vt(x, buf))

  implement fail() =
    $delay(lam(_) => Err_vt())

  implement bind(p, f) =
    $delay(lam(buf) => let
      val res_cons = (!p)(buf)
    in
      case res_cons of
      | ~Ok_vt(a, buf) => (!(f(a)))(buf)
      | @Err_vt() => (fold@res_cons; res_cons)
    end)

  implement read() =
    $delay(lam(buf) =>
      case !buf of
      | stream_nil() => Err_vt()
      | stream_cons(c, buf) => Ok_vt('{ value= c }, buf))

  implement alt(p1, p2) =
    $delay(lam(buf) => let
        val p1_cons = (!p1)(buf)
      in
        case p1_cons of
        | @Ok_vt(_, _) => (fold@p1_cons; p1_cons)
        | ~Err_vt() => (!p2)(buf)
      end)

  implement choice(ps) =
    case ps of
    | list_nil() => fail()
    | list_cons(p, ps) => alt(p, choice(ps))

  implement satisfy(f) =
    bind(read(), lam(c) =>
      if f(c.value) then
        return(c)
      else
        fail())
  
  implement seql(p1, p2) =
    bind(p1, lam(a) =>
    bind(p2, lam(_) =>
      return(a)))

  implement seqr(p1, p2) =
    bind(p1, lam(_) =>       
    bind(p2, lam(b) =>
      return(b)))

  implement map(p, f) = 
    bind(p, lam(a) => return(f(a)))

  implement const(p, a) =
    seqr(p, return a)
  
  implement many{a}(p: parser(a)) =
    alt(bind{a,List0(a)}(p, lam(x) => 
      bind{List0(a),List0(a)}(many(p), lam(xs) =>
        return(list_cons(x, xs)))),
      return(list_nil()))

  implement many1{a}(p: parser(a)) =
    bind{a,List1(a)}(p, lam(x) =>
    bind{List0(a),List1(a)}(many(p), lam(xs) =>
      return(list_cons(x, xs))))

  implement chainl{a}(p, q) =
    bind(p, lam(x) =>
    bind(many(bind(q, lam(f) =>
              bind(p, lam(x) =>
              return($tup(f, x))))), lam(fxs) =>
    return(list_foldleft<a><chain_elem(a)>(fxs, x))))
  where {
    implement list_foldleft$fopr<a><chain_elem(a)>(acc, $tup(f, x)) =
      f(acc, x)
  }

  implement chainr(p, q) =
    bind(p, lam(x) =>
    alt(bind(q, lam(f) =>
        bind(chainr(p, q), lam(y) =>
          return(f(x, y)))),
        return(x)))

  implement blank() =
    const(satisfy(is_blank), unit())

  implement ws() =
    (const(many(blank()), unit()))

  implement ws1() =
    const(many1(blank()), unit())

  implement digit() =
    satisfy(is_digit)

  implement nat() =
    bind(many1(digit()), lam(xs : List1(Char)) => let
      val cs = list_vt2t(list_map<Char><charNZ>(xs))
      val str = strnptr2string(string_make_list(cs))
      val i = g0string2int(str)
    in
      return('{ value= i })
    end) 
  where {
    implement list_map$fopr<Char><charNZ>(x) = let
      val c = g1ofg0(x.value)
    in
      if isneqz(c) then
        c
      else
        $raise parser_error()
    end
  }

  implement char(c) =
    satisfy(lam(x) => c = x)

  implement string(s) = 
    $delay(lam(buf) => let
        fun loop(xs: List_vt(char), buf: stream(char)): Result_vt(unit) =
          case (xs, !buf) of
          | (~list_vt_cons(x, xs), stream_cons(y, buf)) =>
            if x = y then
              loop(xs, buf)
            else
              (free(xs); Err_vt())
          | (~list_vt_cons(x, xs), stream_nil()) =>
            (free(xs); Err_vt())
          | (~list_vt_nil(), stream_cons(y, buf)) =>
            Ok_vt(unit(), stream_make_cons(y, buf))
          | (~list_vt_nil(), stream_nil()) =>
            Ok_vt(unit(), stream_make_nil())
      in
        loop(string_explode(g1ofg0_string(s)), buf)
      end)

  implement kw(s) =
    seql(string(s), ws())

  implement parens(p) =
    seqr(kw("("), seql(p, kw(")")))
end