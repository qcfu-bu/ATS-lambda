#include "share/atspre_staload.hats"
#staload "./../sats/parsec.sats"
#staload UN = "prelude/SATS/unsafe.sats"

implement{a}unwrap(box) = let
  val Box(v) = box
in
  v
end

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
  vtypedef bktrace = List0_vt(char)
  vtypedef buffer = stream_vt(char)
  datavtype Result_vt(a:type) =
    | Ok_vt of (a, bktrace, buffer)
    | Err_vt of (bktrace, buffer)

  val counter = ref<int>0
  
  assume parser_boxed(a) =
    lazy((bktrace, buffer) -<cloref1> Result_vt(a))

  fun backtrack(i: int, cs: bktrace, st: buffer): @(bktrace, buffer) =
    if i <= 0 then
      (cs, st)
    else
      case cs of
      | @list_vt_nil() => (fold@cs; (cs, st))
      | ~list_vt_cons(c, cs) =>
        backtrack(i - 1, cs, stream_vt_make_cons(c, st))
in
  implement run_parser(p, st) = let
    val _ = counter[] := 0
  in
    case (!p)(list_vt_nil(), st) of
    | ~Ok_vt(a, cs, st) => let
        val _ = free(cs)
      in
        if stream_vt_is_nil(st) then
          Some_vt(a)
        else
          None_vt()
      end
    | ~Err_vt(cs, st) => let
        val _ = free(cs)
        val _ = ~st
      in
        None_vt()
      end
  end

  implement return(x) =
    $delay(lam(cs, st) => Ok_vt(x, cs, st))

  implement fail() =
    $delay(lam(cs, st) => Err_vt(cs, st))

  implement bind(p, f) =
    $delay(lam(cs, st) => let
      val res_cons = (!p)(cs, st)
    in
      case res_cons of
      | ~Ok_vt(a, cs, st) => (!(f(a)))(cs, st)
      | @Err_vt(cs, st) => (fold@res_cons; res_cons)
    end)

  implement read() =
    $delay(lam(cs, st) => let
      val st_cons = !st
    in
      case st_cons of
      | @stream_vt_nil() => let 
          prval () = fold@{char}st_cons
        in
          Err_vt(cs, stream_vt_make_con(st_cons))
        end
      | ~stream_vt_cons(c, st) => let
          val _ = counter[] := counter[] + 1
        in
          Ok_vt(Box(c), list_vt_cons(c, cs), st)
        end
    end)

  implement alt(p1, p2) =
    $delay(lam(cs, st) => let
        val i = counter[]
        val p1_cons = (!p1)(cs, st)
      in
        case p1_cons of
        | @Ok_vt(a, cs, st) => 
          (fold@p1_cons; p1_cons)
        | ~Err_vt(cs, st) => let
            val diff = counter[] - i
            val _ = counter[] := i
            val (cs, st) = backtrack(diff, cs, st) 
          in
            (!p2)(cs, st)
          end
      end)

  implement choice(ps) =
    case ps of
    | list_nil() => fail()
    | list_cons(p, ps) => alt(p, choice(ps))

  implement satisfy(f) =
    bind(read(), lam(c) =>
      if f(c.unwrap()) then
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
    bind(many1(digit()), lam(xs : List1(BChar)) => let
      val cs = list_vt2t(list_map<BChar><charNZ>(xs))
      val str = strnptr2string(string_make_list(cs))
      val i = g0string2int(str)
    in
      return(Box(i))
    end) 
  where {
    implement list_map$fopr<BChar><charNZ>(x) = let
      val c = g1ofg0(x.unwrap())
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
    $delay(lam(cs, st) => let 
        fun loop(cs: bktrace, st: buffer, xs: List_vt(char)): Result_vt(unit) = let
          val st_cons = !st
          val xs_cons = xs
        in
          case+ (xs_cons, st_cons) of
          | (@list_vt_cons(x, xs), @stream_vt_cons(y, st)) =>
            if x = y then let
              val xs0 = xs
              val st0 = st
              val _ = xs := cs
              val _ = free@{char}st_cons
              prval _ = fold@xs_cons
              val _ = counter[] := counter[] + 1
            in
              loop(xs_cons, st0, xs0)
            end
            else let
              val _ = free(xs)
              val _ = free@{char}{0}(xs_cons)
              prval _ = fold@{char}st_cons
            in
              Err_vt(cs, stream_vt_make_con(st_cons))
            end
          | (~list_vt_cons(x, xs), ~stream_vt_nil()) =>
            (free(xs); Err_vt(cs, stream_vt_make_nil()))
          | (~list_vt_nil(), @stream_vt_cons(y, st)) =>
            (fold@{char}st_cons; Ok_vt(unit(), cs, stream_vt_make_con(st_cons)))
          | (~list_vt_nil(), @stream_vt_nil()) =>
            (fold@{char}st_cons; Ok_vt(unit(), cs, stream_vt_make_con(st_cons)))
        end
      in
        loop(cs, st, string_explode(g1ofg0_string(s)))
      end)

  implement kw(s) =
    seql(string(s), ws())

  implement parens(p) =
    seqr(kw("("), seql(p, kw(")")))
end