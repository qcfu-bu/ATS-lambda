#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/parsec.sats"

implement is_lower_case(c) = 'a' <= c && c <= 'z'
implement is_upper_case(c) = 'A' <= c && c <= 'Z'
implement is_alpha(c) = is_lower_case(c) || is_upper_case(c)
implement is_digit(c) = '0' <= c && c <= '9'
implement is_alphanum(c) = is_alpha(c) || is_digit(c)
implement is_blank(c) = c = '\n' || c = 'r' || c = '\t'

local
  vtypedef inp = @(List_vt(char), stream_vt(char))

  datavtype Result_vt(a:t@ype) =
    | Ok_vt of (a, inp)
    | Err_vt of inp
  
  assume parser_boxed(a) =
    lazy(inp -<cloref1> Result_vt(a))

  fun backtrack(inp: inp): inp =
    case inp of
    | @(~list_vt_nil(), st) => @(list_vt_nil(), st)
    | @(~list_vt_cons(c, ls), st) =>
      backtrack(@(ls, stream_vt_make_cons(c, st)))
in
  implement return(x) =
    $delay(lam(inp) => Ok_vt(x, inp))

  implement fail() =
    $delay(lam(inp) => Err_vt(inp))
end


