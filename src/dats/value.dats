#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/types.sats"
#staload "./../sats/name.sats"
#staload "./../sats/value.sats"

#staload _ = "./../dats/name.dats"
#staload _ = "./../dats/nmap.dats"

implement fprint_val<value> = fprint_value
implement fprint_value(out, v) = 
  case v of
  | VInt(i) => fprint!(out, "VInt(", i, ")")
  | VBool(b) => fprint!(out, "VBool(", b, ")")
  | VClo(f, x, m, e) => fprint!(out, "VClo(", f, ", ", x, ")")

implement print_value(v) = fprint_value(stdout_ref, v)
implement prerr_value(v) = fprint_value(stderr_ref, v)