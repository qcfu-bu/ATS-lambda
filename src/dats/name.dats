#include "share/atspre_staload.hats"

#staload "./../sats/name.sats"

#staload UN = "prelude/SATS/unsafe.sats"
#staload "libats/SATS/hashfun.sats"

#staload _ = "libats/DATS/hashfun.dats"

local
assume name_flt = @(string, ulint)
in
fun hash_key(k: string) =
  // string_hash_multiplier(31UL, 31415926536UL, k) does not work for some reason
  string_hash_multiplier(33UL, 31415926536UL, k)

implement mk_name(str) = (str, hash_key(str))
implement eq_name(x1, x2) = x1.1 = x2.1
implement id_of_name(x) = x.1
implement string_of_name(x) = x.0
implement gcompare_val_val<name>(x, y) = let
  val x = $UN.cast{int}(x.1)
  val y = $UN.cast{int}(y.1)
in
  x - y
end

implement fprint_name(out, (s, id)) = fprint!(out, s)
implement print_name(x) = fprint_name(stdout_ref, x)
implement prerr_name(x) = fprint_name(stderr_ref, x)
implement fprint_val<name> = fprint_name
end
