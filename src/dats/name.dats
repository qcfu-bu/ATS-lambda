#include "share/atspre_staload.hats"
#staload "./../sats/types.sats"
#staload "./../sats/name.sats"
#staload "libats/SATS/hashfun.sats"
#staload _ = "libats/DATS/hashfun.dats"
#staload UN = "prelude/SATS/unsafe.sats"

local
  assume name_flt = @(string, ulint)
in
  fun hash_key(k: string) =
    string_hash_multiplier(31UL, 31415926536UL, k)

  implement mk_name(str) =
    (str, hash_key(str))

  implement eq_name(x1, x2) = let
    val (_, id1) = x1 
    val (_, id2) = x2
  in
    id1 = id2
  end

  implement id_of_name(x) = let
    val (_, id) = x
  in
    id
  end

  implement string_of_name(x) = let
    val (str, _) = x
  in 
    str
  end

  implement gcompare_val_val<name>(x, y) =
    $UN.cast{int}(id_of_name(x) - id_of_name(y))

  implement fprint_val<name> = fprint_name
  implement fprint_name(out, x) = fprint!(out, string_of_name(x))
  implement print_name(x) = fprint_name(stdout_ref, x)
  implement prerr_name(x) = fprint_name(stderr_ref, x)
end
