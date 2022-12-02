#include "share/atspre_staload.hats"
#staload "./../sats/types.sats"
#staload "./../sats/name.sats"

local
  assume name_flt = @(string, int)
  val stamp = ref<int>1
in
  implement mk_name(str) = let 
    val id = stamp[]
    val _ = stamp[] := id + 1
  in
    (str, id)
  end

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
    id_of_name(x) - id_of_name(y)

  implement fprint_val<name> = fprint_name
  implement fprint_name(out, x) = fprint!(out, string_of_name(x))
  implement print_name(x) = fprint_name(stdout_ref, x)
  implement prerr_name(x) = fprint_name(stderr_ref, x)
end