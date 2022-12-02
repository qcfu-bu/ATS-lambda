#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/types.sats"
#staload "./../sats/name.sats"
#staload "./../sats/nmap.sats"

#staload "libats/ML/SATS/funmap.sats"
#staload UN = "prelude/SATS/unsafe.sats"

#staload _ = "./name.dats"
#staload _ = "libats/ML/DATS/funmap.dats"
#staload _ = "libats/DATS/funmap_avltree.dats"

local
  assume nmap_boxed(a) = map(name, a)
in
  implement empty{a}() = funmap_nil<>()

  implement one{a}(x, a) = add(x, a, empty())

  implement add{a}(x, a, nmap) = let
    var nmap = nmap
    val opt = funmap_insert<name,a>(nmap, x, a)
  in
    case opt of
    | ~None_vt _ => nmap
    | ~Some_vt _ => nmap
  end

  implement remove{a}(x, nmap) = let
    var nmap = nmap
    val _ = funmap_remove<name,a>(nmap, x)
  in
    nmap
  end

  implement find{a}(x, nmap) = let
    val opt = funmap_search<name,a>(nmap, x)
  in
    case opt of
    | ~Some_vt v => v
    | ~None_vt _ => $raise nmap_find_exn(string_of_name(x))
  end
end