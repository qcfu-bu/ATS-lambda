#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/name.sats"
#staload "./../sats/nmap.sats"

#staload UN = "prelude/SATS/unsafe.sats"
#staload "libats/ML/SATS/funmap.sats"

#staload _ = "./name.dats"
#staload _ = "libats/ML/DATS/funmap.dats"
#staload _ = "libats/DATS/funmap_avltree.dats"

local
  assume nmap_type(a) = map(name, a)
in
  implement nmap_empty{a}() = 
    funmap_nil<>()

  implement nmap_unit{a}(x, a) = 
    nmap_insert(nmap_empty(), x, a)

  implement nmap_insert{a}(nmap, x, a) = let
    var nmap = nmap
    val opt = funmap_insert<name,a>(nmap, x, a)
  in
    case opt of
    | ~None_vt _ => nmap
    | ~Some_vt _ => nmap
  end

  implement nmap_remove{a}(nmap, x) = let
    var nmap = nmap
    val _ = funmap_remove<name,a>(nmap, x)
  in
    nmap
  end

  implement nmap_find{a}(nmap, x) = let
    val opt = funmap_search<name,a>(nmap, x)
  in
    case opt of
    | ~Some_vt v => v
    | ~None_vt _ => $raise NMap_find_exn(x)
  end
end