#staload "./name.sats"

abstype nmap_type(a : type)
typedef nmap(a) = nmap_type(a)

exception NMap_find_exn of name

fun nmap_empty{a:type}(): nmap a
fun nmap_unit{a:type}(name, a): nmap a
fun nmap_insert{a:type}(nmap a, name, a): nmap a
fun nmap_find{a:type}(nmap a, name): a
fun nmap_remove{a:type}(nmap a, name): nmap a