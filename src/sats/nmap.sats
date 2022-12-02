#staload "./types.sats"

exception nmap_find_exn of string

fun empty{a:type}(): nmap a
fun one{a:type}(name, a): nmap a
fun add{a:type}(name, a, nmap a): nmap a
fun find{a:type}(name, nmap a): a
fun remove{a:type}(name, nmap a): nmap a