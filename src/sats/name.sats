#staload "./types.sats"

fun mk_name(string): name
fun eq_name(name, name): bool
fun id_of_name(name):<> INV(int)
fun string_of_name(name): string

fun fprint_name(FILEref, name): void
fun print_name(name): void
fun prerr_name(name): void

overload = with eq_name
overload fprint with fprint_name
overload print with print_name
overload prerr with prerr_name