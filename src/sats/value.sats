#staload "./types.sats"

fun fprint_value(FILEref, value): void
fun print_value(value): void
fun prerr_value(value): void

overload fprint with fprint_value
overload print with print_value
overload prerr with prerr_value