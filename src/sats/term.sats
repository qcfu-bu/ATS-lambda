#staload "./types.sats"

fun fprint_op1(FILEref, op1): void
fun print_op1(op1): void
fun prerr_op1(op1): void

overload fprint with fprint_op1
overload print with print_op1
overload prerr with prerr_op1

fun fprint_op2(FILEref, op2): void
fun print_op2(op2): void
fun prerr_op2(op2): void

overload fprint with fprint_op2
overload print with print_op2
overload prerr with prerr_op2

fun fprint_term(FILEref, term): void
fun print_term(term): void
fun prerr_term(term): void

overload fprint with fprint_term
overload print with print_term
overload prerr with prerr_term