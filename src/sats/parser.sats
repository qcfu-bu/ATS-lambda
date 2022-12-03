#staload "./types.sats"
#staload "./parsec.sats"

fun int_parser(): parser(term)
fun bool_parser(): parser(term)
fun var_parser(): parser(string)
fun fun_parser(): parser(term)
fun letin_parser(): parser(term)
fun letrec_parser(): parser(term)
fun ifte_parser(): parser(term)
fun term_parser(): parser(term)
fun term0_parser(): parser(term)
fun term1_parser(): parser(term)
fun term2_parser(): parser(term)
fun term3_parser(): parser(term)
fun term4_parser(): parser(term)
fun term5_parser(): parser(term)
fun term6_parser(): parser(term)
fun term7_parser(): parser(term)