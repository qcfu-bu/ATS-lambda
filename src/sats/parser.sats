#staload "./parsec.sats"
#staload "./term0.sats"

fun int_parser(): parser(term0)
fun bool_parser(): parser(term0)
fun var_parser(): parser(string)
fun fun_parser(): parser(term0)
fun letin_parser(): parser(term0)
fun letrec_parser(): parser(term0)
fun ifte_parser(): parser(term0)
fun term_parser(): parser(term0)
fun term0_parser(): parser(term0)
fun term1_parser(): parser(term0)
fun term2_parser(): parser(term0)
fun term3_parser(): parser(term0)
fun term4_parser(): parser(term0)
fun term5_parser(): parser(term0)
fun term6_parser(): parser(term0)
fun term7_parser(): parser(term0)