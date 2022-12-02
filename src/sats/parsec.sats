fun is_lower_case : char -> bool
fun is_upper_case : char -> bool
fun is_alpha : char -> bool
fun is_digit : char -> bool
fun is_alphanum : char -> bool
fun is_blank : char -> bool

abstype parser_boxed(a:t@ype)
typedef parser(a) = parser_boxed(a)

fun run_parser{a:t@ype}(parser(a), string): Option_vt@(a, string)
fun return{a:t@ype}(a): parser(a)
fun fail{a:t@ype}(): parser(a)
fun bind{a,b:t@ype}(parser(a), a -<cloref1> parser(b)): parser(b)
fun read(): parser(char)
fun alt{a:t@ype}(parser(a), parser(a)): parser(a)
fun choice{a:t@ype}(List(parser(a))): parser(a)
fun satisfy(char -> bool): parser(char)
fun seql{a,b:t@ype}(parser(a), parser(b)): parser(a)
fun seqr{a,b:t@ype}(parser(a), parser(b)): parser(b)
fun map{a,b:t@ype}(parser(a), a -<cloref1> b): parser(b)
fun const{a,b:t@ype}(parser(a), b): parser(b)
fun many{a:t@ype}(parser(a)): parser(List(a))
fun many1{a:t@ype}(parser(a)): parser(List(a))
fun chainl{a:t@ype}(parser(a), parser((a, a) -<cloref1> a)): parser(a)
fun chainr{a:t@ype}(parser(a), parser((a, a) -<cloref1> a)): parser(a)
fun blank(): parser(unit)
fun ws(): parser(unit)
fun ws1(): parser(unit)
fun digit(): parser(char)
fun nat(): parser(int)
fun char(char): parser(char)
fun string(string): parser(unit)
fun kw(string): parser(unit)

symintr >>=
infixr 50 >>=
overload >>= with bind

symintr <<
infixr 50 <<
overload << with seql

symintr >>
infixr 50 >>
overload >> with seqr

symintr <|>
infixr 50 <|>
overload <|> with alt

symintr >|=
infixr 50 >|=
overload >|= with map

symintr >|
infixr 50 >|
overload >| with const