
typedef boxed(a:t@ype) = '{
  value= a
}

typedef Char = boxed(char)
typedef Int  = boxed(int)

val is_lower_case: char -<cloref1> bool
val is_upper_case: char -<cloref1> bool
val is_alpha: char -<cloref1> bool
val is_digit: char -<cloref1> bool
val is_alphanum: char -<cloref1> bool
val is_blank: char -<cloref1> bool

abstype parser_boxed(a:type)
typedef parser(a) = parser_boxed(a)
exception parser_error of ()

typedef chain_fun(a:type) = (a, a) -<cloref1> a
typedef chain_elem(a:type) = $tup(((a, a) -<cloref1> a), a)

fun run_parser{a:type}(parser(a), stream_vt(char)): Option_vt(a)
fun return{a:type}(a): parser(a)
fun fail{a:type}(): parser(a)
fun bind{a,b:type}(parser(a), a -<cloref1> parser(b)): parser(b)
fun loc(): parser(Int)
fun read(): parser(Char)
fun alt{a:type}(parser(a), parser(a)): parser(a)
fun choice{a:type}(List0(parser(a))): parser(a)
fun satisfy(char -<cloref1> bool): parser(Char)
fun seql{a,b:type}(parser(a), parser(b)): parser(a)
fun seqr{a,b:type}(parser(a), parser(b)): parser(b)
fun map{a,b:type}(parser(a), a -<cloref1> b): parser(b)
fun const{a,b:type}(parser(a), b): parser(b)
fun many{a:type}(parser(a)): parser(List0(a))
fun many1{a:type}(parser(a)): parser(List1(a))
fun chainl{a:type}(parser(a), parser(chain_fun(a))): parser(a)
fun chainr{a:type}(parser(a), parser(chain_fun(a))): parser(a)
fun blank(): parser(unit)
fun ws(): parser(unit)
fun ws1(): parser(unit)
fun digit(): parser(Char)
fun nat(): parser(Int)
fun char(char): parser(Char)
fun string(string): parser(unit)
fun kw(string): parser(unit)
fun parens{a:type}(p: parser(a)) : parser(a)