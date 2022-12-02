#include "share/atspre_staload.hats"
#staload "./../sats/types.sats"
#staload "./../sats/name.sats"
#staload "./../sats/value.sats"
#staload "./../sats/nmap.sats"
#staload "./../sats/tree.sats"
#staload "./../sats/parsec.sats"
#staload "./../sats/parser.sats"

#staload _ = "./../dats/name.dats"
#staload _ = "./../dats/nmap.dats"
#staload _ = "./../dats/value.dats"
#staload _ = "./../dats/tree.dats"
#staload _ = "./../dats/parsec.dats"
#staload _ = "./../dats/parser.dats"

#dynload "./../dats/name.dats"
#dynload "./../dats/parsec.dats"

implement main0() = let 
  val fp = fileref_open_exn("test.txt", file_mode_r) 
  val st = streamize_fileref_char(fp)
  val simpl = seqr(string("abc"), seqr(ws(), alt(string("ocaml"), alt(string("haskell"), string("coq")))))
in
  case run_parser(var_parser(), st) of
  | ~Some_vt x => println!("ok(", x, ")")
  | ~None_vt _ => println!("bad")
end