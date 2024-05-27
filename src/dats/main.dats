#include "share/atspre_staload.hats"
#staload "./../sats/types.sats"
#staload "./../sats/value.sats"
#staload "./../sats/tree.sats"
#staload "./../sats/parsec.sats"
#staload "./../sats/parser.sats"
#staload "./../sats/term.sats"

#staload _ = "./../dats/value.dats"
#staload _ = "./../dats/tree.dats"
#staload _ = "./../dats/parsec.dats"
#staload _ = "./../dats/parser.dats"
#staload _ = "./../dats/term.dats"

#dynload "./../dats/name.dats"
#dynload "./../dats/parsec.dats"

implement main0(argc, argv) = 
  if 2 <= argc then let 
    val fp = fileref_open_exn(argv[1], file_mode_r) 
    val buf = streamize_fileref_char(fp)
  in
    case run_parser(seqr(ws(), term_parser()), buf) of
    | ~Some_vt m => begin
      println!("ParseOk(", m, ")");
      println!("TreeWalk(", run_tree(m),")") 
    end
    | ~None_vt _ => println!("ParseError")
  end else println!("name of file expected")