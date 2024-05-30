// header
#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

// statics
#staload "./../sats/bindlib.sats"
#staload "./../sats/parsec.sats"
#staload "./../sats/parser.sats"
#staload "./../sats/term0.sats"
#staload "./../sats/term1.sats"
#staload "./../sats/trans01.sats"
#staload "./../sats/interp1.sats"

// dynamics
#staload _ = "./../dats/bindlib.dats"
#staload _ = "./../dats/parsec.dats"
#staload _ = "./../dats/parser.dats"
#staload _ = "./../dats/term0.dats"
#staload _ = "./../dats/term1.dats"
#staload _ = "./../dats/trans01.dats"
#staload _ = "./../dats/interp1.dats"

// initialize
#dynload "./../dats/bindlib.dats"
#dynload "./../dats/parsec.dats"
#dynload "./../dats/name.dats"
#dynload "./../dats/nmap.dats"
#dynload "./../dats/interp1.dats"

implement main0(argc, argv) = 
  if 2 <= argc then let 
    val fp = fileref_open_exn(argv[1], file_mode_r) 
    val buf = streamize_fileref_char(fp)
  in
    case run_parser(seqr(ws(), term_parser()), buf) of
    | ~Some_vt m => let
        val _ = println!("parse_ok(", m, ")")
        val m = trans01(m) 
        val _ = println!("trans01_ok(", m, ")")
        val m = interp1(m)
        val _ = println!("interp1_ok(", m, ")")
      in end
    | ~None_vt _ => println!("ParseError")
  end else println!("name of file expected")