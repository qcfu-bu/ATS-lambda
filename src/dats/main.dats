#include "share/atspre_staload.hats"
#staload "./../sats/types.sats"
#staload "./../sats/name.sats"
#staload "./../sats/value.sats"
#staload "./../sats/nmap.sats"
#staload "./../sats/tree.sats"
#staload "./../sats/parsec.sats"

#staload _ = "./../dats/name.dats"
#staload _ = "./../dats/nmap.dats"
#staload _ = "./../dats/value.dats"
#staload _ = "./../dats/tree.dats"
#staload _ = "./../dats/parsec.dats"

#dynload "./../dats/name.dats"

implement main0() = let 
  val fp = fileref_open_exn("test.txt", file_mode_r) 
  val fact = mk_name("fact")
  val x = mk_name("x")
  val test =
    LetIn(fact, 
      Fun(fact, x, 
        Ifte(Op2(Lte(), Var(x), Int(0)), 
          Int(1),
          Op2(Mul(), Var(x), App(Var(fact), Op2(Sub, Var(x), Int(1)))))),
      App(Var(fact), Int(100)))
in
  println!(run_tree(test))
end