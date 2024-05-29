(** The [Bindlib] library provides support for free and bound variables in the
    ATS2 language. The main application is the representation of types with a
    binding structure (e.g., abstract syntax trees).

    @author[ATS2]  Qiancheng Fu
    @author[OCaml] Christophe Raffalli
    @author[OCaml] Rodolphe Lepigre
    @version 6.0.0 *)

(* header *)
#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

(* statics *)
#staload UN = "prelude/SATS/unsafe.sats"
#staload "./../sats/bindlib.sats"

(* list notation *)
#define :: list0_cons
#define nil0 list0_nil 

(* exception(s) *)
exception Not_found
exception Arity_mismatch

(* Counter for generating fresh variable keys (i.e., unique identifiers). *)
local 
val c: ref(int) = ref(~1)
in
fn fresh_key(): int = (!c := !c + 1; !c)
fn reset_counter(): void = !c := ~1
end

(* A placeholder type for any boxed type. *)
abstype any_boxed = ptr
typedef any = any_boxed

(* Environment **************************************************************)

(* The [Bindlib] library uses environments to store values associated to bound
   variables upon substitution. We rely on unsafe.sats to store variables with 
   potentially different types in a single array. *)

abst@ype env
extern fn env_create(next_free: size_t, size: size_t): env
extern fn env_set{a:type}(env: env, i: size_t, e: a): void
extern fn env_get{a:type}(i: size_t, env: env): a
extern fn env_blit(src: env, dst: env, len: size_t): void
extern fn env_copy(env: env): env
extern fn env_get_next_free(env: env): size_t
extern fn env_set_next_free(env: env, n: size_t): void

local
assume env = @{ tab= array0(any), next_free= ref(size_t) }
in
implement env_create(next_free, size) = @{
  tab= array0_make_elt<any>(size, $UN.cast{any}(0)), 
  next_free= ref<size_t>(next_free)
}

implement env_set(env, i, e) = let
  val tab = env.tab
in
  tab[i] := $UN.cast{any}(e)
end

implement env_get{a}(i, env) = let 
  val tab = env.tab
in
  $UN.cast{a}(tab[i])
end

implement env_blit(src, dst, len) = let
  val src_tab = src.tab
  val dst_tab = dst.tab
  implement intrange_foreach$fwork<void>(i, env) =
    dst_tab[i] := src_tab[i]; 
  val _ = intrange_foreach(0, g0uint2int(len))
in end

implement env_copy(env) = let 
  val tab = array0_copy<any>(env.tab) 
  val next_free = ref(!(env.next_free))
in 
  @{ tab= tab, next_free= next_free }
end

implement env_get_next_free(env) = !(env.next_free)
implement env_set_next_free(env, n) = !(env.next_free) := n
end

(* Closure representation ***************************************************)

(* In the internals of [Bindlib], variables are identified by a unique integer
   key. Closures are then formed by mapping each free variable to a slot in an
   environment (of type [env]), but not directly. The mapping is established
   using a map (of type [varpos]) which associates variable keys to indices in
   the environment. *)

typedef varpos = map(int, size_t)
typedef closure(a: type) = cfun2(varpos,env,a)

fn varpos_empty(): varpos = funmap_nil<>()

fn varpos_insert(vp: varpos, i: int, x: size_t): varpos = let
  var vp = vp
in
  case funmap_insert(vp, i, x) of
  | ~Some_vt(_) => vp
  | ~None_vt( ) => vp
end

fn varpos_unit(i: int, x: size_t): varpos =
  varpos_insert(varpos_empty(), i, x)

fn varpos_find(vp: varpos, i: int): size_t =
  case funmap_search(vp, i) of
  | ~Some_vt(x) => x
  | ~None_vt( ) => (println!("varpos_not_found"); $raise Not_found)

fn map_closure{a,b:type}(f: cfun(a,b), cla: closure(a)): closure(b) = 
  lam(vs, env) => f(cla(vs, env))

fn app_closure{a,b:type}(clf: closure(cfun(a,b)), a: a): closure(b) =
  lam(vs, env) => clf(vs, env)(a)

fn comp_closure{a,b:type}(clf: closure(cfun(a,b)), cla: closure(a)): closure(b) =
  lam(vs, env) => clf(vs, env)(cla(vs, env))

(* Box and variable representation ******************************************)

(* At the core of [Bindlib] is the type [box(a)] which represents an object of
   type [a] whose free variables are available for binding. Intuitively, this
   is represented as the combination of an environment and a closure as in the
   [Env(vs,n,t)]. The [Box(e)] constructor's only role is to optimise the case
   where an object is known to be closed.

   In the [Env(vs,n,t)] constructor, the list [vs] contains all free variables
   in scope (sorted by their keys), the integer [n] counts the number of bound
   variables that have a reserved slot in the environment, and finally, [t] is
   a representation of the objects (with variables) using a closure.

   Note that the closure [t] is intended to receive the environment as  second
   argument, and the position of the free variables of [vs] in the environment
   as a first argument.

   Important remark: to be able to efficiently implement substitution, we make
   sure that the [varpos] map (given as first argument to closures) is used at
   most once for each variable, even if a variable appears many times. *)

datatype box_type(a:type) =
  | Box of a 
  | Env of (list0(any_var), size_t, closure(a))

and var_type(a:type) = 
  Var of @{
    var_key   = int,       (* Unique identifier.          *)
    var_name  = string,    (* Name as a free variable.    *)
    var_mkfree= mkfree(a), (* Function to build a term.   *)
    var_box   = box(a)     (* Variable as a boxed object. *)
  }

(* Variable of any type (using an existential). *)
and any_var =
  | V of [a:type](ref(var_t(a)))

assume box_type(a) = box_type(a)
assume var_type(a) = var_type(a)

(* var_t methods *)
implement name_of(Var(x)) = x.var_name
implement box_var(Var(x)) = x.var_box
implement compare_vars(Var(x), Var(y)) = y.var_key - x.var_key
implement eq_vars(Var(x), Var(y)) = x.var_key = y.var_key
implement uid_of(Var(x)) = x.var_key

implement print_var(x) = fprint_var(stdout_ref, x)
implement prerr_var(x) = fprint_var(stderr_ref, x)
implement fprint_var(out, x) = fprint!(out, name_of(x), "$", uid_of(x))

implement(a) fprint_val<var_t(a)>(out, x) = fprint_var(out, x)
implement(a) gcompare_val_val<var_t(a)>(x, y) = $effmask_all(compare_vars(x, y))

fun print_any_var(V(x): any_var) = fprint_var(stdout_ref, !x)
fun prerr_any_var(V(x): any_var) = fprint_var(stderr_ref, !x)
fun fprint_any_var(out: FILEref, V(x): any_var) = fprint_var(out, !x)

implement(a) fprint_val<any_var>(out, x) = fprint_any_var(out, x)

(* mvar_t methods *)
implement names_of(xs) = array0_map(xs, lam(x) => name_of(x))
implement uids_of(xs) = array0_map(xs, lam(x) => uid_of(x))

(* The [merge_uniq] and [remove] functions manipulate variables lists found in
   boxed object of the form [Env(_,_,_)]. They rely on the fact that such list
   is always sorted in the order of variable keys, and has no duplicates. *)

fn merge_uniq(
  l1: list0(any_var), 
  l2: list0(any_var)
): list0(any_var) = let
  fun merge_uniq(
    acc: list0(any_var), 
    l1: list0(any_var), 
    l2: list0(any_var)
  ): list0(any_var) =
    case (l1, l2) of
    | (nil0(), _) => list0_reverse_append(acc, l2)
    | (_, nil0()) => list0_reverse_append(acc, l1)
    | ((vx as V(x)) :: xs, (vy as V(y)) :: ys) => let
      val x = !x
      val y = !y
    in
      if uid_of(x) = uid_of(y) 
      then merge_uniq(vx :: acc, xs, ys)
      else if uid_of(x) < uid_of(y)
      then merge_uniq(vx :: acc, xs, l2)
      else merge_uniq(vy :: acc, l1, ys)
    end
in merge_uniq(nil0, l1, l2)
end

fn remove{a:type}(x: var_t(a), xs: list0(any_var)): list0(any_var) = let
  val var_key = uid_of(x)
  fun remove(acc, xs: list0(any_var)) =
    case xs of
    | (v as V(x)) :: l when uid_of(!x) < var_key => remove(v :: acc, l)
    | V(x) :: l when uid_of(!x) = var_key => list0_revapp(acc, l)
    | _ => (
      println!("remove_not_found");
      $raise Not_found)
in
  remove(nil0, xs)
end

fn minimize_aux_prefix{a:type}(
  size: size_t, 
  n: size_t, 
  t: cfun(env,a), 
  env: env
): a = let
  val new_env = env_create(size, size + n)
in
  env_blit(env, new_env, size);
  t(new_env)
end

fn minimize_aux{a:type}(
  tab: array0(size_t),
  n: size_t,
  t: cfun(env,a),
  env: env
): a = let
  val size = tab.size()
  val new_env = env_create(size, size + n)
in
  array0_iforeach(tab, lam(i, x) => env_set(new_env, i, env_get(x, env)));
  t(new_env)
end

(* Function [minimize vs n cl] constructs a minimal closure equivalent to [cl]
   and only containing variables of [vs]. Additionally, the function builds an
   environment with [n] extra slots. *)

fn minimize{a:type}(vs: list0(any_var), n: size_t, t: closure(a)): closure(a) =
  if n = 0 then t
  else lam(vp, env) => let
    val size = vs.length()
    val tab = array0_make_elt<size_t>(size, i2sz(0))
    var pr1: bool with pf1 = true
    var vp1: varpos with pf2 = varpos_empty()
    fun loop{l1,l2:addr}(
      pf1: !bool@l1, pf2: !varpos@l2
    | i: size_t, vs: list0(any_var), pr1: ptr(l1), vp1: ptr(l2)): void =
      case vs of
      | V(x) :: vs => let
        val x = !x
        val _ = println!("varpos_find(", x, ")")
        val j = varpos_find(vp, uid_of(x))
        val _ = if i != j then !pr1 := false
        val _ = tab[i] := j
        val _ = !vp1 := varpos_insert(!vp1, uid_of(x), i)
      in
        loop(pf1, pf2 | i + 1, vs, pr1, vp1)
      end
      | nil0() => ()
    val _ = loop(pf1, pf2 | i2sz(0), vs, addr@pr1, addr@vp1)
    val vp1 = vp1
    val t1 = lam(env: env) =<cloref1> t(vp1, env)
  in
    if pr1
    then minimize_aux_prefix(g1int2uint(size), n, t1, env)
    else minimize_aux(tab, n, t1, env)
  end

implement box(t) = Box(t)

implement apply_box(f, a) =
  case (f, a) of
  | (Box(f), Box(a)) => Box(f(a))
  | (Box(f), Env(va, na, ta)) => Env(va, na, map_closure(f, ta))
  | (Env(vf, nf, tf), Box(a)) => Env(vf, nf, app_closure(tf, a))
  | (Env(vf, nf, tf), Env(va, na, ta)) =>
    Env(merge_uniq(vf, va), i2sz(0), 
        comp_closure(minimize(vf, nf, tf), minimize(va, na, ta)))

implement occur(v, b) =
  case b of
  | Box(_) => false
  | Env(vs, _, _) => 
    list0_exists(vs, lam(V(x)) => uid_of(!x) = uid_of(v))

implement is_closed(b) =
  case b of
  | Box(_) => true
  | Env(_, _, _) => false

implement box_apply(f, a) =
  case a of
  | Box(a) => Box(f(a))
  | Env(vs, na, ta) => Env(vs, na, map_closure(f, ta))

implement box_apply2(f, ta, tb) =
  apply_box(box_apply(lam(a)(b) => f(a, b), ta), tb)

implement box_apply3(f, ta, tb, tc) = 
  apply_box(box_apply2(lam(a, b)(c) => f(a, b, c), ta, tb), tc)

implement box_apply4(f, ta, tb, tc, td) = 
  apply_box(box_apply3(lam(a, b, c)(d) => f(a, b, c, d), ta, tb, tc), td)

implement box_pair(x, y) = 
  box_apply2(lam(x, y) => '(x, y), x, y)

implement box_triple(x, y, z) = 
  box_apply3(lam(x, y, z) => '(x, y, z), x, y, z)

implement box_opt0(o) =
  case o of
  | None0( ) => box(None0())
  | Some0(e) => box_apply(lam(e) => Some0(e), e)

implement box_opt(o) =
  case o of
  | None( ) => box(None())
  | Some(e) => box_apply(lam(e) => Some(e), e)

implement box_opt_vt(o) =
  case o of
  | ~None_vt( ) => box(None())
  | ~Some_vt(e) => box_apply(lam(e) => Some(e), e)

implement unbox(b) =
  case b of
  | Box(t) => t
  | Env(vs, nb, t) => let
    val nbvs = g1int2uint(vs.length())
    val env = env_create(nbvs, nbvs + nb)
    var vp1: varpos with pf = varpos_empty()
    fun loop{l:addr}(
      pf: !varpos@l
    | cur: size_t, vs: list0(any_var), vp1: ptr(l)): void = 
      case vs of
      | V(v) :: vs => let
        val v = !v
        val Var(x) = v
        val mk_free = x.var_mkfree
        val v1 = mk_free(v)
      in
        env_set(env, cur, v1);
        !vp1 := varpos_insert(!vp1, x.var_key, cur);
        loop(pf | cur + 1, vs, vp1)
      end
      | nil0 => ()
    val _ = loop(pf | i2sz(0), vs, addr@vp1)
  in 
    t(vp1, env)
  end

(* Representation of binders ************************************************)

typedef binder_type(a,b) = '{
  b_name  = string,    (* Preferred name for the bound variable. *)
  b_bind  = bool,      (* Indicates whether the variable occurs. *)
  b_rank  = size_t,    (* Number of remaining free variables.    *)
  b_mkfree= mkfree(a), (* Injection of variables into domain.    *)
  b_value = cfun(a,b)  (* Substitution function.                 *) 
}

assume binder_type(a,b) = binder_type(a,b)

implement subst(b, x) = b.b_value(x)
implement binder_name(b) = b.b_name
implement binder_occur(b) = b.b_bind
implement binder_const(b) = ~b.b_bind
implement binder_closed(b) = b.b_rank = 0
implement binder_rank(b) = b.b_rank
implement binder_compose(b, f) = '{
  b_name   = b.b_name,
  b_bind   = b.b_bind,
  b_rank   = b.b_rank,
  b_mkfree = b.b_mkfree,
  b_value  = lam(x) =<cloref1> f(b.b_value(x))
}
implement bind_apply(b, arg) = 
  box_apply2(lam(b, x) => subst(b, x), b, arg)

typedef mbinder_type(a,b) = '{
  mb_names = array0(string),     (* Preferred names for the bound variables.   *)
  mb_binds = array0(bool),       (* Indicates whether the variables occur.     *)
  mb_rank  = size_t,             (* Number of remaining free variables.        *)
  mb_mkfree = array0(mkfree(a)), (* Injections of variables into domain.       *)
  mb_value  = cfun(array0(a), b) (* Substitution function.                     *)
}

assume mbinder_type(a,b) = mbinder_type(a,b)

implement msubst(b, xs) = b.mb_value(xs)
implement mbinder_arity(b) = let 
  val mb_names = b.mb_names
  val len = mb_names.length()
in
  g1int2uint(len)
end
implement mbinder_names(b) = b.mb_names
implement mbinder_occur(b) = b.mb_binds
implement mbinder_const(b) = array0_forall(b.mb_binds, lam(x) => ~x)
implement mbinder_rank(b) = b.mb_rank
implement mbinder_compose(b, f) = '{
  mb_names = b.mb_names, 
  mb_binds = b.mb_binds,
  mb_rank = b.mb_rank,
  mb_mkfree = b.mb_mkfree,
  mb_value = lam(x) =<cloref1> f(b.mb_value(x))
}
implement mbind_apply(b, arg) =
  box_apply2(lam(b, xs) => msubst(b, xs), b, arg)

(* Variable creation ********************************************************)

fn build_var_aux{a:type}(key:int)(vp: varpos, env: env): a = let
  val _ = println!("varpos_find_aux(", key, " in ", funmap_listize(vp), ")");
in
  try env_get(varpos_find(vp, key) , env) 
  with
  | ~Not_found() => (
    println!("varpos_not_found_aux(", key, ")");
    $raise Not_found())
end

fn build_var{a:type}(
  var_key: int, 
  var_mkfree: mkfree(a), 
  name: string
): var_t(a) = let
  val _ = println!("build_var(", name, ")")
  val r: ref(var_t(a)) = ref($UN.cast(0))
  val x: var_t(a) = try Var@{ 
    var_key   = var_key, 
    var_name  = name, 
    var_mkfree= var_mkfree, 
    var_box   = Env(V(r) :: nil0, i2sz(0), build_var_aux(var_key))
  }
  with ~Not_found() => (
    println!("build_var_failed(", name, ")"); $raise Not_found)
  val _ = !r := x
in 
  x 
end

implement new_var(mkfree, name) = 
  build_var(fresh_key(), mkfree, name)

implement copy_var(Var(x), mkfree, name) =
  build_var(x.var_key, mkfree, name)

implement unbind(b) = let
  val x = new_var(b.b_mkfree, b.b_name)
in
  (x, subst(b, b.b_mkfree(x)))
end

implement unbind2(b1, b2) = let
  val x = new_var(b1.b_mkfree, b1.b_name)
  val v = b1.b_mkfree(x)
in
  (x, subst(b1, v), subst(b2, v))
end

implement eq_binder(eq, f, g) = 
  let val (_, t, u) = unbind2(f, g) in eq(t, u) end

implement unmbind{a,_}(b) = let
  val arity: size_t = mbinder_arity(b)
  val names = b.mb_names
  val mkfree = b.mb_mkfree
  val xs = array0_make_elt<var_t(a)>(arity, $UN.cast(0))
  val vs = array0_make_elt<a>(arity, $UN.cast(0))
  fun loop(i: size_t) = if i < arity then (
    xs[i] := new_var(mkfree[i], names[i]);
    vs[i] := mkfree[i](xs[i]);
    loop(i + 1))
  val _ = loop(i2sz(0))
in
  (xs, msubst(b, vs))
end

implement unmbind2{a,_}(b1, b2) = let
  val arity1 = mbinder_arity(b1)
  val arity2 = mbinder_arity(b2)
  val names = b1.mb_names
  val mkfree = b1.mb_mkfree
  val _ = if arity1 != arity2 then $raise Arity_mismatch
  val xs = array0_make_elt<var_t(a)>(arity1, $UN.cast(0))
  val vs = array0_make_elt<a>(arity1, $UN.cast(0))
  fun loop(i: size_t) = if i < arity1 then (
    xs[i] := new_var(mkfree[i], names[i]);
    vs[i] := mkfree[i](xs[i]);
    loop(i + 1))
  val _ = loop(i2sz(0))
in
  (xs, msubst(b1, vs), msubst(b2, vs))
end

implement eq_mbinder(eq, f, g) =
  (mbinder_arity(f) = mbinder_arity(g)) &&
  let val (_, t, u) = unmbind2(f, g) in eq(t, u) end

(* Implementation of [bind_var] *********************************************)

fn build_binder{a,b:type}(
  Var(x): var_t(a), 
  b_rank: size_t, 
  b_bind: bool, 
  b_value: cfun(a,b)
): binder(a,b) = '{
  b_name   = x.var_name,
  b_bind   = b_bind,
  b_rank   = b_rank,
  b_mkfree = x.var_mkfree,
  b_value  = b_value
}

fn bind_var_aux1{a,b:type}(
  n: size_t, 
  t: cfun(env,a)
)(arg: b): a = let
  val env = env_create(i2sz(1), n + 1)
  val _ = env_set(env, i2sz(0), arg)
in
  t(env)
end

fn bind_var_aux2{a,b:type}(
  rank: size_t, 
  t: cfun(env,a), 
  env: env
)(arg: b): a = let
  val next = env_get_next_free(env)
in
  if next = rank then (
    env_set_next_free(env, next + 1);
    env_set(env, next, arg);
    t(env))
  else let
    val env = env_copy(env)
    val _ = env_set_next_free(env, rank + 1);
    val _ = env_set(env, rank, arg);
    // TODO: check if this is needed
    // implement intrange_foreach$fwork<void>(i, env0) =
    //   env_set(env, g0int2uint(i), $UN.cast{any}(0))
    // val _ = intrange_foreach(sz2i(rank + 1), sz2i(next))
  in
    t(env)
  end
end

fn bind_var_aux3{a,b:type}(
  x: var_t(a),
  rank: size_t,
  t: cfun(env, b),
  env: env
): binder(a,b) =
  build_binder(x, rank, true, bind_var_aux2(rank, t, env))

fn bind_var_aux4{a,b:type}(
  t: cfun(env, b), 
  env: env
)(_: a): b = t(env)

fn bind_var_aux5{a,b:type}(
  x: var_t(a),
  rank: size_t,
  t: cfun(env, b),
  env: env
): binder(a,b) =
  build_binder(x, rank, false, bind_var_aux4(t, env))

implement bind_var(x, b) = let
  val _ = println!("bind_var(", x, ")")
in
  case b of
  | Box(t) => Box(build_binder(x, i2sz(0), false, lam(_) => t))
  | Env(vs, n, t) => 
    try 
      case vs of
      | V(y) :: nil0() => let
        val _ = println!("bind_var1")
        val Var(x0) = x
        val Var(y0) = !y
        val _ = 
          if x0.var_key != y0.var_key then (
            println!("not_found");
            $raise Not_found)
        val r = i2sz(0)
        val t = lam(env: env) =<cloref1> 
          t(varpos_unit(x0.var_key, r), env)
        val value = bind_var_aux1(n, t)
      in
        Box(build_binder(x, i2sz(0), true, value))
      end
      | _ => let
        val _ = println!("bind_var2")
        val vs = remove(x, vs)
        val Var(x0) = x
        val cl = lam(vp, env: env) =<cloref1> let
          val r = i2sz(vs.length())
          val _ = println!("insert(", x, ", ", r, ")")
          val t = lam(env: env) =<cloref1> 
            t(varpos_insert(vp, x0.var_key, r), env)
        in
          bind_var_aux3(x, r, t, env)
        end
      in
        Env(vs, n + 1, cl)
      end
    with
    | ~Not_found() => let
      val value = lam(vp, env: env) =<cloref1> let
        val t = lam(env: env) =<cloref1> t(vp, env)
        val rank = g1int2uint(vs.length()) 
      in
        bind_var_aux5(x, rank, t, env)
      end
    in
      Env(vs, n, value)
    end
end

implement box_binder(f, b) =
  if binder_closed(b) then box(b)
  else let
    val (x, t) = unbind b
  in
    bind_var(x, f(t))
  end

(* Implementation of [bind_mvar] ********************************************)

fn check_arity{a:type}(xs: mvar(a), args: array0(a)): void =
  if xs.size() != args.size() then $raise Arity_mismatch

fn bind_mvar_aux0{a,b:type}(xs: mvar(a), t: b)(args: array0(a)): b =
  (check_arity(xs, args); t)

fn bind_mvar_aux1{a,b:type}(
  m: size_t, 
  xs: mvar(a), 
  mb_binds: array0(bool),
  t: cfun(env,b)
)(args: array0(a)): b = let
  val _ = check_arity(xs, args)
  val v = env_create(i2sz(0), m)
  val sz = xs.size()
  var pos: size_t with pf = i2sz(0)
  fun loop{l:addr}(
    pf: !size_t@l 
  | i: size_t, pos: ptr(l)): void = if i < sz then (
    if mb_binds[i] then (
      env_set(v, !pos, args[i]);  
      !pos := !pos + 1);
    loop(pf | i + 1, pos))
  val _ = loop(pf | i2sz(0), addr@pos)
  val _ = env_set_next_free(v, pos)
in
  t(v)
end

fn bind_mvar_aux2{a,b:type}(
  xs: mvar(a),
  t: cfun(env,b),
  env: env
)(args: array0(a)): b =
  (check_arity(xs, args); t(env))

fn bind_mvar_aux3{a,b:type}(
  xs: mvar(a),
  t: cfun(env,b),
  mb_names: array0(string),
  mb_rank: size_t, 
  mb_binds: array0(bool),
  mb_mkfree: array0(mkfree(a)),
  env: env
): mbinder(a, b) = '{
  mb_names  = mb_names,
  mb_binds  = mb_binds,
  mb_rank   = mb_rank,
  mb_mkfree = mb_mkfree,
  mb_value  = bind_mvar_aux2(xs, t, env)
}

fn bind_mvar_aux4{a,b:type}(
  xs: mvar(a),
  t: cfun(env,b),
  mb_rank: size_t,
  mb_binds: array0(bool),
  env: env
)(args: array0(a)): b = let
  val _ = check_arity(xs, args)
  val sz = xs.size()
  val next = env_get_next_free(env)
  var cur_pos: size_t with pf = mb_rank 
  fun loop{l:addr}(
    pf: !size_t@l 
  | i: size_t, cur_pos: ptr(l), env: env): void = if i < sz then (
    if mb_binds[i] then (
      env_set(env, !cur_pos, args[i]);
      !cur_pos := !cur_pos + 1); 
    loop(pf | i + 1, cur_pos, env))
in
  if next = mb_rank then let
    val _ = loop(pf | i2sz(0), addr@cur_pos, env)
    val _ = env_set_next_free(env, cur_pos)
  in
    t(env)
  end 
  else let
    val env = env_copy(env)
    val _ = loop(pf | i2sz(0), addr@cur_pos, env)
    val _ = env_set_next_free(env, cur_pos)
    // TODO: check if this is needed
    // implement intrange_foreach$fwork<void>(i, env0) =
    //   env_set(env, g0int2uint(i), $UN.cast{any}(0))
    // val _ = intrange_foreach(sz2i(cur_pos), sz2i(next))
  in
    t(env)
  end
end

fn bind_mvar_aux5{a,b:type}(
  xs: mvar(a),
  t: cfun(env,b),
  mb_names: array0(string),
  mb_rank: size_t,
  mb_binds: array0(bool),
  mb_mkfree: array0(mkfree(a)),
  env: env
): mbinder(a,b) = '{
  mb_names  = mb_names,
  mb_binds  = mb_binds,
  mb_rank   = mb_rank,
  mb_mkfree = mb_mkfree,
  mb_value  = bind_mvar_aux4(xs, t, mb_rank, mb_binds, env) 
}

implement bind_mvar(xs, b) = let
  val mb_mkfree = array0_map(xs, lam(x) => 
    let val Var(x) = x in x.var_mkfree end)
in
  case b of
  | Box(t) => let
    val mb_binds = array0_map(xs, lam(x) => false)
    val mb_names = array0_map(xs, lam(x) => name_of(x))
    val mb_value = bind_mvar_aux0(xs, t)
  in Box '{
    mb_names  = mb_names,
    mb_binds  = mb_binds,
    mb_rank   = i2sz(0),
    mb_mkfree = mb_mkfree,
    mb_value  = mb_value
  }
  end
  | Env(vs, n, t) => let 
    val _ = println!("bind_mvar0(", xs, ")")
    val sz = xs.size()
    val keys: array0(int) = array0_map(xs, lam(x) => 0)
    val vss = array0_map(xs, lam(x) => vs)
    var vs: list0(any_var) with pf1 = vs
    var m: size_t with pf2 = n
    val _ = println!("bind_mvar1")
    fun loop{l1,l2:addr}(
      pf1: !list0(any_var)@l1, pf2: !size_t@l2
    | i: int, vs: ptr(l1), m: ptr(l2)): void = if (0 <= i) then (
      let
        val v = xs[i]
        val _ = println!("removing(", xs, "with",  i, " = ",  v, ")");
        val _ = println!("from(", !vs, ")");
        val Var(x0) = v
        val _ = try
          !vs := remove(v, !vs);
          println!("removed(", v, ")");
          println!("after_remove(", !vs, ")");
          !m := !m + 1;
          keys[i] := x0.var_key
        with ~Not_found() => (
          println!("not_found(", v, ")");
          keys[i] := ~1)
      in
        vss[i] := !vs
      end;
      loop(pf1, pf2 | i - 1, vs, m))
    val _ = loop(pf1, pf2 | sz2i(sz) - 1, addr@vs, addr@m)
    val _ = println!("bind_mvar1")
    val vs = vs
  in
    case vs of
    | nil0() => let
      val mb_names = array0_map(xs, lam(x) => "")
      val mb_binds = array0_map(xs, lam(x) => false)
      var cur_pos: size_t with pf1 = i2sz(0)
      var vp: varpos with pf2 = varpos_empty()
      fun loop{l1,l2:addr}(
        pf1: !size_t@l1, pf2: !varpos@l2
      | i: size_t, cur_pos: ptr(l1), vp: ptr(l2)): void = if i < sz then (
        let
          val Var(x0) = xs[i]
          val key = keys[i]
        in
          mb_names[i] := x0.var_name;
          if key >= 0 then (
            !vp := varpos_insert(!vp, key, !cur_pos);
            !cur_pos := !cur_pos + 1;
            mb_binds[i] := true)
          else 
            mb_binds[i] := false
        end; loop(pf1, pf2 | i + 1, cur_pos, vp))
      val _ = loop(pf1, pf2 | i2sz(0), addr@cur_pos, addr@vp)
      val vp = vp
      val t = lam(env: env) =<cloref1> t(vp, env)
      val mb_value = bind_mvar_aux1(m, xs, mb_binds, t)
    in Box '{
      mb_names  = mb_names,
      mb_binds  = mb_binds,
      mb_rank   = i2sz(0),
      mb_mkfree = mb_mkfree,
      mb_value  = mb_value
    }
    end
    | _ when m = n => let
      val cl = lam(vp, env: env) =<cloref1> let
        val mb_rank = g1int2uint(vs.length())
        val mb_binds = array0_map(xs, lam(x) => false)
        val mb_names = array0_map(xs, lam(x) => 
          let val Var(x0) = x in x0.var_name end)
        val t = lam(env: env) =<cloref1> t(vp, env)
      in
        bind_mvar_aux3(xs, t, mb_names, mb_rank, mb_binds, mb_mkfree, env)
      end
    in
      Env(vs, n, cl)
    end
    | _ => let
      val cl = lam(vp, env: env) =<cloref1> let
        val mb_names = array0_map(xs, lam(x) => "")
        val mb_binds = array0_map(xs, lam(x) => false)
        val mb_rank = g1int2uint(vs.length())
        var cur_pos: size_t with pf1 = mb_rank
        var vp: varpos with pf2 = vp
        fun loop{l1,l2:addr}(
          pf1: !size_t@l1, pf2: !varpos@l2
        | i: size_t, cur_pos: ptr(l1), vp: ptr(l2)): void = if i < sz then (
          let
            val Var(x0) = xs[i]
            val key = keys[i]
          in
            mb_names[i] := x0.var_name;
            if key >= 0 then (
              !vp := varpos_insert(!vp, key, !cur_pos);
              !cur_pos := !cur_pos + 1;
              mb_binds[i] := true)
            else 
              mb_binds[i] := false
          end; loop(pf1, pf2 | i + 1, cur_pos, vp))
        val _ = loop(pf1, pf2 | i2sz(0), addr@cur_pos, addr@vp)
        val vp = vp
        val t = lam(env: env) =<cloref1> t(vp, env)
      in
        bind_mvar_aux5(xs, t, mb_names, mb_rank, mb_binds, mb_mkfree, env)
      end
    in
      Env(vs, m, cl)
    end
  end
end
