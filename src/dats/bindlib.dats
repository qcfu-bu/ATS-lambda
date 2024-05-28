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

(* Counter for generating fresh variable keys (i.e., unique identifiers). *)
local 
val c: ref(int) = ref(~1)
in
fn fresh_key(): int = begin
  !c := !c + 1; !c
end
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
typedef closure(a: type) = (varpos, env) -<cloref1> a

fn varpos_empty(): varpos = funmap_nil<>()

fn varpos_insert(vp: varpos, i: int, x: size_t): varpos = let
  var vp = vp
in
  case funmap_insert(vp, i, x) of
  | ~Some_vt(_) => vp
  | ~None_vt( ) => vp
end

fn varpos_find(vp: varpos, i: int): size_t =
  case funmap_search(vp, i) of
  | ~Some_vt(x) => x
  | ~None_vt( ) => $raise Not_found

fn map_closure{a,b:type}(f: a -<cloref1> b, cla: closure(a)): closure(b) = 
  lam(vs, env) => f(cla(vs, env))

fn app_closure{a,b:type}(clf: closure(a -<cloref1> b), a: a): closure(b) =
  lam(vs, env) => clf(vs, env)(a)

fn comp_closure{a,b:type}(clf: closure(a -<cloref1> b), cla: closure(a)): closure(b) =
  lam(vs, env) =<cloref1> clf(vs, env)(cla(vs, env))

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
  var_struct of @{
    var_key= int, 
    var_name= string, 
    var_mkfree= mkfree(a), 
    var_box= box_t(a)
  }

(* Variable of any type (using an existential). *)
and any_var =
  | V of [a:type](ref(var_t(a)))

assume box_boxed(a) = box_type(a)
assume var_boxed(a) = var_type(a)

(* var_t methods *)
implement name_of(var_struct(x)) = x.var_name
implement box_var(var_struct(x)) = x.var_box
implement compare_vars(var_struct(x), var_struct(y)) = y.var_key - x.var_key
implement eq_vars(var_struct(x), var_struct(y)) = x.var_key = y.var_key
implement uid_of(var_struct(x)) = x.var_key

implement print_var(x) = fprint_var(stdout_ref, x)
implement prerr_var(x) = fprint_var(stderr_ref, x)
implement fprint_var(out, x) = fprint!(out, name_of(x), "$", uid_of(x))

implement(a) gcompare_val_val<var_t(a)>(x, y) = $effmask_all(compare_vars(x, y))
implement(a) fprint_val<var_t(a)>(out, x) = fprint_var(out, x)

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
    | v as V(x) :: l when uid_of(!x) < var_key => remove(v :: acc, l)
    | V(x) :: l when uid_of(!x) = var_key => list0_revapp(acc, l)
    | _ => $raise Not_found 
in
  remove(nil0, xs)
end

fn minimize_aux_prefix{a:type}(
  size: size_t, 
  n: size_t, 
  t: env -<cloref1> a, 
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
  t: env -<cloref1> a,
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
    val tab = array0_make_elt<size_t>(size, g0int2uint(0))
    var pr1: bool with pf1 = true
    var vp1: varpos with pf2 = varpos_empty()
    fun loop{l1,l2:addr}(
      pf1: !bool@l1, pf2: !varpos@l2
    | i: size_t, vs: list0(any_var), pr1: ptr(l1), vp1: ptr(l2)
    ): void =
      case vs of
      | V(x) :: vs => let
        val x = !x
        val j = varpos_find(vp, uid_of(x))
        val _ = if i != j then !pr1 := false
        val _ = tab[i] := j
        val _ = !vp1 := varpos_insert(!vp1, uid_of(x), i)
      in
        loop(pf1, pf2 | i + 1, vs, pr1, vp1)
      end
      | nil0() => ()
    val _ = loop(pf1, pf2 | g0int2uint(0), vs, addr@pr1, addr@vp1)
    val new_vp = vp1
    val t1 = lam(env: env) =<cloref1> t(new_vp, env)
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
    Env(merge_uniq(vf, va), g0int2uint(0), 
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
    | cur: size_t, vs: list0(any_var), vp1: ptr(l) 
    ): void = 
      case vs of
      | V(v) :: vs => let
        val v = !v
        val var_struct(x) = v
        val mk_free = x.var_mkfree
        val v1 = mk_free(v)
      in
        env_set(env, cur, v1);
        !vp1 := varpos_insert(!vp1, x.var_key, cur);
        loop(pf | cur + 1, vs, vp1)
      end
      | nil0 => ()
    val _ = loop(pf | g0int2uint(0), vs, addr@vp1)
    in t(vp1, env)
    end

fn build_var_aux{a:type}(key:int)(vp: varpos, env: env): a =
  env_get(varpos_find(vp, key) , env)

fn build_var{a:type}(
  var_key: int, 
  var_mkfree: var_t(a) -<cloref1> a, 
  name: string
): var_t(a) = let
  val r: ref(var_t(a)) = ref($UN.cast(0))
  val x: var_t(a) = 
    var_struct @{ 
      var_key= var_key, 
      var_name= name, 
      var_mkfree= var_mkfree, 
      var_box= Env(V(r) :: nil0, g0int2uint(0), build_var_aux(var_key))
    }
  val _ = !r := x
in 
  x 
end

implement new_var(mkfree, name) = 
  build_var(fresh_key(), mkfree, name)