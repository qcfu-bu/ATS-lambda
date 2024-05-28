(* header *)
#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"
#staload UN = "prelude/SATS/unsafe.sats"

(* statics *)
#staload "./../sats/bindlib.sats"

local 
val c: ref(int) = ref(~1)
in
fun fresh_key(): int = begin
  !c := !c + 1; !c
end
fun reset_counter(): void = !c := ~1
end

(* A placeholder type for any boxed type. *)
abstype any_boxed = ptr
typedef any = any_boxed

(* Environment **************************************************************)

(* The [Bindlib] library uses environments to store values associated to bound
   variables upon substitution. We rely on unsafe.sats to store variables with 
   potentially different types in a single array. *)

abst@ype env
extern fun create(next_free: int, size: int): env
extern fun set{a: type}(env: env, i: int, e: a): void
extern fun blit(src: env, dst: env, len: int): void
extern fun copy(env: env): env
extern fun get_next_free(env: env): int
extern fun set_next_free(env: env, n: int): void

local
assume env = @{ tab= array0(any), next_free= ref(int) }
in
implement create(next_free, size) = @{
  tab= array0_make_elt<any>(g0int2uint_int_size(size), $UN.cast{any}(0))
, next_free= ref<int>(next_free)
}

implement set{a}(env, i, e) = let
  val tab = env.tab
in
  tab[i] := $UN.cast{any}(e)
end

implement blit(src, dst, len) = let
  val src_tab = src.tab
  val dst_tab = dst.tab
  implement intrange_foreach$fwork<void>(i, env) =
    dst_tab[i] := src_tab[i]; 
  val _ = intrange_foreach(0, len)
in end

implement copy(env) = let 
  val tab = array0_copy<any>(env.tab) 
  val next_free = ref(!(env.next_free))
in 
  @{ tab= tab, next_free= next_free }
end

implement get_next_free(env) = !(env.next_free)
implement set_next_free(env, n) = !(env.next_free) := n
end

(* Closure representation ***************************************************)

(* In the internals of [Bindlib], variables are identified by a unique integer
   key. Closures are then formed by mapping each free variable to a slot in an
   environment (of type [env]), but not directly. The mapping is established
   using a map (of type [varpos]) which associates variable keys to indices in
   the environment. *)

typedef varpos = map(int, int)
typedef closure(a: type) = (varpos, env) -<cloref1> a

fun map_closure{a,b:type}(f: a -<cloref1> b, cla: closure(a)): closure(b) = 
  lam(vs, env) => f(cla(vs, env))

fun app_closure{a,b:type}(clf: closure(a -<cloref1> b), a: a): closure(b) =
  lam(vs, env) => clf(vs, env)(a)

fun compose_closure{a,b:type}(clf: closure(a -<cloref1> b), cla: closure(a)): closure(b) =
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

datatype box(a:type) =
  | Box of a 
  | Env of (list0(any_var), int, closure(a))

and any_var =
  | V of [a:type](var_t(a))

assume var_boxed(a) = '{
  var_key= int
, var_name= string
, var_mkfree= var_t(a) -<cloref1> a
, var_box= box(a)
}

