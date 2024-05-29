#include "share/atspre_staload_libats_ML.hats"

abstype box_type(a:type)
typedef box(a) = box_type(a)

abstype var_type(a:type)
typedef var_t(a) = var_type(a)
typedef mkfree(a) = var_t(a) -<cloref1> a

fn name_of{a:type}(x: var_t(a)): string
fn box_var{a:type}(x: var_t(a)): box(a)
fn compare_vars{a:type}(x: var_t(a), y: var_t(a)): int
fn eq_vars{a:type}(x: var_t(a), y: var_t(a)): bool
fn uid_of{a:type}(x: var_t(a)): int

fn fprint_var{a:type}(out: FILEref, m: var_t(a)): void
fn print_var{a:type}(x: var_t(a)): void
fn prerr_var{a:type}(x: var_t(a)): void

overload fprint with fprint_var
overload print with print_var
overload prerr with prerr_var

typedef mvar(a:type) = array0(var_t(a))

fn names_of{a:type}(xs: mvar(a)): array0(string)
fn uids_of{a:type}(xs: mvar(a)): array0(int)

fn box{a:type}(t: a): box(a)
fn apply_box{a,b:type}(f: box(cfun(a,b)), a: box(a)): box(b)
fn occur{a,b:type}(v: var_t(a), b: box(b)): bool
fn is_closed{a:type}(v: box(a)): bool
fn box_apply{a,b:type}(f: cfun(a,b), a: box(a)): box(b)
fn box_apply2{a,b,c:type}(f: cfun2(a,b,c), a: box(a), b: box(b)): box(c)
fn box_apply3{a,b,c,d:type}(f: cfun3(a,b,c,d), a: box(a), b: box(b), c: box(c)): box(d)
fn box_apply4{a,b,c,d,e:type}(f: cfun4(a,b,c,d,e), a: box(a), b: box(b), c: box(c), d: box(d)): box(e)
fn box_pair{a,b:type}(x: box(a), y: box(b)): box('(a, b))
fn box_triple{a,b,c:type}(x: box(a), y: box(b), z: box(c)): box('(a,b,c))
fn box_opt0{a:type}(o: option0(box(a))): box(option0(a))
fn box_opt{a:type}(o: Option(box(a))): box(Option(a))
fn box_opt_vt{a:type}(o: Option_vt(box(a))): box(Option(a))
fn unbox{a:type}(b: box(a)): a

abstype binder_type(a:type,b:type)
typedef binder(a,b) = binder_type(a,b)

fn subst{a,b:type}(b: binder(a,b), x: a): b
fn binder_name{a,b:type}(b: binder(a,b)): string
fn binder_occur{a,b:type}(b: binder(a,b)): bool
fn binder_const{a,b:type}(b: binder(a,b)): bool
fn binder_closed{a,b:type}(b: binder(a,b)): bool
fn binder_rank{a,b:type}(b: binder(a,b)): size_t
fn binder_compose{a,b,c:type}(b: binder(a,b), f: cfun(b,c)): binder(a,c)
fn bind_apply{a,b:type}(b: box(binder(a,b)), b: box(a)): box(b)

abstype mbinder_type(a:type,b:type)
typedef mbinder(a,b) = mbinder_type(a,b)

fn msubst{a,b:type}(b: mbinder(a,b), xs: array0(a)): b
fn mbinder_arity{a,b:type}(b: mbinder(a,b)): size_t
fn mbinder_names{a,b:type}(b: mbinder(a,b)): array0(string)
fn mbinder_occur{a,b:type}(b: mbinder(a,b)): array0(bool)
fn mbinder_const{a,b:type}(b: mbinder(a,b)): bool
fn mbinder_rank{a,b:type}(b: mbinder(a,b)): size_t
fn mbinder_compose{a,b,c:type}(b: mbinder(a,b), f: cfun(b,c)): mbinder(a,c)
fn mbind_apply{a,b:type}(b: box(mbinder(a,b)), b: box(array0(a))): box(b)

fn new_var{a:type}(mkfree: mkfree(a), name: string): var_t(a)
fn copy_var{a,b:type}(x: var_t(a), mkfree: mkfree(b), name: string): var_t(b)

fn unbind{a,b:type}(b: binder(a,b)): (var_t(a),b)
fn unbind2{a,b:type}(b1: binder(a,b), b2: binder(a,b)): (var_t(a),b,b)
fn eq_binder{a,b:type}(eq: cfun2(b,b,bool), f: binder(a,b), g: binder(a,b)): bool

fn unmbind{a,b:type}(b: mbinder(a,b)): (mvar(a), b)
fn unmbind2{a,b:type}(b1: mbinder(a,b), b2: mbinder(a,b)): (mvar(a),b,b)
fn eq_mbinder{a,b:type}(eq: cfun2(b,b,bool), f: mbinder(a,b), g: mbinder(a,b)): bool

fn bind_var{a,b:type}(x: var_t(a), b: box(b)): box(binder(a, b))
fn box_binder{a,b:type}(f: cfun(b,box(b)), b: binder(a,b)): box(binder(a,b))

fn bind_mvar{a,b:type}(xs: mvar(a), b: box(b)): box(mbinder(a, b))