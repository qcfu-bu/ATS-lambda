#include "share/atspre_staload_libats_ML.hats"

abstype box_type(a:type)
typedef box_t(a) = box_type(a)

abstype var_type(a:type)
typedef var_t(a) = var_type(a)
typedef mkfree_t(a) = var_t(a) -<cloref1> a

fn name_of{a:type}(x: var_t(a)): string
fn box_var{a:type}(x: var_t(a)): box_t(a)
fn compare_vars{a:type}(x: var_t(a), y: var_t(a)): int
fn eq_vars{a:type}(x: var_t(a), y: var_t(a)): bool
fn uid_of{a:type}(x: var_t(a)): int

fn fprint_var{a:type}(out: FILEref, m: var_t(a)): void
fn print_var{a:type}(x: var_t(a)): void
fn prerr_var{a:type}(x: var_t(a)): void

overload fprint with fprint_var
overload print with print_var
overload prerr with prerr_var

typedef mvar_t(a:type) = array0(var_t(a))

fn names_of{a:type}(xs: mvar_t(a)): array0(string)
fn uids_of{a:type}(xs: mvar_t(a)): array0(int)

fn box{a:type}(t: a): box_t(a)
fn apply_box{a,b:type}(f: box_t(cfun(a,b)), a: box_t(a)): box_t(b)
fn occur{a,b:type}(v: var_t(a), b: box_t(b)): bool
fn is_closed{a:type}(v: box_t(a)): bool
fn box_apply{a,b:type}(f: cfun(a,b), a: box_t(a)): box_t(b)
fn box_apply2{a,b,c:type}(f: cfun2(a,b,c), a: box_t(a), b: box_t(b)): box_t(c)
fn box_apply3{a,b,c,d:type}(f: cfun3(a,b,c,d), a: box_t(a), b: box_t(b), c: box_t(c)): box_t(d)
fn box_apply4{a,b,c,d,e:type}(f: cfun4(a,b,c,d,e), a: box_t(a), b: box_t(b), c: box_t(c), d: box_t(d)): box_t(e)
fn box_pair{a,b:type}(x: box_t(a), y: box_t(b)): box_t('(a, b))
fn box_triple{a,b,c:type}(x: box_t(a), y: box_t(b), z: box_t(c)): box_t('(a, b, c))
fn box_opt0{a:type}(o: option0(box_t(a))): box_t(option0(a))
fn box_opt{a:type}(o: Option(box_t(a))): box_t(Option(a))
fn box_opt_vt{a:type}(o: Option_vt(box_t(a))): box_t(Option(a))
fn unbox{a:type}(b: box_t(a)): a

abstype binder_type(a:type,b:type)
typedef binder_t(a,b) = binder_type(a,b)

fn subst{a,b:type}(b: binder_t(a,b), x: a): b
fn binder_name{a,b:type}(b: binder_t(a,b)): string
fn binder_occur{a,b:type}(b: binder_t(a,b)): bool
fn binder_const{a,b:type}(b: binder_t(a,b)): bool
fn binder_closed{a,b:type}(b: binder_t(a,b)): bool
fn binder_rank{a,b:type}(b: binder_t(a,b)): size_t
fn binder_compose{a,b,c:type}(b: binder_t(a,b), f: cfun(b,c)): binder_t(a,c)



fn new_var{a:type}(mkfree: mkfree_t(a), name: string): var_t(a)
fn copy_var{a,b:type}(x: var_t(a), mkfree: mkfree_t(b), name: string): var_t(b)

fn unbind{a,b:type}(b: binder_t(a,b)): (var_t(a), b)
fn unbind2{a,b:type}(b1: binder_t(a,b), b2: binder_t(a,b)): (var_t(a), b, b)
fn eq_binder{a,b:type}(eq: cfun2(b,b,bool), f: binder_t(a,b), g: binder_t(a,b)): bool
fn bind_var{a,b:type}(x: var_t(a), b: box_t(b)): box_t(binder_t(a, b))
fn box_binder{a,b:type}(f: cfun(b,box_t(b)), b: binder_t(a,b)): box_t(binder_t(a,b))