#include "share/atspre_staload_libats_ML.hats"

abstype box_type(a:type)
typedef box(a) = box_type(a)

abstype var_type(a:type)
typedef var_t(a) = var_type(a)
typedef mkfree(a) = var_t(a) -<cloref1> a

fn name_of{a:type}(var_t(a)): string
fn box_var{a:type}(var_t(a)): box(a)
fn compare_vars{a:type}(var_t(a), var_t(a)): int
fn eq_vars{a:type}(var_t(a), var_t(a)): bool
fn uid_of{a:type}(var_t(a)): int

fn fprint_var{a:type}(FILEref, var_t(a)): void
fn print_var{a:type}(var_t(a)): void
fn prerr_var{a:type}(var_t(a)): void

overload fprint with fprint_var
overload print with print_var
overload prerr with prerr_var

typedef mvar(a:type) = array0(var_t(a))

fn names_of{a:type}(mvar(a)): array0(string)
fn uids_of{a:type}(mvar(a)): array0(int)

fn box{a:type}(a): box(a)
fn apply_box{a,b:type}(box(cfun(a,b)), box(a)): box(b)
fn occur{a,b:type}(var_t(a), box(b)): bool
fn is_closed{a:type}(box(a)): bool
fn box_apply{a,b:type}(cfun(a,b), box(a)): box(b)
fn box_apply2{a,b,c:type}(cfun2(a,b,c), box(a), box(b)): box(c)
fn box_apply3{a,b,c,d:type}(cfun3(a,b,c,d), box(a), box(b), box(c)): box(d)
fn box_apply4{a,b,c,d,e:type}(cfun4(a,b,c,d,e), box(a), box(b), box(c), box(d)): box(e)
fn box_pair{a,b:type}(box(a), box(b)): box('(a, b))
fn box_triple{a,b,c:type}(box(a), box(b), box(c)): box('(a,b,c))
fn box_opt0{a:type}(option0(box(a))): box(option0(a))
fn box_opt{a:type}(Option(box(a))): box(Option(a))
fn box_opt_vt{a:type}(Option_vt(box(a))): box(Option(a))
fn unbox{a:type}(box(a)): a

abstype binder_type(a:type,b:type)
typedef binder(a,b) = binder_type(a,b)

fn subst{a,b:type}(binder(a,b), a): b
fn binder_name{a,b:type}(binder(a,b)): string
fn binder_occur{a,b:type}(binder(a,b)): bool
fn binder_const{a,b:type}(binder(a,b)): bool
fn binder_closed{a,b:type}(binder(a,b)): bool
fn binder_rank{a,b:type}(binder(a,b)): size_t
fn binder_compose{a,b,c:type}(binder(a,b), cfun(b,c)): binder(a,c)
fn bind_apply{a,b:type}(box(binder(a,b)), box(a)): box(b)

abstype mbinder_type(a:type,b:type)
typedef mbinder(a,b) = mbinder_type(a,b)

fn msubst{a,b:type}(mbinder(a,b),  array0(a)): b
fn mbinder_arity{a,b:type}(mbinder(a,b)): size_t
fn mbinder_names{a,b:type}(mbinder(a,b)): array0(string)
fn mbinder_occur{a,b:type}(mbinder(a,b)): array0(bool)
fn mbinder_const{a,b:type}(mbinder(a,b)): bool
fn mbinder_rank{a,b:type}(mbinder(a,b)): size_t
fn mbinder_compose{a,b,c:type}(mbinder(a,b), cfun(b,c)): mbinder(a,c)
fn mbind_apply{a,b:type}(box(mbinder(a,b)), box(array0(a))): box(b)

fn new_var{a:type}(mkfree(a), string): var_t(a)
fn copy_var{a,b:type}(var_t(a), mkfree(b), string): var_t(b)

fn unbind{a,b:type}(binder(a,b)): (var_t(a),b)
fn unbind2{a,b:type}(binder(a,b), binder(a,b)): (var_t(a),b,b)
fn eq_binder{a,b:type}(cfun2(b,b,bool), binder(a,b), binder(a,b)): bool

fn unmbind{a,b:type}(mbinder(a,b)): (mvar(a), b)
fn unmbind2{a,b:type}(mbinder(a,b), mbinder(a,b)): (mvar(a),b,b)
fn eq_mbinder{a,b:type}(cfun2(b,b,bool), mbinder(a,b), mbinder(a,b)): bool

fn bind_var{a,b:type}(var_t(a), box(b)): box(binder(a, b))
fn box_binder{a,b:type}(cfun(b,box(b)), binder(a,b)): box(binder(a,b))

fn bind_mvar{a,b:type}(mvar(a), box(b)): box(mbinder(a, b))
fn box_mbinder{a,b:type}(cfun(b,box(b)), mbinder(a,b)): box(mbinder(a,b))