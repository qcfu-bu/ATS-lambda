#include "share/atspre_staload_libats_ML.hats"

abstype box_boxed(a:type)
typedef box_t(a) = box_boxed(a)

abstype var_boxed(a:type)
typedef var_t(a) = var_boxed(a)
typedef mkfree(a) = var_t(a) -<cloref1> a

fn name_of{a:type}(x: var_t(a)): string
fn box_var{a:type}(x: var_t(a)): box_t(a)
fn compare_vars{a:type}(x: var_t(a), y: var_t(a)): int
fn eq_vars{a:type}(x: var_t(a), y: var_t(a)): bool
fn uid_of{a:type}(x: var_t(a)): int

typedef mvar_t(a:type) = array0(var_t(a))

fn names_of{a:type}(xs: mvar_t(a)): array0(string)
fn uids_of{a:type}(xs: mvar_t(a)): array0(int)

fn box{a:type}(t: a): box_t(a)
fn apply_box{a,b:type}(f: box_t(a -<cloref1> b), a: box_t(a)): box_t(b)
fn occur{a,b:type}(v: var_t(a), b: box_t(b)): bool
fn is_closed{a:type}(v: box_t(a)): bool
fn box_apply{a,b:type}(f: a -<cloref1> b, a: box_t(a)): box_t(b)
fn box_apply2{a,b,c:type}(f: (a, b) -<cloref1> c, a: box_t(a), b: box_t(b)): box_t(c)
fn box_apply3{a,b,c,d:type}(f: (a, b, c) -<cloref1> d, a: box_t(a), b: box_t(b), c: box_t(c)): box_t(d)
fn box_apply4{a,b,c,d,e:type}(f: (a, b, c, d) -<cloref1> e, a: box_t(a), b: box_t(b), c: box_t(c), d: box_t(d)): box_t(e)
fn box_pair{a,b:type}(x: box_t(a), y: box_t(b)): box_t('(a, b))
fn box_triple{a,b,c:type}(x: box_t(a), y: box_t(b), z: box_t(c)): box_t('(a, b, c))
fn box_opt0{a:type}(o: option0(box_t(a))): box_t(option0(a))
fn box_opt{a:type}(o: Option(box_t(a))): box_t(Option(a))
fn box_opt_vt{a:type}(o: Option_vt(box_t(a))): box_t(Option(a))
fn unbox{a:type}(b: box_t(a)): a


fun new_var{a:type}(mkfree: mkfree(a), name: string): var_t(a)