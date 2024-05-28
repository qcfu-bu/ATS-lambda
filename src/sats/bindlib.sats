#include "share/atspre_staload_libats_ML.hats"

abstype box_boxed(a:type)
typedef box_t(a) = box_boxed(a)

abstype var_boxed(a:type)
typedef var_t(a) = var_boxed(a)
typedef mkfree(a) = var_t(a) -<cloref1> a

fun name_of{a:type}(x: var_t(a)): string
fun box_var{a:type}(x: var_t(a)): box_t(a)
fun compare_vars{a:type}(x: var_t(a), y: var_t(a)): int
fun eq_vars{a:type}(x: var_t(a), y: var_t(a)): bool
fun uid_of{a:type}(x: var_t(a)): int

typedef mvar_t(a:type) = array0(var_t(a))

fun names_of{a:type}(xs: mvar_t(a)): array0(string)
fun uids_of{a:type}(xs: mvar_t(a)): array0(int)


fun new_var{a:type}(mkfree: mkfree(a), name: string): var_t(a)