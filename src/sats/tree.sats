#staload "./types.sats"

exception eval_error of ()

fun run_tree(term): value
