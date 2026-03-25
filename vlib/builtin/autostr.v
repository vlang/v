module builtin

const autostr_type_stack_max_depth = 64

__global g_autostr_type_stack = [autostr_type_stack_max_depth]int{}
__global g_autostr_type_stack_len = 0

@[markused]
fn autostr_type_in_stack(typ int) bool {
	for i := 0; i < g_autostr_type_stack_len; i++ {
		if g_autostr_type_stack[i] == typ {
			return true
		}
	}
	return false
}

@[markused]
fn autostr_type_push(typ int) {
	if g_autostr_type_stack_len >= autostr_type_stack_max_depth {
		return
	}
	g_autostr_type_stack[g_autostr_type_stack_len] = typ
	g_autostr_type_stack_len++
}

@[markused]
fn autostr_type_pop() {
	if g_autostr_type_stack_len > 0 {
		g_autostr_type_stack_len--
	}
}
