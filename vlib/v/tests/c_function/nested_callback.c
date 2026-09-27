typedef int (*v_nested_inner_cb)(int, int);

static int v_nested_call_outer(int (*outer)(int, v_nested_inner_cb, void*), v_nested_inner_cb inner) {
	return outer(3, inner, 0);
}
