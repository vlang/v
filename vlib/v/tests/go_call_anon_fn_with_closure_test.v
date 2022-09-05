fn test_go_call_anon_fn_with_closure1() {
	a := 1

	b := fn [a] () int {
		println(a)
		return a
	}

	g := go b()
	ret := g.wait()
	assert ret == 1
}

fn test_go_call_anon_fn_with_closure2() {
	m := {
		'key1': 1
		'key2': 2
	}

	h := go fn [m] () int {
		println(m['key2'])
		return m['key2']
	}()

	ret := h.wait()
	assert ret == 2
}
