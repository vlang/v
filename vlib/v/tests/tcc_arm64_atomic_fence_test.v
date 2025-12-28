// vtest build: tinyc && arm64

module main

fn test_tcc_arm64_atomic_fence() {
	a := 1

	b := fn [a] () int {
		println(a)
		return a
	}

	g := spawn b()
	ret := g.wait()
	println(ret) // 1
}
