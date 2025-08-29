fn test_main() {
	mut m := map[int]?u32{}
	if c := m[0] {
		println('c:  ${c} not none!')
	}
}
