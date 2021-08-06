fn test_anon_functions_can_have_loops_with_breaks() {
	mut res := 123
	for true {
		x := fn () int {
			for x in 0 .. 10 {
				println(x)
				break
			}
			return 3
		}()
		println('${x}')
		res = x
		break
	}
	assert res == 3
}
