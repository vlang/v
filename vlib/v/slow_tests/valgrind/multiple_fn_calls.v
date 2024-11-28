import strconv

fn color_code_to_rgb(color string) []int {
	clr := color.replace('#', '')
	return [int(strconv.parse_int(clr[0..2], 16, 0) or { return [0, 0, 0] }),
		int(strconv.parse_int(clr[2..4], 16, 0) or { return [0, 0, 0] }),
		int(strconv.parse_int(clr[4..6],
			16, 0) or { return [0, 0, 0] })]
}

fn main() {
	dump(color_code_to_rgb('#abcdef'))
}
