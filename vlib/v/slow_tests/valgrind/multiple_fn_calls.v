fn copy_string(s string) !string {
	return s
}

// get_hex_rgb_colors returns hex codes of RGB colors
// with redundant cloning of strings to check autofree is working.
fn get_hex_rgb_colors(color string) []string {
	colors := color.replace('#', '')

	return [copy_string(colors[0..2]) or { return ['', '', ''] },
		copy_string(colors[2..4]) or { return ['', '', ''] },
		copy_string(colors[4..6]) or { return ['', '', ''] }]
}

fn main() {
	dump(get_hex_rgb_colors('#abcdef'))
}
