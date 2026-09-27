module ansi

// color applies the V1 diagnostic palette, respecting set_colors_enabled.
pub fn color(kind string, message string) string {
	if kind.contains('error') {
		return red(message)
	}
	if kind.contains('notice') {
		return yellow(message)
	}
	if kind.contains('details') {
		return format(message, '94', '39')
	}
	return format(message, '35', '39')
}
