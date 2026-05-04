module builder

import os
import v.pref

fn test_parse_existing_ico_file() {
	icon_path := os.join_path(@VEXEROOT, 'cmd', 'tools', 'vdoc', 'theme', 'favicons', 'favicon.ico')
	images := parse_ico_file(icon_path)!
	assert images.len > 0
	for image in images {
		assert image.image_data.len > 0
	}
}

fn test_png_to_ico_bytes_roundtrip() {
	png_path := os.join_path(@VEXEROOT, 'examples', 'assets', 'logo.png')
	png_bytes := os.read_bytes(png_path)!
	ico_bytes := png_to_ico_bytes(png_bytes)!
	images := parse_ico_bytes(ico_bytes)!
	assert images.len == 1
	assert images[0].bytes_in_res == png_bytes.len
	assert images[0].image_data == png_bytes
}

fn test_png_dimensions_reads_valid_png() {
	png_path := os.join_path(@VEXEROOT, 'examples', 'assets', 'logo.png')
	size := png_dimensions(os.read_bytes(png_path)!)!
	assert size.width > 0
	assert size.height > 0
	assert size.width <= max_windows_icon_dimension
	assert size.height <= max_windows_icon_dimension
}

fn test_parse_ico_bytes_rejects_invalid_data() {
	if _ := parse_ico_bytes([]u8{}) {
		assert false
	} else {
		assert err.msg().contains('invalid icon file')
	}
}

fn test_windows_icon_flag_parsing() {
	target := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	icon := os.join_path(@VEXEROOT, 'cmd', 'tools', 'vdoc', 'theme', 'favicons', 'favicon.ico')
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-os', 'windows', '-icon', icon, target],
		false)
	assert prefs.icon_path == os.real_path(icon)
	assert prefs.build_options.contains('-icon "${os.real_path(icon)}"')
}

fn test_windows_icon_flag_parsing_with_inline_aliases() {
	target := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	icon := os.join_path(@VEXEROOT, 'examples', 'assets', 'logo.png')
	for arg in ['-icon=${icon}', '--icon=${icon}', '-seticon=${icon}', '--seticon=${icon}'] {
		prefs, _ := pref.parse_args_and_show_errors([], ['', '-os', 'windows', arg, target], false)
		assert prefs.icon_path == os.real_path(icon)
		assert prefs.build_options.contains('-icon "${os.real_path(icon)}"')
	}
}
