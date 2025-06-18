// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.

module font

import os

// Variant enumerates the different variants a font can have.
pub enum Variant {
	normal = 0
	bold
	mono
	italic
}

@[if debug_font ?]
fn debug_font_println(s string) {
	println(s)
}

// default returns an absolute path the default system TTF font.
// If the env variable `VUI_FONT` is set this is used instead.
// NOTE that, in some cases, the function calls out to external OS programs
// so running this in a hot loop is not advised.
@[manualfree]
pub fn default() string {
	env_font := os.getenv('VUI_FONT')
	if env_font != '' && os.exists(env_font) {
		return env_font
	}
	unsafe { env_font.free() }
	$if windows {
		if os.exists('C:\\Windows\\Fonts\\segoeui.ttf') {
			debug_font_println('Using font "C:\\Windows\\Fonts\\segoeui.ttf"')
			return 'C:\\Windows\\Fonts\\segoeui.ttf'
		}
		debug_font_println('Using font "C:\\Windows\\Fonts\\arial.ttf"')
		return 'C:\\Windows\\Fonts\\arial.ttf'
	}
	$if macos {
		fonts := ['/System/Library/Fonts/SFNS.ttf', '/System/Library/Fonts/SFNSText.ttf',
			'/Library/Fonts/Arial.ttf']
		for font in fonts {
			if os.is_file(font) {
				debug_font_println('Using font "${font}"')
				return font
			}
		}
		unsafe { fonts.free() }
	}
	$if android {
		xml_files := ['/system/etc/system_fonts.xml', '/system/etc/fonts.xml',
			'/etc/system_fonts.xml', '/etc/fonts.xml', '/data/fonts/fonts.xml',
			'/etc/fallback_fonts.xml']
		font_locations := ['/system/fonts', '/data/fonts']
		for xml_file in xml_files {
			if os.is_file(xml_file) && os.is_readable(xml_file) {
				xml := os.read_file(xml_file) or { continue }
				lines := xml.split('\n')
				mut candidate_font := ''
				for line in lines {
					if line.contains('<font') {
						tmp1 := line.all_after('>')
						tmp2 := tmp1.all_before('<')
						tmp3 := tmp2.trim(' \n\t\r')
						mut_assign(candidate_font, tmp3)
						if candidate_font.contains('.ttf') {
							for location in font_locations {
								candidate_path := os.join_path_single(location, candidate_font)
								if os.is_file(candidate_path) && os.is_readable(candidate_path) {
									debug_font_println('Using font "${candidate_path}"')
									return candidate_path
								}
								unsafe { candidate_path.free() }
							}
						}
						unsafe { tmp3.free() }
						unsafe { tmp2.free() }
						unsafe { tmp1.free() }
					}
				}
				unsafe { candidate_font.free() }
				unsafe { lines.free() }
				unsafe { xml.free() }
			}
		}
		unsafe { font_locations.free() }
		unsafe { xml_files.free() }
	}
	mut fm := os.execute("fc-match --format='%{file}\n' -s")
	if fm.exit_code == 0 {
		lines := fm.output.split('\n')
		for l in lines {
			if !l.contains('.ttc') {
				debug_font_println('Using font "${l}"')
				return l
			}
		}
		unsafe { lines.free() }
	} else {
		panic('fc-match failed to fetch system font')
	}
	unsafe { fm.free() }
	panic('failed to init the font')
}

// get_path_variant returns the `font_path` file name replaced with the
// file name of the font's `variant` version if it exists.
@[manualfree]
pub fn get_path_variant(font_path string, variant Variant) string {
	// TODO: find some way to make this shorter and more eye-pleasant
	// NotoSans, LiberationSans, DejaVuSans, Arial and SFNS should work
	mut file := os.file_name(font_path)
	defer { unsafe { file.free() } }

	mut fpath := font_path.replace(file, '')
	defer { unsafe { fpath.free() } }

	mut_replace(file, '.ttf', '')

	flower := file.to_lower()
	defer { unsafe { flower.free() } }

	match variant {
		.normal {}
		.bold {
			if fpath.ends_with('-Regular') {
				mut_replace(file, '-Regular', '-Bold')
			} else if file.starts_with('DejaVuSans') {
				mut_plus(file, '-Bold')
			} else if flower.starts_with('arial') {
				mut_plus(file, 'bd')
			} else {
				mut_plus(file, '-bold')
			}
			$if macos {
				if os.exists('SFNS-bold') {
					mut_assign(file, 'SFNS-bold')
				}
			}
		}
		.italic {
			if file.ends_with('-Regular') {
				mut_replace(file, '-Regular', '-Italic')
			} else if file.starts_with('DejaVuSans') {
				mut_plus(file, '-Oblique')
			} else if flower.starts_with('arial') {
				mut_plus(file, 'i')
			} else {
				mut_plus(file, 'Italic')
			}
		}
		.mono {
			if !file.ends_with('Mono-Regular') && file.ends_with('-Regular') {
				mut_replace(file, '-Regular', 'Mono-Regular')
			} else if flower.starts_with('arial') {
				// Arial has no mono variant
			} else {
				mut_plus(file, 'Mono')
			}
		}
	}
	res := '${fpath}${file}.ttf'
	return res
}

@[manualfree]
fn mut_replace(s &string, find string, replacement string) {
	new := (*s).replace(find, replacement)
	unsafe { s.free() }
	unsafe {
		*s = new
	}
}

@[manualfree]
fn mut_plus(s &string, tail string) {
	new := (*s) + tail
	unsafe { s.free() }
	unsafe {
		*s = new
	}
}

@[manualfree]
fn mut_assign(s &string, value string) {
	unsafe { s.free() }
	unsafe {
		*s = value.clone()
	}
}
