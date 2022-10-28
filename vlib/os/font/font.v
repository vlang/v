// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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

[if debug_font ?]
fn debug_font_println(s string) {
	println(s)
}

// default returns an absolute path the default system TTF font.
// If the env variable `VUI_FONT` is set this is used instead.
// NOTE that, in some cases, the function calls out to external OS programs
// so running this in a hot loop is not advised.
pub fn default() string {
	env_font := os.getenv('VUI_FONT')
	if env_font != '' && os.exists(env_font) {
		return env_font
	}
	$if windows {
		debug_font_println('Using font "C:\\Windows\\Fonts\\arial.ttf"')
		return 'C:\\Windows\\Fonts\\arial.ttf'
	}
	$if macos {
		fonts := ['/System/Library/Fonts/SFNS.ttf', '/System/Library/Fonts/SFNSText.ttf',
			'/Library/Fonts/Arial.ttf']
		for font in fonts {
			if os.is_file(font) {
				debug_font_println('Using font "$font"')
				return font
			}
		}
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
						candidate_font = line.all_after('>').all_before('<').trim(' \n\t\r')
						if candidate_font.contains('.ttf') {
							for location in font_locations {
								candidate_path := os.join_path(location, candidate_font)
								if os.is_file(candidate_path) && os.is_readable(candidate_path) {
									debug_font_println('Using font "$candidate_path"')
									return candidate_path
								}
							}
						}
					}
				}
			}
		}
	}
	mut fm := os.execute("fc-match --format='%\{file}\n' -s")
	if fm.exit_code == 0 {
		lines := fm.output.split('\n')
		for l in lines {
			if !l.contains('.ttc') {
				debug_font_println('Using font "$l"')
				return l
			}
		}
	} else {
		panic('fc-match failed to fetch system font')
	}
	panic('failed to init the font')
}

// get_path_variant returns the `font_path` file name replaced with the
// file name of the font's `variant` version if it exists.
pub fn get_path_variant(font_path string, variant Variant) string {
	// TODO: find some way to make this shorter and more eye-pleasant
	// NotoSans, LiberationSans, DejaVuSans, Arial and SFNS should work
	mut file := os.file_name(font_path)
	mut fpath := font_path.replace(file, '')
	file = file.replace('.ttf', '')

	match variant {
		.normal {}
		.bold {
			if fpath.ends_with('-Regular') {
				file = file.replace('-Regular', '-Bold')
			} else if file.starts_with('DejaVuSans') {
				file += '-Bold'
			} else if file.to_lower().starts_with('arial') {
				file += 'bd'
			} else {
				file += '-bold'
			}
			$if macos {
				if os.exists('SFNS-bold') {
					file = 'SFNS-bold'
				}
			}
		}
		.italic {
			if file.ends_with('-Regular') {
				file = file.replace('-Regular', '-Italic')
			} else if file.starts_with('DejaVuSans') {
				file += '-Oblique'
			} else if file.to_lower().starts_with('arial') {
				file += 'i'
			} else {
				file += 'Italic'
			}
		}
		.mono {
			if !file.ends_with('Mono-Regular') && file.ends_with('-Regular') {
				file = file.replace('-Regular', 'Mono-Regular')
			} else if file.to_lower().starts_with('arial') {
				// Arial has no mono variant
			} else {
				file += 'Mono'
			}
		}
	}
	return fpath + file + '.ttf'
}
