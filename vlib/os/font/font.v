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

fn is_readable_font_file(font_path string) bool {
	if font_path == '' {
		return false
	}
	return !font_path.to_lower().ends_with('.ttc') && os.is_file(font_path)
		&& os.is_readable(font_path)
}

fn use_font_if_readable(font_path string) string {
	if is_readable_font_file(font_path) {
		debug_font_println('Using font "${font_path}"')
		return font_path
	}
	return ''
}

fn find_first_font_in_paths(font_paths []string) string {
	for font_path in font_paths {
		if is_readable_font_file(font_path) {
			return font_path.clone()
		}
	}
	return ''
}

fn find_first_font_in_dirs(font_dirs []string) string {
	for font_dir in font_dirs {
		if !os.is_dir(font_dir) {
			continue
		}
		mut font_paths := os.walk_ext(font_dir, '.ttf')
		font_paths.sort()
		font_path := find_first_font_in_paths(font_paths)
		if font_path != '' {
			return font_path
		}
	}
	return ''
}

fn default_user_font_dirs() []string {
	mut font_dirs := []string{}
	if config_dir := os.config_dir() {
		font_dirs << os.join_path(config_dir, 'v', 'fonts')
	}
	return font_dirs
}

fn bundled_font_dirs() []string {
	return [os.join_path(@VEXEROOT, 'examples', 'assets', 'fonts')]
}

fn find_fc_match_font(output string) string {
	return find_first_font_in_paths(output.split('\n'))
}

// default returns an absolute path to a readable default font.
// Search order:
// 1. The env variable `VUI_FONT`.
// 2. A user-provided fallback font under `${os.config_dir()}/v/fonts`.
// 3. Platform defaults and `fc-match`.
// 4. Bundled fonts under `@VEXEROOT/examples/assets/fonts`.
// NOTE that, in some cases, the function calls out to external OS programs
// so running this in a hot loop is not advised.
@[manualfree]
pub fn default() string {
	env_font := os.getenv('VUI_FONT')
	if env_font != '' && os.is_file(env_font) && os.is_readable(env_font) {
		debug_font_println('Using font "${env_font}"')
		return env_font
	}
	unsafe { env_font.free() }
	user_font_path := find_first_font_in_dirs(default_user_font_dirs())
	if user_font_path != '' {
		debug_font_println('Using font "${user_font_path}"')
		return user_font_path
	}
	$if windows {
		fonts := ['C:\\Windows\\Fonts\\segoeui.ttf', 'C:\\Windows\\Fonts\\arial.ttf']
		for system_font in fonts {
			existing_font := use_font_if_readable(system_font)
			if existing_font != '' {
				return existing_font
			}
		}
	}
	$if macos {
		fonts := ['/System/Library/Fonts/SFNS.ttf', '/System/Library/Fonts/SFNSText.ttf',
			'/Library/Fonts/Arial.ttf']
		for system_font in fonts {
			existing_font := use_font_if_readable(system_font)
			if existing_font != '' {
				return existing_font
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
								existing_font := use_font_if_readable(candidate_path)
								if existing_font != '' {
									return existing_font
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
		fc_match_font_path := find_fc_match_font(fm.output)
		if fc_match_font_path != '' {
			unsafe { fm.free() }
			debug_font_println('Using font "${fc_match_font_path}"')
			return fc_match_font_path
		}
	} else {
		debug_font_println('fc-match failed to fetch system font')
	}
	unsafe { fm.free() }
	bundled_font_path := find_first_font_in_dirs(bundled_font_dirs())
	if bundled_font_path != '' {
		debug_font_println('Using font "${bundled_font_path}"')
		return bundled_font_path
	}
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
