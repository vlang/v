// Generate private JSON5 category tables from Unicode 15.0.0 UnicodeData.txt.
// Run from the V repository root with: v run vlib/json2/testdata/gen_json5_unicode.v

module main

import net.http
import os
import rand
import strconv
import strings

const unicode_data_url = 'https://www.unicode.org/Public/15.0.0/ucd/UnicodeData.txt'
const vexe = os.real_path(os.getenv_opt('VEXE') or { @VEXE })

struct CodeRange {
mut:
	start int
	end   int
}

fn main() {
	script_dir := os.dir(os.real_path(@FILE))
	json2_dir := os.dir(script_dir)
	reference_path := os.join_path(json2_dir, 'json5_unicode.v')
	temporary_dir := os.join_path(os.temp_dir(), 'json5_unicode_${rand.ulid()}')
	os.mkdir(temporary_dir) or { panic(err) }
	defer {
		os.rmdir_all(temporary_dir) or {}
	}
	unicode_data_path := os.join_path(temporary_dir, 'UnicodeData.txt')
	output_path := os.join_path(temporary_dir, 'json5_unicode.v')

	http.download_file(unicode_data_url, unicode_data_path) or { panic(err) }
	categories := read_categories(unicode_data_path) or { panic(err) }
	generated := generate(categories)
	os.write_file(output_path, generated) or { panic(err) }

	format_result := os.exec([vexe, 'fmt', '-w', output_path])
	if format_result.exit_code != 0 {
		panic('v fmt failed: ${format_result.output}')
	}

	if (os.read_file(output_path) or { panic(err) }) == (os.read_file(reference_path) or {
		panic(err)
	}) {
		println('Generated file matches ${reference_path}; no update needed.')
	} else {
		os.cp(output_path, reference_path, os.CopyParams{}) or { panic(err) }
		println('Generated file differed; updated ${reference_path}.')
	}
}

fn read_categories(path string) !map[int]string {
	mut categories := map[int]string{}
	mut first := -1
	data := os.read_file(path)!
	for line in data.split_into_lines() {
		fields := line.split(';')
		if fields.len < 3 {
			continue
		}
		parsed_code := strconv.parse_int(fields[0], 16, 32) or {
			return error('Invalid code point in UnicodeData.txt: ${fields[0]}')
		}
		code := int(parsed_code)
		name := fields[1]
		category := fields[2]
		if name.ends_with(', First>') {
			first = code
		} else if name.ends_with(', Last>') {
			if first < 0 {
				return error('UnicodeData.txt has a Last record without a First record')
			}
			for point in first .. code + 1 {
				categories[point] = category
			}
			first = -1
		} else {
			categories[code] = category
		}
	}
	return categories
}

fn generate(categories map[int]string) string {
	mut output := strings.new_builder(4096)
	output.write_string('// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.\n')
	output.write_string('// Use of this source code is governed by an MIT license found in the LICENSE file.\n')
	output.write_string("// Generated from Unicode 15.0.0, matching encoding.utf8's letter tables.\n")
	output.write_string('// Source: ${unicode_data_url}\n')
	output.write_string('// Regenerate with testdata/gen_json5_unicode.v.\n')
	output.write_string('module json2\n\n')
	output.write_string("// Nl completes ECMAScript UnicodeLetter alongside utf8.is_letter's L categories.\n")
	emit_ranges(mut output, 'json5_letter_number_ranges', categories, ['Nl'])
	output.write_string('// IdentifierPart adds Mn, Mc, Nd and Pc; No and Me are excluded.\n')
	emit_ranges(mut output, 'json5_identifier_part_ranges', categories, ['Mn', 'Mc', 'Nd', 'Pc'])
	print_oracle('IdentifierStart', categories, ['Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Nl'], [
		0x24,
		0x5f,
	])
	print_oracle('IdentifierPart', categories, ['Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Nl', 'Mn', 'Mc',
		'Nd', 'Pc'],
		[0x24, 0x5f, 0x200c, 0x200d])
	return output.str()
}

fn emit_ranges(mut output strings.Builder, name string, categories map[int]string, allowed []string) {
	mut codes := categories.keys()
	codes.sort()
	output.write_string('const ${name} = [\n')
	mut range_start := -1
	mut range_end := -1
	for code in codes {
		if categories[code] !in allowed {
			continue
		}
		if range_start >= 0 && range_end + 1 == code {
			range_end = code
		} else {
			if range_start >= 0 {
				write_range(mut output, range_start, range_end)
			}
			range_start = code
			range_end = code
		}
	}
	if range_start >= 0 {
		write_range(mut output, range_start, range_end)
	}
	output.write_string(']\n\n')
}

fn write_range(mut output strings.Builder, start int, end int) {
	output.write_string('\t[0x${start:04x}, 0x${end:04x}]!,\n')
}

fn print_oracle(name string, categories map[int]string, allowed []string, extras []int) {
	mut count := 0
	mut checksum := u64(0xcbf29ce484222325)
	for code in 0 .. 0x110000 {
		bit := if categories[code] in allowed || code in extras { u64(1) } else { u64(0) }
		count += int(bit)
		checksum = (checksum ^ bit) * u64(0x100000001b3)
	}
	eprintln('${name}: count=${count}, FNV-1a=${checksum}')
}
