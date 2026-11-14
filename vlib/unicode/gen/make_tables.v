module main

import os
import strings

// make_tables regenerates vlib/unicode/tables.v from the Unicode
// Character Database. Usage:
//   v run vlib/unicode/gen/make_tables.v -- <ucd-dir> <version>
// <ucd-dir> must contain UnicodeData.txt, PropList.txt and
// CaseFolding.txt from the same UCD release; <version> stamps the
// output (e.g. 16.0.0). Network access is not required and no files
// are downloaded: fetch the UCD release yourself, e.g.
//   curl -O https://www.unicode.org/Public/16.0.0/ucd/UnicodeData.txt

struct CatRange {
mut:
	lo  u32
	hi  u32
	cat string
}

const cats = ['Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Mn', 'Mc', 'Me', 'Nd', 'Nl', 'No', 'Pc', 'Pd', 'Ps',
	'Pe', 'Pi', 'Pf', 'Po', 'Sm', 'Sc', 'Sk', 'So', 'Zs', 'Zl', 'Zp', 'Cc', 'Cf', 'Cs', 'Co', 'Cn']

fn strip_comment(s string) string {
	return s.split('#')[0].trim_space()
}

fn split_fields(s string) []string {
	return s.split(';').map(fn (p string) string { return p.trim_space() })
}

fn parse_hex_range(s string) !(u32, u32) {
	t := s.trim_space()
	if t.contains('..') {
		parts := t.split('..')
		return u32(parts[0].parse_uint(16, 32)!), u32(parts[1].parse_uint(16, 32)!)
	}
	v := u32(t.parse_uint(16, 32)!)
	return v, v
}

fn main() {
	if os.args.len < 3 {
		eprintln('usage: make_tables <ucd-dir> <version>')
		exit(1)
	}
	ucd := os.args[os.args.len - 2]
	version := os.args[os.args.len - 1]
	// 1. General categories from UnicodeData.txt.
	mut ranges := []CatRange{}
	mut pending_first := map[string]u32{}
	lines := os.read_lines(os.join_path(ucd, 'UnicodeData.txt'))!
	for line in lines {
		if line == '' || line.starts_with('#') {
			continue
		}
		f := line.split(';')
		if f.len < 14 {
			continue
		}
		cp := u32(f[0].parse_uint(16, 32)!)
		cat := f[2]
		name := f[1]
		if name.ends_with(', First>') {
			pending_first[cat] = cp
			continue
		}
		if name.ends_with(', Last>') {
			start := pending_first[cat] or { panic('unmatched Last range for ${cat}') }
			ranges << CatRange{ lo: start, hi: cp, cat: cat }
			pending_first.delete(cat)
			continue
		}
		ranges << CatRange{ lo: cp, hi: cp, cat: cat }
	}
	if pending_first.len != 0 {
		panic('dangling First ranges')
	}
	// Merge adjacent ranges with equal categories.
	ranges.sort(a.lo < b.lo)
	mut merged := []CatRange{}
	for r in ranges {
		if merged.len > 0 && merged[merged.len - 1].cat == r.cat
			&& r.lo <= merged[merged.len - 1].hi + 1 {
			merged[merged.len - 1].hi = r.hi
		} else {
			merged << r
		}
	}
	// 2. White_Space from PropList.txt, Alphabetic/Uppercase/Lowercase
	// from DerivedCoreProperties.txt.
	mut spaces := [][]u32{}
	mut alphabetic := [][]u32{}
	mut cased_upper := [][]u32{}
	mut cased_lower := [][]u32{}
	plines := os.read_lines(os.join_path(ucd, 'PropList.txt'))!
	for line in plines {
		t := strip_comment(line)
		if t == '' {
			continue
		}
		f := split_fields(t)
		if f.len < 2 || f[1] != 'White_Space' {
			continue
		}
		lo, hi := parse_hex_range(f[0])!
		spaces << [lo, hi]
	}
	dlines := os.read_lines(os.join_path(ucd, 'DerivedCoreProperties.txt'))!
	for line in dlines {
		t := strip_comment(line)
		if t == '' {
			continue
		}
		f := split_fields(t)
		if f.len < 2 {
			continue
		}
		lo, hi := parse_hex_range(f[0])!
		if f[1] == 'Alphabetic' {
			alphabetic << [lo, hi]
		} else if f[1] == 'Uppercase' {
			cased_upper << [lo, hi]
		} else if f[1] == 'Lowercase' {
			cased_lower << [lo, hi]
		}
	}
	// 3. Case mappings from UnicodeData.txt fields 12-14.
	mut lowers := map[u32]u32{}
	mut uppers := map[u32]u32{}
	mut titles := map[u32]u32{}
	for line in lines {
		if line == '' || line.starts_with('#') {
			continue
		}
		f := line.split(';')
		if f.len < 14 {
			continue
		}
		cp := u32(f[0].parse_uint(16, 32)!)
		if f[12] != '' {
			uppers[cp] = u32(f[12].parse_uint(16, 32)!)
		}
		if f[13] != '' {
			lowers[cp] = u32(f[13].parse_uint(16, 32)!)
		}
		if f[14] != '' {
			titles[cp] = u32(f[14].parse_uint(16, 32)!)
		}
	}
	// 4. Case folding: simple (C/S single target) and full (F).
	mut fold_simple := map[u32]u32{}
	mut fold_full := map[u32]string{}
	clines := os.read_lines(os.join_path(ucd, 'CaseFolding.txt'))!
	for line in clines {
		t := strip_comment(line)
		if t == '' {
			continue
		}
		f := split_fields(t)
		if f.len < 3 {
			continue
		}
		cp := u32(f[0].parse_uint(16, 32)!)
		kind := f[1]
		mut targets := []u32{}
		for hx in f[2].split(' ') {
			targets << u32(hx.parse_uint(16, 32)!)
		}
		if targets.len == 1 && (kind == 'C' || kind == 'S' || kind == 'F') {
			fold_simple[cp] = targets[0]
		} else if kind == 'F' {
			mut s := ''
			for t2 in targets {
				s += rune(t2).str()
			}
			fold_full[cp] = s
		}
	}
	write_tables(version, merged, spaces, alphabetic, cased_upper, cased_lower, lowers,
		uppers, titles, fold_simple, fold_full)!
}

fn w_pair(mut sb strings.Builder, from u32, to u32) {
	sb.write_string('\n\tCasePair{from: 0x${from:06x}, to: 0x${to:06x}},')
}

fn write_span(mut sb strings.Builder, name string, spans [][]u32) {
	sb.write_string('const ${name} = [')
	for s in spans {
		sb.write_string('\n\tSpanRange{lo: 0x${s[0]:06x}, hi: 0x${s[1]:06x}},')
	}
	sb.write_string('\n]\n\n')
}

fn write_tables(version string, ranges []CatRange, spaces [][]u32, alphabetic [][]u32, cased_upper [][]u32, cased_lower [][]u32, lowers map[u32]u32, uppers map[u32]u32, titles map[u32]u32, folds map[u32]u32, fold_full map[u32]string) ! {
	mut sb := strings.new_builder(1 << 20)
	sb.write_string('// Code generated by vlib/unicode/gen/make_tables.v. DO NOT EDIT.\n')
	sb.write_string('// Source: Unicode Character Database, version ${version}.\n')
	sb.write_string('// Regenerate: v run vlib/unicode/gen/make_tables.v -- <ucd-dir> ${version}\n')
	sb.write_string('module unicode\n\n')
	sb.write_string("pub const unicode_data_version = '${version}'\n\n")
	sb.write_string('// Category codes, indexes into category_names.\n')
	for i, c in cats {
		sb.write_string('pub const cat_${c.to_lower()} = ${i}\n')
	}
	sb.write_string('\npub const category_names = [')
	for c in cats {
		sb.write_string("'${c}', ")
	}
	sb.write_string(']\n\n')
	sb.write_string('struct CatRange {\n\tlo  u32\n\thi  u32\n\tcat u8\n}\n\n')
	mut cat_idx := map[string]int{}
	for i, c in cats {
		cat_idx[c] = i
	}
	sb.write_string('const cat_ranges = [')
	for r in ranges {
		idx := cat_idx[r.cat] or { 29 }
		sb.write_string('\n\tCatRange{lo: 0x${r.lo:06x}, hi: 0x${r.hi:06x}, cat: ${idx}},')
	}
	sb.write_string('\n]\n\n')
	sb.write_string('struct SpanRange {\n\tlo u32\n\thi u32\n}\n\n')
	write_span(mut sb, 'white_space_ranges', spaces)
	write_span(mut sb, 'alphabetic_ranges', alphabetic)
	write_span(mut sb, 'uppercase_ranges', cased_upper)
	write_span(mut sb, 'lowercase_ranges', cased_lower)
	sb.write_string('struct CasePair {\n\tfrom u32\n\tto   u32\n}\n\n')
	mut keys := lowers.keys()
	keys.sort()
	sb.write_string('const lower_map = [')
	for k in keys {
		w_pair(mut sb, k, lowers[k])
	}
	sb.write_string(']\n\n')
	keys = uppers.keys()
	keys.sort()
	sb.write_string('const upper_map = [')
	for k in keys {
		w_pair(mut sb, k, uppers[k])
	}
	sb.write_string(']\n\n')
	keys = titles.keys()
	keys.sort()
	sb.write_string('const title_map = [')
	for k in keys {
		w_pair(mut sb, k, titles[k])
	}
	sb.write_string(']\n\n')
	keys = folds.keys()
	keys.sort()
	sb.write_string('const fold_map = [')
	for k in keys {
		w_pair(mut sb, k, folds[k])
	}
	sb.write_string(']\n\n')
	mut fkeys := fold_full.keys()
	fkeys.sort()
	sb.write_string('struct FoldFull {\n\tfrom u32\n\tto   string\n}\n\n')
	sb.write_string('const fold_full_map = [')
	for k in fkeys {
		sb.write_string('\n\tFoldFull{from: 0x${k:06x}, to: "${fold_full[k]}"},')
	}
	sb.write_string('\n]\n')
	out := sb.str()
	os.write_file('vlib/unicode/tables.v', out)!
	println('wrote vlib/unicode/tables.v (${out.len} bytes)')
}
