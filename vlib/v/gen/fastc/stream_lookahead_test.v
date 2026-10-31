module fastc

import v.pref
import v.scanner
import v.token

fn test_punctuation_byte_rejection_matches_scanner_lookahead() {
	prefs := pref.new_preferences()
	suffixes := [
		'',
		' ',
		'\t\r',
		'(',
		':',
		' : ',
		' (',
		',',
		')',
		']',
		'}',
		'+',
		'-',
		'*',
		'%',
		'&',
		'|',
		'^',
		'=',
		'!',
		'<',
		'>',
		'.',
		';',
		'[',
		'...',
		'.25',
		':=',
		'\n(',
		'\n:',
		'\r\n (',
		'/* c */ (',
		'/* outer /* nested */ c */ :',
		'// c\n(',
		'/*\n*/ :',
		'/',
		'/=',
		'#line 7 "x.v"\n(',
		"'('",
		"r'('",
		r'"${x}"',
		'value',
		'\v(',
		'\f:',
	]
	for suffix in suffixes {
		source := 'field' + suffix
		for line_directives in [false, true] {
			mut scan := scanner.new_scanner(prefs, .normal)
			scan.init(token.File.unindexed('lookahead.v', source.len), source)
			scan.line_directives = line_directives
			assert scan.scan() == .name
			g := Parser{ prefs: prefs, s: scan }
			for punctuation in [u8(`(`), `:`] {
				if !g.next_source_byte_may_be(punctuation) {
					mut probe := scan
					actual := probe.scan()
					expected := if punctuation == `(` {
						token.Token.lpar
					} else {
						token.Token.colon
					}
					assert actual != expected, '${suffix}: ${punctuation}: ${actual}'
				}
			}
			assert g.s.offset == scan.offset && g.s.pos == scan.pos
			assert g.s.diagnostics == scan.diagnostics
		}
	}
}

fn test_punctuation_byte_rejection_inside_interpolation() {
	prefs := pref.new_preferences()
	for source in [r"'${item.field}'", r"'${item.field:5}'", r"'${item.field(2)}'",
		r"'${item.field + 1}'", r"'${item.field /* c */ (2)}'", r"'${item.field.len}'"] {
		mut scan := scanner.new_scanner(prefs, .normal)
		scan.init(token.File.unindexed('interpolation.v', source.len), source)
		mut found := false
		for _ in 0 .. 20 {
			tok := scan.scan()
			if tok == .eof { break }
			if tok != .name || scan.lit != 'field' { continue }
			found = true
			g := Parser{ prefs: prefs, s: scan }
			for punctuation in [u8(`(`), `:`] {
				if !g.next_source_byte_may_be(punctuation) {
					mut probe := scan
					expected := if punctuation == `(` {
						token.Token.lpar
					} else {
						token.Token.colon
					}
					assert probe.scan() != expected, source
				}
			}
		}
		assert found, source
	}
}

fn statement_label44_compare(scan scanner.Scanner, current token.Token) {
	prefs := pref.new_preferences()
	mut original := scan
	expected := current == .name && original.scan() == .colon
	g := Parser{ prefs: prefs, s: scan, tok: current, lit: scan.lit, selfhost: true }
	assert g.statement_starts_label() == expected, scan.src
	assert g.s.offset == scan.offset && g.s.pos == scan.pos
	assert g.s.lit == scan.lit && g.s.diagnostics == scan.diagnostics
	assert g.s.in_str_incomplete == scan.in_str_incomplete
	assert g.s.in_str_inter == scan.in_str_inter
	assert g.s.in_str_inter_format == scan.in_str_inter_format
	assert g.s.str_inter_cbr_depth == scan.str_inter_cbr_depth
	assert g.s.str_quote == scan.str_quote
	assert g.s.str_parent_quotes == scan.str_parent_quotes
	assert g.s.str_parent_depths == scan.str_parent_depths
	assert g.s.line_directive_offsets == scan.line_directive_offsets
	// Advancing both original scanners also compares their private semicolon/dot state.
	mut actual_scan := g.s
	mut expected_scan := scan
	for _ in 0 .. 4 {
		actual := actual_scan.scan()
		assert actual == expected_scan.scan(), scan.src
		assert actual_scan.offset == expected_scan.offset
		assert actual_scan.lit == expected_scan.lit
		if actual == .eof { break }
	}
}

fn test_statement_label_punctuation_guard_preserves_original_lookahead() {
	prefs := pref.new_preferences()
	for source in ['label:', 'label : for {}', 'label\t\r: while true {}', 'label := 1', 'label = 1',
		'label.field()', 'label(1)', 'label[0]', 'label + 1', 'label != 1', 'label;', 'label',
		'label\n:', 'label\r\n:', 'label /* c */ :', 'label /* outer /* nested */ c */ :',
		'label // c\n:', 'label /*\n*/ :', 'label #line 7 "label.v"\n:', 'label / 2', 'label other',
		'if true', '123 + 1'] {
		for line_directives in [false, true] {
			mut scan := scanner.new_scanner(prefs, .normal)
			scan.init(token.File.unindexed('label.v', source.len), source)
			scan.line_directives = line_directives
			current := scan.scan()
			statement_label44_compare(scan, current)
		}
	}
	for source in [r"'${label}'", r"'${label:5}'", r"'${label + 1}'", r"'${label.field}'"] {
		mut scan := scanner.new_scanner(prefs, .normal)
		scan.init(token.File.unindexed('label_interpolation.v', source.len), source)
		mut found := false
		for _ in 0 .. 20 {
			current := scan.scan()
			if current == .eof { break }
			if current == .name && scan.lit == 'label' {
				statement_label44_compare(scan, current)
				found = true
			}
		}
		assert found, source
	}
}
