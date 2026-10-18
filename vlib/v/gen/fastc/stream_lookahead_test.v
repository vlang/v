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
