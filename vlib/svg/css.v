module svg

import strings

// CssRule is a simple CSS rule: a selector and its declarations.
pub struct CssRule {
pub:
	selector string
	style    Style
}

// parse_css parses a small CSS subset: type, .class, #id and descendant
// combinators. At-rules are preserved as raw rules with their selector text.
pub fn parse_css(input string) []CssRule {
	mut rules := []CssRule{}
	// Strip comments first.
	mut clean := strings.new_builder(input.len)
	mut i := 0
	for i < input.len {
		if i + 3 < input.len && input[i..i + 2] == '/*' {
			end := input.index_after('*/', i + 2) or { break }
			i = end + 2
			continue
		}
		clean.write_string(input[i..i + 1])
		i++
	}
	text := clean.str()
	for block in split_blocks(text) {
		open := block.index('{') or { continue }
		close := block.index_after('}', open) or { continue }
		selector := block[0..open].trim_space()
		decls := block[open + 1..close]
		if selector == '' {
			continue
		}
		style := parse_style(decls) or { Style{} }
		for sel in selector.split(',') {
			s := sel.trim_space()
			if s != '' {
				rules << CssRule{ selector: s, style: style }
			}
		}
	}
	return rules
}

fn split_blocks(s string) []string {
	mut out := []string{}
	mut depth := 0
	mut start := 0
	for i, c in s {
		if c == `{` {
			depth++
		} else if c == `}` {
			if depth > 0 {
				depth--
			}
			if depth == 0 {
				out << s[start..i + 1]
				start = i + 1
			}
		}
	}
	return out
}

// selector_matches reports whether an element matches a simple selector.
pub fn selector_matches(e &Element, selector string) bool {
	parts := selector.split(' ')
	for part in parts {
		p := part.trim_space()
		if p == '' {
			continue
		}
		if !single_selector_matches(e, p) {
			return false
		}
	}
	return true
}

fn single_selector_matches(e &Element, s string) bool {
	if s == '*' {
		return true
	}
	if s.starts_with('#') {
		return e.id() == s[1..]
	}
	if s.starts_with('.') {
		needle := s[1..]
		for c in e.classes() {
			if c == needle {
				return true
			}
		}
		return false
	}
	return e.tag == s
}

// specificity returns the (id, class, type) specificity of a selector.
pub fn specificity(selector string) (int, int, int) {
	mut ids := 0
	mut classes := 0
	mut types := 0
	for token in selector.split(' ') {
		t := token.trim_space()
		if t.starts_with('#') {
			ids++
		} else if t.starts_with('.') {
			classes++
		} else if t != '' && t != '*' {
			types++
		}
	}
	return ids, classes, types
}

// cascade_style applies matching CSS rules to an element's inline style.
// More specific rules win; later rules win ties.
pub fn cascade_style(e &Element, rules []CssRule) Style {
	mut result := Style{}
	cascade_style_into(e, rules, mut result)
	return result
}

// cascade_style_into merges matching rules into an existing style, avoiding
// an extra allocation when the caller already has a style buffer.
pub fn cascade_style_into(e &Element, rules []CssRule, mut out Style) {
	mut best_ids := -1
	mut best_classes := -1
	mut best_types := -1
	for r in rules {
		if !selector_matches(e, r.selector) {
			continue
		}
		ids, classes, types := specificity(r.selector)
		better := ids > best_ids || (ids == best_ids && (classes > best_classes || (classes == best_classes && types >= best_types)))
		if better {
			best_ids = ids
			best_classes = classes
			best_types = types
			for p in r.style.props {
				out.set(p.name, p.value)
			}
		}
	}
	// Inline style wins over stylesheet rules.
	for p in e.style.props {
		out.set(p.name, p.value)
	}
}
