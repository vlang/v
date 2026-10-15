module fastc

import os
import strconv
import strings
import v.pref

// fastc_target_project_headers indexes real project headers once, including
// nested include directories supplied by the project's external C build.
fn fastc_target_project_headers(sources []FastcSourceFile, prefs &pref.Preferences) []string {
	mut roots := map[string]bool{}
	if prefs.module_resolution_root != '' {
		roots[fastc_vmod_root_for_file(os.join_path(prefs.module_resolution_root, 'main.v'))] = true
	}
	for source in sources {
		roots[fastc_vmod_root_for_file(os.real_path(source.path))] = true
	}
	mut headers := map[string]bool{}
	for root, _ in roots {
		for child in ['c', 'include'] {
			dir := os.join_path(root, child)
			if !os.is_dir(dir) { continue }
			for path in os.walk_ext(dir, '.h') {
				headers[os.real_path(path)] = true
			}
		}
	}
	mut paths := headers.keys()
	paths.sort()
	return paths
}

struct FastcHeaderBranch {
mut:
	parent_visible bool
	visible        bool
	taken          bool
	unknown        bool
}

// fastc_target_visible_header follows genuine includes and object macros.
// Unresolved conditions contribute no declarations, so they cannot suppress
// a native prototype that the actual C build still needs.
fn fastc_target_visible_header(path string, prefs &pref.Preferences, project_headers []string, mut macros map[string]string, mut visited map[string]bool, depth int) string {
	if depth > 64 || path in visited { return '' }
	visited[path] = true
	source := os.read_file(path) or { return '' }
	clean := fastc_target_header_without_comments(source).replace('\\\n', '')
	mut out := strings.new_builder(clean.len)
	mut branches := []FastcHeaderBranch{}
	for line in clean.split_into_lines() {
		trimmed := line.trim_space()
		visible := branches.len == 0 || branches.last().visible
		if !trimmed.starts_with('#') {
			if visible { out.writeln(line) }
			continue
		}
		directive := trimmed[1..].trim_space()
		words := directive.fields()
		if words.len == 0 { continue }
		kind := words[0]
		argument := directive[kind.len..].trim_space()
		if kind in ['if', 'ifdef', 'ifndef'] {
			mut state := FastcHeaderBranch{ parent_visible: visible }
			if visible {
				if kind == 'ifdef' || kind == 'ifndef' {
					defined := argument in macros
					state.visible = if kind == 'ifdef' { defined } else { !defined }
				} else if condition := fastc_target_header_condition(argument, macros, 0) {
					state.visible = condition != 0
				} else {
					state.unknown = true
				}
			}
			state.taken = state.visible
			branches << state
			continue
		}
		if kind == 'endif' {
			if branches.len > 0 { branches.delete_last() }
			continue
		}
		if kind == 'else' || kind == 'elif' {
			if branches.len == 0 { continue }
			mut state := branches.last()
			state.visible = false
			if state.parent_visible && !state.taken && !state.unknown {
				if kind == 'else' {
					state.visible = true
				} else if condition := fastc_target_header_condition(argument, macros, 0) {
					state.visible = condition != 0
				} else {
					state.unknown = true
				}
			}
			state.taken = state.taken || state.visible
			branches[branches.len - 1] = state
			continue
		}
		if !visible { continue }
		if kind == 'define' && argument != '' {
			mut end := 0
			for end < argument.len && (argument[end].is_alnum() || argument[end] == `_`) { end++ }
			if end > 0 && (end == argument.len || argument[end] != `(`) {
				macros[argument[..end]] = argument[end..].trim_space()
			}
		} else if kind == 'undef' {
			macros.delete(argument)
		} else if kind == 'include' {
			name := macros[argument] or { argument }
			included := fastc_target_include_path('include ${name}', path, prefs, project_headers)
			if included != '' {
				out.writeln(fastc_target_visible_header(included, prefs, project_headers, mut macros, mut visited, depth + 1))
			}
		}
	}
	return out.str()
}

fn fastc_target_header_without_comments(source string) string {
	mut out := strings.new_builder(source.len)
	mut i := 0
	mut quoted := u8(0)
	for i < source.len {
		ch := source[i]
		if quoted != 0 {
			out.write_u8(ch)
			if ch == `\\` && i + 1 < source.len {
				i++
				out.write_u8(source[i])
			} else if ch == quoted {
				quoted = 0
			}
		} else if ch == `"` || ch == `'` {
			quoted = ch
			out.write_u8(ch)
		} else if ch == `/` && i + 1 < source.len && source[i + 1] == `*` {
			i += 2
			out.write_u8(` `)
			for i < source.len && !(source[i] == `*` && i + 1 < source.len && source[i + 1] == `/`) {
				if source[i] == `\n` { out.write_u8(`\n`) }
				i++
			}
			i++
		} else if ch == `/` && i + 1 < source.len && source[i + 1] == `/` {
			for i < source.len && source[i] != `\n` { i++ }
			if i < source.len { out.write_u8(`\n`) }
		} else {
			out.write_u8(ch)
		}
		i++
	}
	return out.str()
}

struct FastcHeaderCondition {
	tokens []string
	macros map[string]string
	depth  int
mut:
	index int
}

fn fastc_target_header_condition(expression string, macros map[string]string, depth int) ?i64 {
	if depth > 32 { return none }
	mut tokens := []string{}
	mut i := 0
	for i < expression.len {
		if expression[i].is_space() {
			i++
			continue
		}
		start := i
		if expression[i].is_alnum() || expression[i] == `_` {
			i++
			for i < expression.len && (expression[i].is_alnum() || expression[i] == `_`) { i++ }
		} else {
			i++
			if i < expression.len && expression[start..i + 1] in ['&&', '||', '==', '!=', '<=',
				'>=', '<<', '>>'] {
				i++
			}
		}
		tokens << expression[start..i]
	}
	mut parser := FastcHeaderCondition{ tokens: tokens, macros: macros, depth: depth }
	value := parser.expression(1)?
	if parser.index != tokens.len { return none }
	return value
}

fn (mut p FastcHeaderCondition) primary() ?i64 {
	if p.index >= p.tokens.len { return none }
	tok := p.tokens[p.index]
	p.index++
	if tok == '(' {
		value := p.expression(1)?
		if p.index >= p.tokens.len || p.tokens[p.index] != ')' { return none }
		p.index++
		return value
	}
	if tok in ['!', '~', '+', '-'] {
		value := p.primary()?
		return match tok {
			'!' { i64(value == 0) }
			'~' { ~value }
			'-' {
				if value == i64(-9223372036854775807) - 1 { return none }
				-value
			}
			else { value }
		}
	}
	if tok == 'defined' {
		parenthesized := p.index < p.tokens.len && p.tokens[p.index] == '('
		if parenthesized { p.index++ }
		if p.index >= p.tokens.len { return none }
		name := p.tokens[p.index]
		p.index++
		if parenthesized {
			if p.index >= p.tokens.len || p.tokens[p.index] != ')' { return none }
			p.index++
		}
		return i64(name in p.macros)
	}
	if tok.len > 0 && tok[0].is_digit() {
		literal := tok.trim_right('uUlL')
		return strconv.parse_int(literal, 0, 64) or { return none }
	}
	if replacement := p.macros[tok] {
		return fastc_target_header_condition(replacement, p.macros, p.depth + 1)
	}
	return none
}

fn fastc_target_header_operator_precedence(operator string) int {
	return match operator {
		'||' { 1 }
		'&&' { 2 }
		'|' { 3 }
		'^' { 4 }
		'&' { 5 }
		'==', '!=' { 6 }
		'<', '<=', '>', '>=' { 7 }
		'<<', '>>' { 8 }
		'+', '-' { 9 }
		'*', '/', '%' { 10 }
		else { 0 }
	}
}

fn (mut p FastcHeaderCondition) expression(minimum int) ?i64 {
	mut left := p.primary()?
	for p.index < p.tokens.len {
		operator := p.tokens[p.index]
		precedence := fastc_target_header_operator_precedence(operator)
		if precedence < minimum { break }
		p.index++
		right := p.expression(precedence + 1)?
		left = fastc_target_header_binary(operator, left, right)?
	}
	return left
}

fn fastc_target_header_binary(operator string, left i64, right i64) ?i64 {
	// Limit arithmetic operands so intermediate operations cannot overflow.
	if operator in ['+', '-', '*', '/', '%', '<<', '>>'] {
		if left < -2147483647 || left > 2147483647 || right < -2147483647 || right > 2147483647 {
			return none
		}
		if operator in ['/', '%'] && right == 0 { return none }
		if operator in ['<<', '>>'] && (left < 0 || right < 0 || right > 31) { return none }
	}
	return match operator {
		'||' { i64(left != 0 || right != 0) }
		'&&' { i64(left != 0 && right != 0) }
		'==' { i64(left == right) }
		'!=' { i64(left != right) }
		'<' { i64(left < right) }
		'<=' { i64(left <= right) }
		'>' { i64(left > right) }
		'>=' { i64(left >= right) }
		'|' { left | right }
		'^' { left ^ right }
		'&' { left & right }
		'+' { left + right }
		'-' { left - right }
		'*' { left * right }
		'/' { left / right }
		'%' { left % right }
		'<<' { left << right }
		'>>' { left >> right }
		else { return none }
	}
}
