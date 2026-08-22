module fastc

import strings
import v3.pref
import v3.scanner
import v3.token

// This file is the scanner-direct optimization. The complete checked backend is
// FlatGen, split across the other files in this module.

const c_preamble = r'#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef intptr_t isize;
typedef uintptr_t usize;
typedef unsigned char byte;
typedef int32_t rune;
typedef float f32;
typedef double f64;
typedef const char *string;
typedef void *voidptr;
typedef unsigned char *byteptr;
typedef char *charptr;

static void v_fastc_print_string(const char *value) { fputs(value, stdout); }
static void v_fastc_print_bool(bool value) { fputs(value ? "true" : "false", stdout); }
static void v_fastc_print_char(char value) { fputc(value, stdout); }
static void v_fastc_print_signed(long long value) { printf("%lld", value); }
static void v_fastc_print_unsigned(unsigned long long value) { printf("%llu", value); }
static void v_fastc_print_float(double value) { printf("%g", value); }
static void v_fastc_println_string(const char *value) { puts(value); }
static void v_fastc_println_bool(bool value) { puts(value ? "true" : "false"); }
static void v_fastc_println_char(char value) { fputc(value, stdout); fputc(10, stdout); }
static void v_fastc_println_signed(long long value) { printf("%lld\n", value); }
static void v_fastc_println_unsigned(unsigned long long value) { printf("%llu\n", value); }
static void v_fastc_println_float(double value) { printf("%g\n", value); }

#define V_FASTC_PRINT_SELECT(value, string_fn, bool_fn, char_fn, signed_fn, unsigned_fn, float_fn) _Generic((value), char *: string_fn, const char *: string_fn, bool: bool_fn, char: char_fn, signed char: signed_fn, short: signed_fn, int: signed_fn, long: signed_fn, long long: signed_fn, unsigned char: unsigned_fn, unsigned short: unsigned_fn, unsigned int: unsigned_fn, unsigned long: unsigned_fn, unsigned long long: unsigned_fn, float: float_fn, double: float_fn)(value)
#define print(value) V_FASTC_PRINT_SELECT(value, v_fastc_print_string, v_fastc_print_bool, v_fastc_print_char, v_fastc_print_signed, v_fastc_print_unsigned, v_fastc_print_float)
#define println(value) V_FASTC_PRINT_SELECT(value, v_fastc_println_string, v_fastc_println_bool, v_fastc_println_char, v_fastc_println_signed, v_fastc_println_unsigned, v_fastc_println_float)
#define assert(value) do { if (!(value)) { fprintf(stderr, "assertion failed: %s\n", #value); abort(); } } while (0)

'

struct DirectGen {
	path string
mut:
	s       scanner.Scanner
	tok     token.Token
	lit     string
	out     strings.Builder
	protos  strings.Builder
	indent  int
	in_main bool
}

// generate scans V source and emits C as each declaration and statement is consumed. It does
// not construct a flat AST or invoke semantic type checking. Unsupported syntax is returned as
// an error so the driver can promote the source to fastc's complete checked lane.
pub fn generate(source string, path string, prefs &pref.Preferences) !string {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut gen := DirectGen{
		path:   path
		s:      scanner.new_scanner(prefs, .normal)
		out:    strings.new_builder(source.len)
		protos: strings.new_builder(256)
	}
	gen.s.init(file, source)
	return gen.run()
}

fn (mut g DirectGen) run() !string {
	g.next()
	for g.tok != .eof {
		g.skip_semicolons()
		if g.tok == .eof {
			break
		}
		if g.tok == .key_module {
			g.parse_module()!
			continue
		}
		if g.tok == .key_pub || g.tok == .key_static {
			g.next()
		}
		if g.tok == .key_fn {
			g.parse_function()!
			continue
		}
		return g.unsupported('top-level `${g.token_source()}`')
	}
	mut result := strings.new_builder(c_preamble.len + g.protos.len + g.out.len + 2)
	result.write_string(c_preamble)
	result.write_string(g.protos.str())
	result.writeln('')
	result.write_string(g.out.str())
	return result.str()
}

fn (mut g DirectGen) next() {
	g.tok = g.s.scan()
	g.lit = g.s.lit
}

fn (mut g DirectGen) skip_semicolons() {
	for g.tok == .semicolon {
		g.next()
	}
}

fn (g &DirectGen) unsupported(feature string) IError {
	return error('fastc does not directly emit ${feature} in ${g.path}')
}

fn (mut g DirectGen) expect(expected token.Token) ! {
	if g.tok != expected {
		return g.unsupported('`${expected.str()}` after `${g.token_source()}`')
	}
	g.next()
}

fn (mut g DirectGen) parse_module() ! {
	g.next()
	if g.tok != .name {
		return g.unsupported('module declaration')
	}
	// A direct single-file unit has no module namespace to resolve. `main` is
	// accepted and discarded; every other module falls through to normal cgen.
	if g.lit != 'main' {
		return g.unsupported('module `${g.lit}`')
	}
	g.next()
	g.skip_semicolons()
}

fn (mut g DirectGen) parse_function() ! {
	g.next()
	if g.tok == .lpar {
		return g.unsupported('methods')
	}
	if g.tok != .name {
		return g.unsupported('function declaration')
	}
	name := g.lit
	g.next()
	if g.tok == .lsbr {
		return g.unsupported('generic functions')
	}
	g.expect(.lpar)!
	params := g.parse_parameters()!
	mut return_type := 'void'
	if g.tok != .lcbr {
		return_type = g.parse_type()!
	}
	g.expect(.lcbr)!
	is_main := name == 'main' && params.len == 0
	c_return_type := if is_main { 'int' } else { return_type }
	c_params := if params.len == 0 { 'void' } else { params.join(', ') }
	g.protos.writeln('${c_return_type} ${name}(${c_params});')
	g.write_line('${c_return_type} ${name}(${c_params}) {')
	g.indent++
	previous_in_main := g.in_main
	g.in_main = is_main
	g.parse_block_body()!
	g.in_main = previous_in_main
	if is_main {
		g.write_line('return 0;')
	}
	g.indent--
	g.write_line('}')
	g.out.writeln('')
}

fn (mut g DirectGen) parse_parameters() ![]string {
	mut params := []string{}
	g.skip_semicolons()
	for g.tok != .rpar {
		if g.tok in [.key_mut, .key_shared] {
			return g.unsupported('mutable or shared parameters')
		}
		if g.tok != .name {
			return g.unsupported('function parameters')
		}
		name := g.lit
		g.next()
		if g.tok == .comma {
			return g.unsupported('grouped parameter names')
		}
		type_name := g.parse_type()!
		params << '${type_name} ${name}'
		if g.tok == .comma {
			g.next()
			g.skip_semicolons()
			continue
		}
		if g.tok != .rpar {
			return g.unsupported('function parameter separator')
		}
	}
	g.next()
	return params
}

fn (mut g DirectGen) parse_type() !string {
	mut pointers := 0
	for g.tok == .amp || g.tok == .mul {
		pointers++
		g.next()
	}
	if g.tok != .name {
		return g.unsupported('type `${g.token_source()}`')
	}
	raw_type := g.lit
	g.next()
	if g.tok in [.dot, .lsbr, .question, .not] {
		return g.unsupported('compound type `${raw_type}`')
	}
	base := match raw_type {
		'bool' { 'bool' }
		'byte' { 'byte' }
		'char' { 'char' }
		'f32' { 'f32' }
		'f64' { 'f64' }
		'i8' { 'i8' }
		'i16' { 'i16' }
		'i32' { 'i32' }
		'i64' { 'i64' }
		'int' { 'int' }
		'isize' { 'isize' }
		'rune' { 'rune' }
		'string' { 'string' }
		'u8' { 'u8' }
		'u16' { 'u16' }
		'u32' { 'u32' }
		'u64' { 'u64' }
		'uint' { 'unsigned int' }
		'usize' { 'usize' }
		'voidptr' { 'voidptr' }
		'byteptr' { 'byteptr' }
		'charptr' { 'charptr' }
		else { raw_type }
	}
	return base + '*'.repeat(pointers)
}

fn (mut g DirectGen) parse_block_body() ! {
	g.skip_semicolons()
	for g.tok != .rcbr {
		if g.tok == .eof {
			return g.unsupported('unfinished block')
		}
		g.parse_statement()!
		g.skip_semicolons()
	}
	g.next()
	g.skip_semicolons()
}

fn (mut g DirectGen) parse_statement() ! {
	match g.tok {
		.key_if {
			g.parse_if()!
		}
		.key_for {
			g.parse_for()!
		}
		.key_return {
			g.parse_return()!
		}
		.key_break {
			g.next()
			g.consume_statement_end()
			g.write_line('break;')
		}
		.key_continue {
			g.next()
			g.consume_statement_end()
			g.write_line('continue;')
		}
		.key_mut {
			g.parse_mutable_declaration()!
		}
		.key_unsafe {
			g.next()
			g.expect(.lcbr)!
			g.parse_block_body()!
		}
		else {
			g.parse_simple_statement()!
		}
	}
}

fn (mut g DirectGen) parse_if() ! {
	g.next()
	condition := g.read_expression([token.Token.lcbr])!
	if condition.len == 0 {
		return g.unsupported('empty if condition')
	}
	g.expect(.lcbr)!
	g.write_line('if (${condition}) {')
	g.indent++
	g.parse_block_body()!
	g.indent--
	if g.tok != .key_else {
		g.write_line('}')
		return
	}
	g.next()
	if g.tok == .key_if {
		g.write_line('} else {')
		g.indent++
		g.parse_if()!
		g.indent--
		g.write_line('}')
		return
	}
	g.expect(.lcbr)!
	g.write_line('} else {')
	g.indent++
	g.parse_block_body()!
	g.indent--
	g.write_line('}')
}

fn (mut g DirectGen) parse_for() ! {
	g.next()
	if g.tok == .lcbr {
		g.next()
		g.write_line('for (;;) {')
		g.indent++
		g.parse_block_body()!
		g.indent--
		g.write_line('}')
		return
	}
	if g.tok == .name {
		name := g.lit
		g.next()
		if g.tok == .key_in {
			g.next()
			start := g.read_expression([token.Token.dotdot])!
			g.expect(.dotdot)!
			end := g.read_expression([token.Token.lcbr])!
			g.expect(.lcbr)!
			g.write_line('for (__typeof__((${start})) ${name} = (${start}); ${name} < (${end}); ${name}++) {')
			g.indent++
			g.parse_block_body()!
			g.indent--
			g.write_line('}')
			return
		}
		if g.tok == .decl_assign {
			g.next()
			initial := g.read_expression([token.Token.semicolon])!
			g.expect(.semicolon)!
			condition := g.read_expression([token.Token.semicolon])!
			g.expect(.semicolon)!
			update := g.read_expression([token.Token.lcbr])!
			g.expect(.lcbr)!
			g.write_line('for (__typeof__((${initial})) ${name} = (${initial}); ${condition}; ${update}) {')
			g.indent++
			g.parse_block_body()!
			g.indent--
			g.write_line('}')
			return
		}
		condition := g.read_expression_with_prefix(name, [token.Token.lcbr])!
		g.expect(.lcbr)!
		g.write_line('while (${condition}) {')
		g.indent++
		g.parse_block_body()!
		g.indent--
		g.write_line('}')
		return
	}
	condition := g.read_expression([token.Token.lcbr])!
	g.expect(.lcbr)!
	g.write_line('while (${condition}) {')
	g.indent++
	g.parse_block_body()!
	g.indent--
	g.write_line('}')
}

fn (mut g DirectGen) parse_return() ! {
	g.next()
	if g.tok == .semicolon || g.tok == .rcbr {
		g.consume_statement_end()
		g.write_line(if g.in_main { 'return 0;' } else { 'return;' })
		return
	}
	expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	g.consume_statement_end()
	g.write_line('return ${expression};')
}

fn (mut g DirectGen) parse_mutable_declaration() ! {
	g.next()
	if g.tok != .name {
		return g.unsupported('mutable declaration')
	}
	name := g.lit
	g.next()
	if g.tok != .decl_assign {
		return g.unsupported('`mut` statement without `:=`')
	}
	g.parse_declaration_after_name(name)!
}

fn (mut g DirectGen) parse_simple_statement() ! {
	if g.tok == .key_assert {
		g.next()
		expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
		g.consume_statement_end()
		g.write_line('assert(${expression});')
		return
	}
	if g.tok == .name {
		name := g.lit
		g.next()
		if g.tok == .decl_assign {
			g.parse_declaration_after_name(name)!
			return
		}
		expression :=
			g.read_expression_with_prefix(name, [token.Token.semicolon, token.Token.rcbr])!
		g.consume_statement_end()
		g.write_line('${expression};')
		return
	}
	expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	if expression.len == 0 {
		return g.unsupported('statement `${g.token_source()}`')
	}
	g.consume_statement_end()
	g.write_line('${expression};')
}

fn (mut g DirectGen) parse_declaration_after_name(name string) ! {
	g.next()
	expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	if expression.len == 0 {
		return g.unsupported('empty declaration')
	}
	g.consume_statement_end()
	// GNU typeof is unevaluated and is supported by bundled TinyCC. It lets the
	// direct path preserve V's `:=` without running any inference or type checker.
	if expression.starts_with('"') {
		// C's typeof preserves a literal's array type instead of applying the usual
		// pointer decay. The spelling alone is enough to lower this case.
		g.write_line('string ${name} = (${expression});')
	} else {
		g.write_line('__typeof__((${expression})) ${name} = (${expression});')
	}
}

fn (mut g DirectGen) consume_statement_end() {
	if g.tok == .semicolon {
		g.next()
	}
}

fn (mut g DirectGen) read_expression(stops []token.Token) !string {
	return g.read_expression_with_prefix('', stops)
}

fn (mut g DirectGen) read_expression_with_prefix(prefix string, stops []token.Token) !string {
	mut result := strings.new_builder(64)
	if prefix.len > 0 {
		result.write_string(prefix)
	}
	mut paren_depth := 0
	mut bracket_depth := 0
	for g.tok != .eof {
		if paren_depth == 0 && bracket_depth == 0 && g.tok in stops {
			break
		}
		if g.tok in [.lcbr, .rcbr, .str_dollar, .key_match, .key_or, .key_as, .key_is, .not_is,
			.key_in, .not_in, .arrow, .power, .right_shift_unsigned] {
			return g.unsupported('expression token `${g.token_source()}`')
		}
		piece := g.expression_token()!
		if result.len > 0 && fastc_needs_space(result.last(), piece) {
			result.write_u8(` `)
		}
		result.write_string(piece)
		match g.tok {
			.lpar {
				paren_depth++
			}
			.rpar {
				if paren_depth == 0 {
					break
				}
				paren_depth--
			}
			.lsbr {
				bracket_depth++
			}
			.rsbr {
				if bracket_depth == 0 {
					break
				}
				bracket_depth--
			}
			else {}
		}
		g.next()
	}
	if paren_depth != 0 || bracket_depth != 0 {
		return g.unsupported('unbalanced expression')
	}
	return result.str().trim_space()
}

fn (g &DirectGen) expression_token() !string {
	return match g.tok {
		.name { g.lit }
		.number { g.lit.replace('_', '') }
		.string { fastc_c_string(g.lit)! }
		.char { fastc_c_char(g.lit)! }
		.key_true { 'true' }
		.key_false { 'false' }
		.key_nil { 'NULL' }
		.key_sizeof { 'sizeof' }
		.key_likely, .key_unlikely { '' }
		.semicolon { ';' }
		else { g.tok.str() }
	}
}

fn (g &DirectGen) token_source() string {
	if g.lit.len > 0 {
		return g.lit
	}
	return g.tok.str()
}

fn (mut g DirectGen) write_line(line string) {
	for _ in 0 .. g.indent {
		g.out.write_u8(`\t`)
	}
	g.out.writeln(line)
}

fn fastc_needs_space(last u8, next string) bool {
	if next.len == 0 {
		return false
	}
	return (last.is_alnum() || last == `_`) && (next[0].is_alnum() || next[0] == `_`)
}

fn fastc_c_string(literal string) !string {
	if literal.len < 2 {
		return error('invalid fastc string literal')
	}
	mut raw := literal
	mut is_raw := false
	if raw[0] == `r` && raw.len >= 3 {
		is_raw = true
		raw = raw[1..]
	}
	quote := raw[0]
	if quote !in [`'`, `"`] || raw[raw.len - 1] != quote {
		return error('interpolated or unfinished fastc string literal')
	}
	mut result := strings.new_builder(raw.len + 2)
	result.write_u8(`"`)
	mut i := 1
	for i < raw.len - 1 {
		c := raw[i]
		if c == `\\` && !is_raw && i + 1 < raw.len - 1 {
			result.write_u8(c)
			result.write_u8(raw[i + 1])
			i += 2
			continue
		} else if c == `"` {
			result.write_string('\\"')
		} else if c == `\\` && is_raw {
			result.write_string('\\\\')
		} else {
			result.write_u8(c)
		}
		i++
	}
	result.write_u8(`"`)
	return result.str()
}

fn fastc_c_char(literal string) !string {
	mut value := literal
	if value.starts_with('c:') {
		value = value[2..]
	}
	if value.len == 0 || value.contains("'") {
		return error('unsupported fastc character literal')
	}
	return "'${value}'"
}
