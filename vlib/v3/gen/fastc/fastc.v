module fastc

import strings
import v3.pref
import v3.scanner
import v3.token

// FastC parses scanner tokens and emits C immediately. It deliberately has no
// AST, semantic-checker, transformer, mark-used, or conventional cgen path.

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
static void v_fastc_println_string(const char *value) { puts(value); }
static void v_fastc_println_bool(bool value) { puts(value ? "true" : "false"); }
static void v_fastc_println_char(char value) { fputc(value, stdout); fputc(10, stdout); }
static void v_fastc_println_signed(long long value) { printf("%lld\n", value); }
static void v_fastc_println_unsigned(unsigned long long value) { printf("%llu\n", value); }

/* Float formatting belongs to the V strconv routines. Leaving float and double
 * unmatched makes TinyCC reject unsupported printing instead of silently
 * applying printf %g semantics. */
#define V_FASTC_PRINT_SELECT(value, string_fn, bool_fn, char_fn, signed_fn, unsigned_fn) _Generic((value), char *: string_fn, const char *: string_fn, bool: bool_fn, char: char_fn, signed char: signed_fn, short: signed_fn, int: signed_fn, long: signed_fn, long long: signed_fn, unsigned char: unsigned_fn, unsigned short: unsigned_fn, unsigned int: unsigned_fn, unsigned long: unsigned_fn, unsigned long long: unsigned_fn)(value)
#define print(value) V_FASTC_PRINT_SELECT(value, v_fastc_print_string, v_fastc_print_bool, v_fastc_print_char, v_fastc_print_signed, v_fastc_print_unsigned)
#define println(value) V_FASTC_PRINT_SELECT(value, v_fastc_println_string, v_fastc_println_bool, v_fastc_println_char, v_fastc_println_signed, v_fastc_println_unsigned)

'

struct FastcFunctionSignature {
	parameter_types []string
	return_type     string
}

struct FastcLocal {
	is_mut bool
	typ    string
}

struct FastcExpressionToken {
	tok token.Token
	lit string
}

struct Parser {
	path string
mut:
	s                    scanner.Scanner
	tok                  token.Token
	lit                  string
	out                  strings.Builder
	protos               strings.Builder
	indent               int
	in_main              bool
	has_main             bool
	unsafe_depth         int
	temp_id              int
	locals               map[string]FastcLocal
	functions            map[string]FastcFunctionSignature
	return_type          string
	last_expression_type string
	last_expression      []FastcExpressionToken
}

// generate scans V source and emits C as each declaration and statement is consumed. It does
// not construct an AST or invoke semantic type checking. Unsupported syntax is returned as an
// error; FastC never retries through an AST-based backend.
pub fn generate(source string, path string, prefs &pref.Preferences) !string {
	functions := collect_function_signatures(source, path, prefs)!
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut gen := Parser{
		path:      path
		s:         scanner.new_scanner(prefs, .normal)
		out:       strings.new_builder(source.len)
		protos:    strings.new_builder(256)
		functions: functions
	}
	gen.s.init(file, source)
	generated := gen.run()!
	if gen.s.diagnostics.len > 0 {
		diagnostic := gen.s.diagnostics[0]
		return error('fastc scanner error at byte ${diagnostic.offset} in ${path}: ${diagnostic.message}')
	}
	return generated
}

fn collect_function_signatures(source string, path string, prefs &pref.Preferences) !map[string]FastcFunctionSignature {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut functions := map[string]FastcFunctionSignature{}
	mut brace_depth := 0
	mut tok := scan.scan()
	for tok != .eof {
		if tok == .key_fn && brace_depth == 0 {
			tok = scan.scan()
			if tok != .name {
				return error('fastc parser does not support function declaration in ${path}')
			}
			name := scan.lit
			if name in functions {
				return error('fastc parser does not support duplicate function `${name}` in ${path}')
			}
			tok = scan.scan()
			if tok != .lpar {
				return error('fastc parser does not support function `${name}` declaration in ${path}')
			}
			tok = scan.scan()
			mut parameter_types := []string{}
			for tok != .rpar {
				if tok in [.key_mut, .key_shared] {
					return error('fastc parser does not support mutable or shared parameters in ${path}')
				}
				if tok != .name {
					return error('fastc parser does not support function parameters in ${path}')
				}
				tok = scan.scan()
				if tok == .comma {
					return error('fastc parser does not support grouped parameter names in ${path}')
				}
				parameter_type, next_token := fastc_scan_type(mut scan, tok, path)!
				parameter_types << parameter_type
				tok = next_token
				if tok == .comma {
					tok = scan.scan()
					continue
				}
				if tok != .rpar {
					return error('fastc parser does not support function parameter separator in ${path}')
				}
			}
			tok = scan.scan()
			mut return_type := 'void'
			if tok != .lcbr {
				return_type, tok = fastc_scan_type(mut scan, tok, path)!
			}
			if tok != .lcbr {
				return error('fastc parser does not support function `${name}` body in ${path}')
			}
			functions[name] = FastcFunctionSignature{
				parameter_types: parameter_types
				return_type:     return_type
			}
			continue
		}
		if tok == .lcbr {
			brace_depth++
		} else if tok == .rcbr && brace_depth > 0 {
			brace_depth--
		}
		tok = scan.scan()
	}
	return functions
}

fn fastc_scan_type(mut scan scanner.Scanner, first token.Token, path string) !(string, token.Token) {
	mut tok := first
	mut pointers := 0
	for tok == .amp || tok == .mul {
		pointers++
		tok = scan.scan()
	}
	if tok != .name {
		return error('fastc parser does not support type `${tok.str()}` in ${path}')
	}
	raw_type := scan.lit
	base := fastc_primitive_c_type(raw_type) or {
		return error('fastc parser does not support undeclared type `${raw_type}` in ${path}')
	}
	tok = scan.scan()
	if tok in [.dot, .lsbr, .question, .not] {
		return error('fastc parser does not support compound type `${raw_type}` in ${path}')
	}
	return base + '*'.repeat(pointers), tok
}

fn (mut g Parser) run() !string {
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
		if g.tok == .key_import {
			return g.unsupported('top-level `${g.token_source()}`')
		}
		if g.has_main {
			return g.unsupported('top-level `${g.token_source()}` after `main`')
		}
		g.parse_script()!
		break
	}
	mut result := strings.new_builder(c_preamble.len + g.protos.len + g.out.len + 2)
	result.write_string(c_preamble)
	result.write_string(g.protos.str())
	result.writeln('')
	result.write_string(g.out.str())
	return result.str()
}

fn (mut g Parser) next() {
	g.tok = g.s.scan()
	g.lit = g.s.lit
}

fn (mut g Parser) temporary_name(kind string) string {
	name := '__v_fastc_${kind}_${g.temp_id}'
	g.temp_id++
	return name
}

fn (mut g Parser) skip_semicolons() {
	for g.tok == .semicolon {
		g.next()
	}
}

fn (g &Parser) unsupported(feature string) IError {
	return error('fastc parser does not support ${feature} in ${g.path}')
}

fn (mut g Parser) expect(expected token.Token) ! {
	if g.tok != expected {
		return g.unsupported('`${expected.str()}` after `${g.token_source()}`')
	}
	g.next()
}

fn (mut g Parser) parse_module() ! {
	g.next()
	if g.tok != .name {
		return g.unsupported('module declaration')
	}
	// A single-file unit has no module namespace to resolve. `main` is accepted
	// and discarded; every other module is reported as unsupported.
	if g.lit != 'main' {
		return g.unsupported('module `${g.lit}`')
	}
	g.next()
	g.skip_semicolons()
}

fn (mut g Parser) parse_function() ! {
	g.locals = map[string]FastcLocal{}
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
	if fastc_has_narrow_integer_type(return_type)
		|| params.any(fastc_parameter_has_narrow_integer_type) {
		// C promotes narrow operands before arithmetic, while V retains the narrow
		// result type. Reject them until the direct parser tracks the required type.
		return g.unsupported('narrow integer function types')
	}
	if name == 'main' {
		if params.len > 0 {
			return g.unsupported('main function with parameters')
		}
		if return_type != 'void' {
			return g.unsupported('main function returning `${return_type}`')
		}
	}
	g.expect(.lcbr)!
	is_main := name == 'main'
	if is_main {
		g.has_main = true
	}
	c_return_type := if is_main { 'int' } else { return_type }
	c_params := if params.len == 0 { 'void' } else { params.join(', ') }
	g.protos.writeln('${c_return_type} ${name}(${c_params});')
	g.write_line('${c_return_type} ${name}(${c_params}) {')
	g.indent++
	if is_main {
		g.write_line('setvbuf(stdout, NULL, _IONBF, 0);')
	}
	previous_in_main := g.in_main
	previous_return_type := g.return_type
	g.in_main = is_main
	g.return_type = return_type
	terminates := g.parse_block_body()!
	g.in_main = previous_in_main
	g.return_type = previous_return_type
	if return_type != 'void' && !terminates {
		return g.unsupported('non-void function `${name}` that can fall through')
	}
	if is_main {
		g.write_line('return 0;')
	}
	g.indent--
	g.write_line('}')
	g.out.writeln('')
}

fn (mut g Parser) parse_script() ! {
	g.locals = map[string]FastcLocal{}
	g.has_main = true
	g.protos.writeln('int main(void);')
	g.write_line('int main(void) {')
	g.indent++
	g.write_line('setvbuf(stdout, NULL, _IONBF, 0);')
	g.in_main = true
	g.skip_semicolons()
	for g.tok != .eof {
		if g.tok in [.key_module, .key_pub, .key_static, .key_fn] {
			return g.unsupported('declaration after top-level statements')
		}
		_ = g.parse_statement()!
		g.skip_semicolons()
	}
	g.write_line('return 0;')
	g.indent--
	g.write_line('}')
	g.out.writeln('')
}

fn (mut g Parser) parse_parameters() ![]string {
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
		g.locals[name] = FastcLocal{
			typ: type_name
		}
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

fn (mut g Parser) parse_type() !string {
	mut pointers := 0
	for g.tok == .amp || g.tok == .mul {
		pointers++
		g.next()
	}
	if g.tok != .name {
		return g.unsupported('type `${g.token_source()}`')
	}
	raw_type := g.lit
	if raw_type == 'charptr' || (raw_type == 'char' && pointers > 0) {
		return g.unsupported('character pointer types')
	}
	if raw_type == 'rune' {
		return g.unsupported('rune types')
	}
	g.next()
	if g.tok in [.dot, .lsbr, .question, .not] {
		return g.unsupported('compound type `${raw_type}`')
	}
	base := fastc_primitive_c_type(raw_type) or {
		return g.unsupported('undeclared type `${raw_type}`')
	}
	return base + '*'.repeat(pointers)
}

fn fastc_primitive_c_type(raw_type string) ?string {
	return match raw_type {
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
		else { none }
	}
}

fn fastc_has_narrow_integer_type(type_name string) bool {
	return type_name.trim_right('*') in ['byte', 'char', 'i8', 'i16', 'u8', 'u16']
}

fn fastc_parameter_has_narrow_integer_type(parameter string) bool {
	fields := parameter.fields()
	return fields.len > 0 && fastc_has_narrow_integer_type(fields[0])
}

fn (mut g Parser) parse_block_body() !bool {
	mut terminates := false
	g.skip_semicolons()
	for g.tok != .rcbr {
		if g.tok == .eof {
			return g.unsupported('unfinished block')
		}
		statement_terminates := g.parse_statement()!
		if statement_terminates {
			terminates = true
		}
		g.skip_semicolons()
	}
	g.next()
	g.skip_semicolons()
	return terminates
}

fn (mut g Parser) parse_statement() !bool {
	return match g.tok {
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
			false
		}
		.key_continue {
			g.next()
			g.consume_statement_end()
			g.write_line('continue;')
			false
		}
		.key_mut {
			g.parse_mutable_declaration()!
			false
		}
		.key_unsafe {
			g.next()
			g.expect(.lcbr)!
			g.unsafe_depth++
			terminates := g.parse_block_body()!
			g.unsafe_depth--
			terminates
		}
		else {
			g.parse_simple_statement()!
			false
		}
	}
}

fn (mut g Parser) parse_if() !bool {
	g.next()
	condition := g.read_expression([token.Token.lcbr])!
	if condition.len == 0 {
		return g.unsupported('empty if condition')
	}
	g.require_boolean_condition('if')!
	g.expect(.lcbr)!
	g.write_line('if (${condition}) {')
	g.indent++
	then_terminates := g.parse_block_body()!
	g.indent--
	if g.tok != .key_else {
		g.write_line('}')
		return false
	}
	g.next()
	if g.tok == .key_if {
		g.write_line('} else {')
		g.indent++
		else_terminates := g.parse_if()!
		g.indent--
		g.write_line('}')
		return then_terminates && else_terminates
	}
	g.expect(.lcbr)!
	g.write_line('} else {')
	g.indent++
	else_terminates := g.parse_block_body()!
	g.indent--
	g.write_line('}')
	return then_terminates && else_terminates
}

fn (mut g Parser) parse_for() !bool {
	g.next()
	if g.tok == .lcbr {
		g.next()
		g.write_line('for (;;) {')
		g.indent++
		_ = g.parse_block_body()!
		g.indent--
		g.write_line('}')
		return false
	}
	if g.tok == .name {
		name := g.lit
		g.next()
		if g.tok == .key_in {
			if name in g.locals {
				return g.unsupported('redeclaration of `${name}`')
			}
			g.next()
			start := g.read_expression([token.Token.dotdot])!
			start_expression_type := g.last_expression_type
			g.expect(.dotdot)!
			end := g.read_expression([token.Token.lcbr])!
			end_expression_type := g.last_expression_type
			if !fastc_is_integer_expression_type(start_expression_type)
				|| !fastc_is_integer_expression_type(end_expression_type) {
				return g.unsupported('range bounds of types `${start_expression_type}` and `${end_expression_type}` must both be integers')
			}
			g.expect(.lcbr)!
			start_name := g.temporary_name('range_start')
			end_name := g.temporary_name('range_end')
			// V evaluates both range bounds exactly once, from left to right.
			g.write_line('__typeof__((${start})) ${start_name} = (${start});')
			g.write_line('__typeof__((${end})) ${end_name} = (${end});')
			g.write_line('for (__typeof__((${start_name})) ${name} = (${start_name}); ${name} < (${end_name}); ${name}++) {')
			g.locals[name] = FastcLocal{
				typ: fastc_normalize_inferred_type(start_expression_type)
			}
			g.indent++
			_ = g.parse_block_body()!
			g.indent--
			g.locals.delete(name)
			g.write_line('}')
			return false
		}
		if g.tok == .decl_assign {
			if name in g.locals {
				return g.unsupported('redeclaration of `${name}`')
			}
			g.next()
			initial := g.read_expression([token.Token.semicolon])!
			initial_type := fastc_normalize_inferred_type(g.last_expression_type)
			g.expect(.semicolon)!
			g.locals[name] = FastcLocal{
				is_mut: true
				typ:    initial_type
			}
			condition := g.read_expression([token.Token.semicolon])!
			g.require_boolean_condition('for')!
			g.expect(.semicolon)!
			update := g.read_expression([token.Token.lcbr])!
			g.expect(.lcbr)!
			g.write_line('for (__typeof__((${initial})) ${name} = (${initial}); ${condition}; ${update}) {')
			g.indent++
			_ = g.parse_block_body()!
			g.indent--
			g.locals.delete(name)
			g.write_line('}')
			return false
		}
		g.validate_expression_name(name, .unknown)!
		condition := g.read_expression_with_prefix(name, [token.Token.lcbr])!
		g.require_boolean_condition('for')!
		g.expect(.lcbr)!
		g.write_line('while (${condition}) {')
		g.indent++
		_ = g.parse_block_body()!
		g.indent--
		g.write_line('}')
		return false
	}
	condition := g.read_expression([token.Token.lcbr])!
	g.require_boolean_condition('for')!
	g.expect(.lcbr)!
	g.write_line('while (${condition}) {')
	g.indent++
	_ = g.parse_block_body()!
	g.indent--
	g.write_line('}')
	return false
}

fn (mut g Parser) parse_return() !bool {
	g.next()
	if g.tok == .semicolon || g.tok == .rcbr {
		if !g.in_main && g.return_type != 'void' {
			return g.unsupported('bare return in non-void function')
		}
		g.consume_statement_end()
		g.write_line(if g.in_main { 'return 0;' } else { 'return;' })
		return true
	}
	if g.return_type == 'void' {
		return g.unsupported('value return in void function')
	}
	expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	actual_type := g.last_expression_type
	if actual_type.len == 0 {
		return g.unsupported('unverifiable return expression type')
	}
	if !fastc_call_types_are_compatible(actual_type, g.return_type) {
		return g.unsupported('return expression of type `${actual_type}` in function returning `${g.return_type}`')
	}
	g.consume_statement_end()
	g.write_line('return ${expression};')
	return true
}

fn (g &Parser) require_boolean_condition(kind string) ! {
	if g.last_expression_type.len == 0 {
		return g.unsupported('unverifiable ${kind} condition type')
	}
	if g.last_expression_type != 'bool' {
		return g.unsupported('${kind} condition of type `${g.last_expression_type}` instead of `bool`')
	}
}

fn (mut g Parser) parse_mutable_declaration() ! {
	g.next()
	if g.tok != .name {
		return g.unsupported('mutable declaration')
	}
	name := g.lit
	g.next()
	if g.tok != .decl_assign {
		return g.unsupported('`mut` statement without `:=`')
	}
	g.parse_declaration_after_name(name, true)!
}

fn (mut g Parser) parse_simple_statement() ! {
	if g.tok == .key_assert {
		return g.unsupported('assert statements')
	}
	if g.tok == .name {
		name := g.lit
		g.next()
		if g.tok == .decl_assign {
			g.parse_declaration_after_name(name, false)!
			return
		}
		if (g.tok.is_assignment() || g.tok in [.inc, .dec])
			&& (name !in g.locals || !g.locals[name].is_mut) {
			return g.unsupported('mutation of immutable or unknown name `${name}`')
		}
		g.validate_expression_name(name, .unknown)!
		if g.tok.is_assignment() {
			if g.tok in [.left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign] {
				return g.unsupported('shift expressions')
			}
			if g.tok in [.div_assign, .mod_assign] {
				return g.unsupported('division or modulo expressions')
			}
			operator := g.tok
			g.next()
			value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
			if value.len == 0 {
				return g.unsupported('empty assignment to `${name}`')
			}
			actual_type := g.last_expression_type
			expected_type := g.locals[name].typ
			if actual_type.len == 0 || expected_type.len == 0 {
				return g.unsupported('unverifiable assignment type for `${name}`')
			}
			if !fastc_call_types_are_compatible(actual_type, expected_type) {
				return g.unsupported('assignment of type `${actual_type}` to `${name}` of type `${expected_type}`')
			}
			if operator != .assign && (!fastc_is_numeric_expression_type(actual_type)
				|| !fastc_is_numeric_expression_type(expected_type)) {
				return g.unsupported('arithmetic assignment `${operator.str()}` on non-numeric type `${expected_type}`')
			}
			g.consume_statement_end()
			g.write_line('${name}${operator.str()}${value};')
			return
		}
		expression :=
			g.read_expression_with_prefix(name, [token.Token.semicolon, token.Token.rcbr])!
		if !g.last_expression_is_statement() {
			return g.unsupported('value-only expression statement')
		}
		g.consume_statement_end()
		g.write_line('${expression};')
		return
	}
	expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	if expression.len == 0 {
		return g.unsupported('statement `${g.token_source()}`')
	}
	return g.unsupported('value-only expression statement')
}

fn (g &Parser) last_expression_is_statement() bool {
	tokens := g.last_expression
	if tokens.len == 2 && tokens[0].tok == .name && tokens[1].tok in [.inc, .dec] {
		return true
	}
	if tokens.len < 3 || tokens[0].tok != .name || tokens[1].tok != .lpar {
		return false
	}
	call_close := fastc_matching_rpar(tokens, 1) or { return false }
	if call_close != tokens.len - 1 {
		return false
	}
	name := tokens[0].lit
	return name in g.functions || name in ['print', 'println']
}

fn (mut g Parser) parse_declaration_after_name(name string, is_mut bool) ! {
	if name in g.locals {
		return g.unsupported('redeclaration of `${name}`')
	}
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
	g.locals[name] = FastcLocal{
		is_mut: is_mut
		typ:    fastc_normalize_inferred_type(g.last_expression_type)
	}
}

fn fastc_normalize_inferred_type(typ string) string {
	return match typ {
		'integer literal' { 'int' }
		'float literal' { 'f64' }
		else { typ }
	}
}

fn (mut g Parser) consume_statement_end() {
	if g.tok == .semicolon {
		g.next()
	}
}

fn (mut g Parser) read_expression(stops []token.Token) !string {
	return g.read_expression_with_prefix('', stops)
}

fn (mut g Parser) read_expression_with_prefix(prefix string, stops []token.Token) !string {
	mut result := strings.new_builder(64)
	mut expression_tokens := []FastcExpressionToken{}
	if prefix.len > 0 {
		result.write_string(prefix)
		expression_tokens << FastcExpressionToken{
			tok: .name
			lit: prefix
		}
	}
	mut paren_depth := 0
	mut has_sum_arithmetic_operator := false
	mut has_multiply_operator := false
	mut has_and_operator := false
	mut has_pipe_operator := false
	mut has_xor_operator := false
	mut previous_token := token.Token.unknown
	for g.tok != .eof {
		if paren_depth == 0 && g.tok in stops {
			break
		}
		if paren_depth == 0 && g.tok == .comma {
			// V's top-level commas form simultaneous multi-target assignments.
			// Copying them to C would instead emit comma operators.
			return g.unsupported('parallel assignments')
		}
		if g.tok in [.eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .not] {
			// C represents comparison and logical results as int. Without V type
			// information, accepting them here would make generic printing and
			// inferred locals observe 0/1 instead of false/true.
			return g.unsupported('comparison or logical expressions')
		}
		if g.tok in [.left_shift, .right_shift, .right_shift_unsigned, .left_shift_assign,
			.right_shift_assign, .right_shift_unsigned_assign] {
			// V defines oversized shifts to produce zero. Raw C shifts are
			// undefined and may mask the count to the operand width instead.
			return g.unsupported('shift expressions')
		}
		if g.tok in [.div, .div_assign, .mod, .mod_assign] {
			// Integer division and modulo require V's runtime zero checks. This
			// scanner-only lane has no type information to add them selectively.
			return g.unsupported('division or modulo expressions')
		}
		if g.tok == .key_sizeof {
			// Direct C representations can differ from V layouts. Reject sizeof
			// until the parser tracks enough V type information to lower it.
			return g.unsupported('sizeof expressions')
		}
		if g.tok in [.lsbr, .rsbr] {
			// Indexing requires V element types and bounds checks. C pointer/array
			// indexing cannot preserve either in this scanner-only lane.
			return g.unsupported('expression token `${g.token_source()}`')
		}
		if g.tok in [.lcbr, .rcbr, .str_dollar, .key_match, .key_or, .key_as, .key_is, .not_is,
			.key_in, .not_in, .arrow, .power] {
			return g.unsupported('expression token `${g.token_source()}`')
		}
		match g.tok {
			.plus, .minus {
				has_sum_arithmetic_operator = true
			}
			.mul {
				has_multiply_operator = true
			}
			.amp {
				has_and_operator = true
			}
			.pipe {
				has_pipe_operator = true
			}
			.xor {
				has_xor_operator = true
			}
			else {}
		}
		if (has_sum_arithmetic_operator && (has_and_operator || has_pipe_operator
			|| has_xor_operator)) || (has_multiply_operator && has_and_operator)
			|| (has_pipe_operator && has_xor_operator) {
			// V groups + and - with | and ^, and * with &, while C splits those
			// levels and also orders + and - above &. Reject ambiguous token streams.
			return g.unsupported('mixed operator precedence')
		}
		expression_tokens << FastcExpressionToken{
			tok: g.tok
			lit: g.lit
		}
		piece := g.expression_token(previous_token)!
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
			else {}
		}
		previous_token = g.tok
		g.next()
	}
	if paren_depth != 0 {
		return g.unsupported('unbalanced expression')
	}
	g.validate_expression_calls(expression_tokens)!
	g.last_expression_type = g.infer_expression_type(expression_tokens)!
	g.last_expression = expression_tokens
	return result.str().trim_space()
}

fn (g &Parser) expression_token(previous token.Token) !string {
	return match g.tok {
		.name { g.expression_name(previous)! }
		.number { fastc_c_number(g.lit)! }
		.string { fastc_c_string(g.lit)! }
		.char { g.unsupported('rune or C character literals') }
		// stdbool's true/false macros have C type int. Cast them so _Generic
		// dispatch preserves V's bool type when no operator requires promotion.
		.key_true { '((bool)true)' }
		.key_false { '((bool)false)' }
		.key_nil { g.nil_expression()! }
		.key_likely, .key_unlikely { '' }
		.semicolon { ';' }
		else { g.tok.str() }
	}
}

fn (g &Parser) nil_expression() !string {
	if g.unsafe_depth == 0 {
		return g.unsupported('`nil` outside an `unsafe` block')
	}
	return 'NULL'
}

fn (g &Parser) expression_name(previous token.Token) !string {
	g.validate_expression_name(g.lit, previous)!
	return g.lit
}

fn (g &Parser) validate_expression_name(name string, previous token.Token) ! {
	if fastc_has_narrow_integer_type(name) {
		// C promotes narrow operands before arithmetic. Reject narrow casts in
		// expressions until FastC can explicitly restore V's wrapping result type.
		return g.unsupported('narrow integer cast expressions')
	}
	if name == 'charptr' {
		return g.unsupported('charptr expressions')
	}
	if name == 'rune' {
		return g.unsupported('rune expressions')
	}
	if previous == .dot || name in g.locals || name in g.functions
		|| name in ['print', 'println', 'bool', 'byte', 'char', 'f32', 'f64', 'i8', 'i16', 'i32', 'i64', 'int', 'isize', 'string', 'u8', 'u16', 'u32', 'u64', 'uint', 'usize', 'voidptr', 'byteptr'] {
		return
	}
	return g.unsupported('unresolved name `${name}`')
}

fn (g &Parser) validate_expression_calls(tokens []FastcExpressionToken) ! {
	mut i := 0
	for i + 1 < tokens.len {
		if tokens[i].tok != .name || tokens[i + 1].tok != .lpar {
			i++
			continue
		}
		call_end := fastc_matching_rpar(tokens, i + 1) or {
			return g.unsupported('unbalanced function call `${tokens[i].lit}`')
		}
		call_args := fastc_call_arguments(tokens, i + 1, call_end) or {
			return g.unsupported('function call `${tokens[i].lit}` arguments')
		}
		for argument in call_args {
			g.validate_expression_calls(argument)!
		}
		name := tokens[i].lit
		if signature := g.functions[name] {
			if call_args.len != signature.parameter_types.len {
				return g.unsupported('function `${name}` call with ${call_args.len} arguments instead of ${signature.parameter_types.len}')
			}
			for argument_index, argument in call_args {
				actual_type := g.infer_expression_type(argument)!
				expected_type := signature.parameter_types[argument_index]
				if actual_type.len == 0 {
					return g.unsupported('unverifiable argument ${argument_index + 1} to function `${name}`')
				}
				if !fastc_call_types_are_compatible(actual_type, expected_type) {
					return g.unsupported('argument ${argument_index + 1} of type `${actual_type}` to function `${name}` expecting `${expected_type}`')
				}
			}
		} else if name in ['print', 'println'] {
			if call_args.len != 1 {
				return g.unsupported('function `${name}` call with ${call_args.len} arguments')
			}
			_ = g.infer_expression_type(call_args[0])!
		} else if _ := fastc_primitive_c_type(name) {
			if call_args.len != 1 {
				return g.unsupported('cast `${name}` with ${call_args.len} arguments')
			}
		} else {
			return g.unsupported('unresolved function call `${name}`')
		}
		i = call_end + 1
	}
}

fn fastc_matching_rpar(tokens []FastcExpressionToken, open int) ?int {
	mut depth := 0
	for i in open .. tokens.len {
		match tokens[i].tok {
			.lpar {
				depth++
			}
			.rpar {
				depth--
				if depth == 0 {
					return i
				}
			}
			else {}
		}
	}
	return none
}

fn fastc_call_arguments(tokens []FastcExpressionToken, open int, close int) ![][]FastcExpressionToken {
	if open + 1 == close {
		return [][]FastcExpressionToken{}
	}
	mut call_args := [][]FastcExpressionToken{}
	mut start := open + 1
	mut depth := 0
	for i in open + 1 .. close {
		match tokens[i].tok {
			.lpar {
				depth++
			}
			.rpar {
				depth--
			}
			.comma {
				if depth == 0 {
					if start == i {
						return error('empty fastc function argument')
					}
					call_args << tokens[start..i]
					start = i + 1
				}
			}
			else {}
		}
	}
	if start == close {
		return error('empty fastc function argument')
	}
	call_args << tokens[start..close]
	return call_args
}

fn (g &Parser) infer_expression_type(tokens []FastcExpressionToken) !string {
	if tokens.len == 0 {
		return ''
	}
	mut start := 0
	mut end := tokens.len
	for end - start >= 2 && tokens[start].tok == .lpar {
		wrapper_end := fastc_matching_rpar(tokens[start..end], 0) or { break }
		if wrapper_end != end - start - 1 {
			break
		}
		start++
		end--
	}
	if start >= end {
		return ''
	}
	if end - start == 1 {
		item := tokens[start]
		return match item.tok {
			.name {
				if local := g.locals[item.lit] {
					local.typ
				} else {
					''
				}
			}
			.number {
				fastc_number_expression_type(item.lit)
			}
			.string {
				'string'
			}
			.key_true, .key_false {
				'bool'
			}
			.key_nil {
				'nil'
			}
			else {
				''
			}
		}
	}
	if tokens[start].tok == .name && start + 1 < end && tokens[start + 1].tok == .lpar {
		if close := fastc_matching_rpar(tokens[start..end], 1) {
			if close == end - start - 1 {
				name := tokens[start].lit
				if signature := g.functions[name] {
					return signature.return_type
				}
				if primitive := fastc_primitive_c_type(name) {
					return primitive
				}
				return ''
			}
		}
	}
	if tokens[start].tok in [.plus, .minus] {
		operand_type := g.infer_expression_type(tokens[start + 1..end])!
		if !fastc_is_numeric_expression_type(operand_type) {
			return g.unsupported('arithmetic `${tokens[start].tok.str()}` on non-numeric type `${operand_type}`')
		}
		return operand_type
	}
	if tokens[start].tok == .bit_not {
		operand_type := g.infer_expression_type(tokens[start + 1..end])!
		if !fastc_is_integer_expression_type(operand_type) {
			return g.unsupported('bitwise negation of non-integer type `${operand_type}`')
		}
		return operand_type
	}
	if tokens[end - 1].tok in [.inc, .dec] {
		operand_type := g.infer_expression_type(tokens[start..end - 1])!
		if !fastc_is_numeric_expression_type(operand_type) {
			return g.unsupported('arithmetic `${tokens[end - 1].tok.str()}` on non-numeric type `${operand_type}`')
		}
		return operand_type
	}
	mut depth := 0
	for i in start .. end {
		match tokens[i].tok {
			.lpar { depth++ }
			.rpar { depth-- }
			else {}
		}
		if depth != 0 {
			continue
		}
		if tokens[i].tok.is_assignment() {
			return g.infer_expression_type(tokens[start..i])!
		}
		if tokens[i].tok in [.plus, .minus, .mul, .amp, .pipe, .xor] && i > start {
			left_type := g.infer_expression_type(tokens[start..i])!
			right_type := g.infer_expression_type(tokens[i + 1..end])!
			common_type := fastc_common_arithmetic_type(left_type, right_type)
			if common_type.len == 0 {
				return g.unsupported('arithmetic `${tokens[i].tok.str()}` operands of types `${left_type}` and `${right_type}`')
			}
			return common_type
		}
	}
	return ''
}

fn fastc_number_expression_type(literal string) string {
	clean := literal.replace('_', '')
	if clean.contains('.') || (!(clean.starts_with('0x') || clean.starts_with('0X'))
		&& clean.contains_any('eE')) {
		return 'float literal'
	}
	return 'integer literal'
}

fn fastc_common_arithmetic_type(left string, right string) string {
	if left == right && fastc_is_numeric_expression_type(left) {
		return left
	}
	if left == 'integer literal' && fastc_is_integer_type(right) {
		return right
	}
	if right == 'integer literal' && fastc_is_integer_type(left) {
		return left
	}
	if left == 'float literal' && right in ['f32', 'f64'] {
		return right
	}
	if right == 'float literal' && left in ['f32', 'f64'] {
		return left
	}
	return ''
}

fn fastc_is_numeric_expression_type(typ string) bool {
	return typ in ['integer literal', 'float literal', 'f32', 'f64'] || fastc_is_integer_type(typ)
}

fn fastc_is_integer_expression_type(typ string) bool {
	return typ == 'integer literal' || fastc_is_integer_type(typ)
}

fn fastc_call_types_are_compatible(actual string, expected string) bool {
	if actual == expected {
		return true
	}
	if actual == 'integer literal' {
		return fastc_is_integer_type(expected)
	}
	if actual == 'float literal' {
		return expected in ['f32', 'f64']
	}
	if actual == 'nil' {
		return expected.ends_with('*') || expected in ['voidptr', 'byteptr', 'charptr']
	}
	return false
}

fn fastc_is_integer_type(typ string) bool {
	return typ in ['byte', 'char', 'i8', 'i16', 'i32', 'i64', 'int', 'isize', 'rune', 'u8', 'u16',
		'u32', 'u64', 'unsigned int', 'usize']
}

fn fastc_nondecimal_literal_is_type_sensitive(literal string) bool {
	clean := literal.replace('_', '')
	if clean.len <= 2 || clean[0] != `0` {
		return false
	}
	digits := clean[2..].trim_left('0')
	if clean[1] in [`x`, `X`] {
		if digits.len > 8 {
			return true
		}
		return digits.len == 8 && ((digits[0] >= `8` && digits[0] <= `9`)
			|| (digits[0] >= `a` && digits[0] <= `f`)
			|| (digits[0] >= `A` && digits[0] <= `F`))
	}
	if clean[1] in [`b`, `B`] {
		return digits.len >= 32
	}
	if clean[1] in [`o`, `O`] {
		return digits.len > 11 || (digits.len == 11 && digits[0] >= `2`)
	}
	return false
}

fn fastc_decimal_literal_is_type_sensitive(literal string) bool {
	clean := literal.replace('_', '')
	if clean.len == 0 || clean.contains_any('.eE') {
		return false
	}
	for digit in clean {
		if !digit.is_digit() {
			return false
		}
	}
	digits := clean.trim_left('0')
	int_max_literal := '2147483647'
	if digits.len != int_max_literal.len {
		return digits.len > int_max_literal.len
	}
	for i in 0 .. digits.len {
		if digits[i] != int_max_literal[i] {
			return digits[i] > int_max_literal[i]
		}
	}
	return false
}

fn fastc_c_number(literal string) !string {
	clean := literal.replace('_', '')
	if fastc_decimal_literal_is_type_sensitive(literal) {
		// C assigns oversized decimal tokens a wider type before any surrounding
		// operation. Reject them until the direct parser can preserve V inference.
		return error('fastc parser does not support oversized decimal literal expressions')
	}
	if fastc_nondecimal_literal_is_type_sensitive(literal) {
		return error('fastc parser does not support high-bit nondecimal literals')
	}
	if clean.len > 2 && clean[0] == `0` && clean[1] in [`o`, `O`] {
		// V spells octal integers with an explicit 0o prefix. GNU C uses a
		// leading zero, so translate the prefix before emitting the token.
		return '0' + clean[2..]
	}
	if clean.len < 2 || clean[0] != `0` || !clean[1].is_digit() || clean.contains_any('.eE') {
		return clean
	}
	mut first_digit := 0
	for first_digit < clean.len - 1 && clean[first_digit] == `0` {
		first_digit++
	}
	return clean[first_digit..]
}

fn (g &Parser) token_source() string {
	if g.lit.len > 0 {
		return g.lit
	}
	return g.tok.str()
}

fn (mut g Parser) write_line(line string) {
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
	content := raw[1..raw.len - 1]
	if fastc_string_contains_nul(content, is_raw) {
		return error('fastc parser does not support embedded NUL string literals')
	}
	mut result := strings.new_builder(raw.len + 2)
	result.write_u8(`"`)
	mut i := 1
	for i < raw.len - 1 {
		c := raw[i]
		if c == `\\` && !is_raw && i + 1 < raw.len - 1 {
			if raw[i + 1] == `\n` {
				i += 2
				for i < raw.len - 1 && raw[i] in [` `, `\t`, `\r`] {
					i++
				}
				continue
			}
			if raw[i + 1] == `\r` && i + 2 < raw.len - 1 && raw[i + 2] == `\n` {
				i += 3
				for i < raw.len - 1 && raw[i] in [` `, `\t`] {
					i++
				}
				continue
			}
			if raw[i + 1] == `x` {
				if i + 3 >= raw.len - 1 {
					return error('invalid fastc hex escape')
				}
				high := fastc_hex_digit_value(raw[i + 2])!
				low := fastc_hex_digit_value(raw[i + 3])!
				value := (high << 4) | low
				// V consumes exactly two hexadecimal digits. C consumes every
				// following hex digit, so use a full three-digit octal escape to
				// terminate the encoded byte unambiguously.
				result.write_u8(`\\`)
				result.write_u8(`0` + (value >> 6))
				result.write_u8(`0` + ((value >> 3) & 7))
				result.write_u8(`0` + (value & 7))
				i += 4
				continue
			}
			if raw[i + 1] >= `0` && raw[i + 1] <= `7` && (i + 3 >= raw.len - 1
				|| raw[i + 2] < `0` || raw[i + 2] > `7` || raw[i + 3] < `0`
				|| raw[i + 3] > `7`) {
				// V only decodes three-digit octal escapes. Preserve a shorter
				// spelling as a literal backslash and digits instead of letting C
				// consume it as a one- or two-digit octal escape.
				result.write_string('\\\\')
				i++
				continue
			}
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

fn fastc_hex_digit_value(c u8) !u8 {
	if c >= `0` && c <= `9` {
		return u8(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return u8(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return u8(c - `A` + 10)
	}
	return error('invalid fastc hex digit `${c.ascii_str()}`')
}

fn fastc_string_contains_nul(content string, is_raw bool) bool {
	if content.bytes().contains(u8(0)) {
		return true
	}
	if is_raw {
		return false
	}
	mut i := 0
	for i + 1 < content.len {
		if content[i] != `\\` {
			i++
			continue
		}
		escape := content[i + 1]
		if escape == `\\` {
			i += 2
			continue
		}
		if escape >= `0` && escape <= `7` && i + 3 < content.len && content[i + 2] >= `0`
			&& content[i + 2] <= `7` && content[i + 3] >= `0` && content[i + 3] <= `7` {
			high := int(escape - `0`)
			middle := int(content[i + 2] - `0`)
			low := int(content[i + 3] - `0`)
			value := high * 64 + middle * 8 + low
			// V stores three-digit octal escapes in a byte, including wrapping
			// values such as \400 to NUL.
			if u8(value) == 0 {
				return true
			}
			i += 4
			continue
		}
		if escape == `0`
			|| (escape == `x` && i + 3 < content.len && content[i + 2..i + 4] == '00')
			|| (escape == `u` && i + 5 < content.len && content[i + 2..i + 6] == '0000')
			|| (escape == `U` && i + 9 < content.len && content[i + 2..i + 10] == '00000000') {
			return true
		}
		i += 2
	}
	return false
}
