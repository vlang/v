module main

import os
import log

// max_signature_lines bounds how many source lines a declaration may span before
// the writer gives up reading its signature and emits a comment-only test.
const max_signature_lines = 24

// FnParam is one parameter of a function, as the source spells it.
struct FnParam {
	name string
	typ  string
}

// FnSignature is what a generated test needs of a function: its parameters, and
// the source text of the whole declaration for the comment the author reads.
struct FnSignature {
	name        string
	params      []FnParam
	is_method   bool
	is_variadic bool
	is_generic  bool
	return_type string
	text        string
}

// test_file_for returns the `_test.v` path beside a V source file. V's test
// runner discovers a test file by that name, next to the code it tests.
fn test_file_for(source string) string {
	for suffix in ['.c.v', '.js.v', '.wasm.v', '.native.v'] {
		if source.ends_with(suffix) {
			return '${source[..source.len - suffix.len]}_test${suffix}'
		}
	}
	dot := source.last_index('.') or { return '${source}_test.v' }
	return '${source[..dot]}_test.v'
}

// zero_value_for returns the V literal spelling the zero value of `typ`, or none
// when the type is not one a generated test may invent. Only these types produce
// a call: everything else leaves a comment, because a test that does not compile
// is worse than a test that does not yet assert anything.
fn zero_value_for(typ string) ?string {
	t := typ.trim_space()
	if t in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte', 'rune', 'isize',
		'usize'] {
		return '0'
	}
	if t in ['f32', 'f64'] {
		return '0.0'
	}
	if t == 'bool' {
		return 'false'
	}
	if t == 'string' {
		return "''"
	}
	if t.starts_with('[]') {
		zero_value_for(t[2..]) or { return none }
		return '${t}{}'
	}
	if t.starts_with('map[') {
		close := t.index(']') or { return none }
		zero_value_for(t[4..close]) or { return none }
		zero_value_for(t[close + 1..]) or { return none }
		return '${t}{}'
	}
	return none
}

// split_top_level splits `text` on `sep`, but only outside any bracket, so a
// parameter type such as `map[string]int` or `fn (int) int` stays whole.
fn split_top_level(text string, sep u8) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i, c in text {
		match c {
			`(`, `[`, `{` {
				depth++
			}
			`)`, `]`, `}` {
				if depth > 0 {
					depth--
				}
			}
			else {
				if depth == 0 && c == sep {
					parts << text[start..i].trim_space()
					start = i + 1
				}
			}
		}
	}
	parts << text[start..].trim_space()
	return parts
}

// strip_line_comment removes a trailing `//` comment, so a comment containing a
// bracket cannot unbalance the signature scan.
fn strip_line_comment(line string) string {
	mut in_string := false
	mut quote := ` `
	for i, c in line {
		if in_string {
			if c == quote {
				in_string = false
			}
			continue
		}
		if c == `"` || c == `'` {
			in_string = true
			quote = c
			continue
		}
		if c == `/` && i + 1 < line.len && line[i + 1] == `/` {
			return line[..i].trim_right(' \t')
		}
	}
	return line.trim_right(' \t')
}

// fn_signature reads the parameter list and the source text of the function
// declared at `line`. A signature may span several lines, so the text is
// accumulated until the body's opening brace is found at bracket depth zero.
fn fn_signature(lines []string, line int) ?FnSignature {
	mut index := line - 1
	if index < 0 || index >= lines.len {
		return none
	}
	mut text := ''
	mut depth := 0
	mut brace_index := -1
	for offset := 0; offset < max_signature_lines; offset++ {
		if index + offset >= lines.len {
			return none
		}
		segment := strip_line_comment(lines[index + offset])
		if text.len > 0 {
			text += ' '
		}
		segment_start := text.len
		text += segment
		for i, c in segment {
			if c == `{` && depth == 0 {
				brace_index = segment_start + i
				break
			}
			match c {
				`(`, `[`, `{` {
					depth++
				}
				`)`, `]`, `}` {
					if depth > 0 {
						depth--
					}
				}
				else {}
			}
		}
		if brace_index >= 0 {
			break
		}
	}
	if brace_index < 0 {
		return none
	}
	signature := text[..brace_index].trim_space()
	fn_start := signature.index('fn ') or { return none }
	is_method := signature[fn_start + 3..].trim_left(' ').starts_with('(')
	// The first group is the parameters, except for a method's receiver. Stop at
	// its closing parenthesis so a tuple or function return type is not mistaken
	// for another parameter list.
	mut param_start := -1
	mut param_end := -1
	mut group_depth := 0
	mut groups := 0
	for i := fn_start + 3; i < signature.len; i++ {
		c := signature[i]
		match c {
			`(` {
				if group_depth == 0 && (!is_method || groups > 0) {
					param_start = i
				}
				group_depth++
			}
			`)` {
				if group_depth > 0 {
					group_depth--
					if group_depth == 0 {
						groups++
						if param_start >= 0 {
							param_end = i
							break
						}
					}
				}
			}
			else {}
		}
	}
	if param_start < 0 || param_end <= param_start {
		return none
	}
	mut params := []FnParam{}
	mut is_variadic := false
	for raw in split_top_level(signature[param_start + 1..param_end], `,`) {
		name := raw.all_before(' ').trim_space()
		typ := raw.all_after(' ').trim_space()
		if name == '' {
			continue
		}
		if typ.starts_with('...') {
			is_variadic = true
		}
		params << FnParam{
			name: name
			typ:  typ
		}
	}
	return FnSignature{
		name:        if is_method {
			''
		} else {
			signature[fn_start + 3..param_start].trim_space().all_before('[')
		}
		params:      params
		is_method:   is_method
		is_variadic: is_variadic
		is_generic:  signature[fn_start + 3..param_start].contains('[')
		return_type: signature[param_end + 1..].trim_space()
		text:        signature
	}
}

// test_body_for builds one test function. A function whose parameters all have
// zero values gets a starter call. More complex signatures get a comment only;
// the author must supply appropriate inputs and meaningful assertions.
fn test_body_for(name string, signature FnSignature, content string, existing_test string) string {
	mut call := ''
	if !signature.is_method && !signature.is_variadic && !signature.is_generic
		&& !signature.return_type.starts_with('!') && !signature.return_type.starts_with('?')
		&& !signature.return_type.starts_with('(') && !signature.return_type.starts_with('fn ') {
		mut args := []string{}
		mut all_zeroable := true
		for param in signature.params {
			if arg := call_arg_for(param) {
				args << arg
			} else {
				all_zeroable = false
				break
			}
		}
		if all_zeroable {
			spelled := call_name_for(content, name, existing_test)
			call = '${spelled}(${args.join(', ')})'
			if signature.return_type != '' {
				call = '_ = ' + call
			}
		}
	}
	mut lines := ['fn test_${name}() {']
	if call == '' {
		lines << '\t// TODO: supply arguments and handle this signature yourself.'
	} else {
		lines << '\t// TODO: replace the zero values and the placeholder assertion.'
	}
	lines << '\t// signature: ${signature.text}'
	if call != '' {
		lines << '\t' + call
	}
	lines << '\tassert true'
	lines << '}'
	return lines.join('\n')
}

// call_arg_for returns the argument to pass for `param`, or none when the
// parameter needs a value no generated test should invent.
fn call_arg_for(param FnParam) ?string {
	if param.typ.len == 0 || param.typ.contains('&') {
		return none
	}
	return zero_value_for(param.typ)
}

// existing_test_names returns the `test_` function names a test file already
// defines, read line by line rather than parsed: a test file that does not
// compile still has to be seen, or the writer would duplicate its functions.
fn existing_test_names(content string) []string {
	mut names := []string{}
	for line in content.split_into_lines() {
		trimmed := line.trim_left(' \t')
		if !trimmed.starts_with('fn test_') {
			continue
		}
		name := trimmed.all_after('fn test_').all_before('(').trim_space()
		if name.len > 0 {
			names << name
		}
	}
	return names
}

// source_module_name reads the module a source file declares.
fn source_module_name(content string) string {
	for line in content.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.starts_with('//') {
			continue
		}
		if trimmed.starts_with('module ') {
			return trimmed.all_after('module ').all_before(' ').trim_space()
		}
	}
	return 'main'
}

// call_name_for returns how a generated test must spell the call. A test file
// in the same directory as the source calls it bare; one that tests a module
// through an import has to go through that module.
fn call_name_for(source string, function_name string, test_content string) string {
	if test_content.trim_space().len == 0 {
		return function_name
	}
	module := source_module_name(source)
	if module == '' {
		return function_name
	}
	for line in test_content.split_into_lines() {
		trimmed := line.trim_space()
		if !trimmed.starts_with('import ') {
			continue
		}
		imported := trimmed.all_after('import ').all_before('{').trim_space()
		path := imported.all_before(' as ').trim_space()
		if path.all_after_last('.') == module {
			alias := if imported.contains(' as ') {
				imported.all_after(' as ').trim_space()
			} else {
				module
			}
			return '${alias}.${function_name}'
		}
	}
	return function_name
}

// write_tests_for_file writes the test functions `file` is missing, and returns
// how many it wrote. An existing test file is only ever appended to.
fn (ctx &Context) write_tests_for_file(file string, functions []FunctionCoverage) !int {
	if !ctx.within_working_folder(file) {
		return error('${file} is outside the working directory, so no test file is written beside it')
	}
	content := os.read_file(file)!
	lines := content.split_into_lines()
	test_path := test_file_for(file)
	if !ctx.within_working_folder(test_path) {
		return error('${test_path} is outside the working directory, so no test file is written')
	}
	// What is already there decides what to add, so the existing test file is
	// read first: a function the author has written a test for is left alone.
	existing_test := os.read_file(test_path) or { '' }
	mut already := existing_test_names(existing_test)
	mut blocks := []string{}
	for function in functions {
		if function.name in already {
			continue
		}
		signature := fn_signature(lines, function.line) or {
			ctx.verbose('> no signature read for ${function.name} at ${file}:${function.line}')
			continue
		}
		if signature.is_method {
			ctx.verbose('> skipping the method ${function.name}, which needs a receiver')
			continue
		}
		if signature.name != function.name {
			ctx.verbose('> skipping stale function metadata for ${function.name} at ${file}:${function.line}')
			continue
		}
		blocks << test_body_for(function.name, signature, content, existing_test)
		already << function.name
	}
	if blocks.len == 0 {
		return 0
	}
	mut test_content := ''
	trimmed_test := existing_test.trim_right(' \t\n')
	if trimmed_test.len > 0 {
		separator := if existing_test.ends_with('\n') { '\n' } else { '\n\n' }
		test_content = existing_test + separator + blocks.join('\n\n') + '\n'
	} else {
		module := source_module_name(content)
		if module == '' {
			return error('cannot read the module of ${file}, so no test file can be written')
		}
		test_content = 'module ${module}\n\n' + blocks.join('\n\n') + '\n'
	}
	if ctx.dry_run {
		println('--- ${test_path}')
		println(test_content.trim_space())
		println('')
		return blocks.len
	}
	os.write_file(test_path, test_content)!
	println('wrote ${blocks.len} test function(s) to ${test_path}')
	return blocks.len
}

// within_working_folder reports whether `file` sits below the directory the tool
// was run from. A generated test goes next to its source, so a source outside
// that directory would put a new file somewhere the caller did not look.
fn (ctx &Context) within_working_folder(file string) bool {
	if ctx.working_folder == '' {
		return false
	}
	mut folder := normalize_path(os.real_path(ctx.working_folder)).trim_right('/')
	mut target := normalize_path(os.real_path(file))
	if !os.exists(file) {
		parent := os.dir(file)
		if os.is_link(file) || !os.is_dir(parent) {
			return false
		}
		// real_path returns the original spelling when the leaf does not exist.
		// Resolve its existing parent to handle backlinks and directory symlinks.
		target = normalize_path(os.join_path(os.real_path(parent), os.file_name(file)))
	}
	$if windows {
		folder = folder.to_lower()
		target = target.to_lower()
	}
	return target.starts_with(folder + '/')
}

// write_tests_for_uncovered appends a test function for every function no run
// entered, beside its source. It never rewrites a test file and never touches a
// test function that is already there.
//
// A whole coverage directory can hold every file a run reached, so this writes
// only when the caller named what it wants with `--filter`. Writing tests for
// an unstated selection is how a test skeleton lands next to a stdlib file the
// user never asked about.
fn (mut ctx Context) write_tests_for_uncovered(filters []string) {
	if filters.len == 0 {
		log.error('--write-tests needs --filter, so that it writes only for the files you mean')
		return
	}
	mut by_file := map[string][]FunctionCoverage{}
	mut files := []string{}
	for function in ctx.reported_functions(filters) {
		if function.is_covered() {
			continue
		}
		if function.file !in by_file {
			files << function.file
		}
		by_file[function.file] << function
	}
	files.sort()
	mut written := 0
	for file in files {
		written += ctx.write_tests_for_file(file, by_file[file]) or {
			log.error('could not write tests for ${file}: ${err.msg()}')
			continue
		}
	}
	if written == 0 {
		println('no uncovered function is still missing a test function')
		return
	}
	if ctx.dry_run {
		println('${written} test function(s) would be written')
	} else {
		println('${written} test function(s) written in total')
	}
}
