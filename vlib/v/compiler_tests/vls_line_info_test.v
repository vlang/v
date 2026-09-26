// Tests for `-line-info`, the questions of the mini-VLS protocol that V1 used to
// answer for the V language server: hover (`hv^`), go-to-definition (`gd^`),
// signature help (`fn^`), completion (a bare column) and inlay hints (`ih^`).
// V3 answers them from the checked program, in V1's formats.
import os
import time
import x.json2

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v.v')
const line_info_v3_bin = os.join_path(os.temp_dir(), 'v3_line_info_test_${os.getpid()}')
const work_dir = os.join_path(os.vtmp_dir(), 'v3_line_info_test_${os.getpid()}')

const program = "module main

import os
import strings

const limit = 10

struct Point {
	x int
	y int
}

enum Color {
	red
	green = 5
	blue
}

fn (p Point) sum() int {
	return p.x + p.y
}

fn divide(a int, b int) !int {
	if b == 0 {
		return error('division by zero')
	}
	return a / b
}

fn total(base int, nums ...int) int {
	return base + nums.len
}

fn main() {
	p := Point{3, 4}
	q := Point{
		y: 1
		x: 2
	}
	c := Color.blue
	y := p.sum()
	x := y
	println(y + x)
	for i := 0; i < limit; i++ {
		println(i)
	}
	res := divide(10, 2) or {
		println(err)
		0
	}
	if v := divide(1, 0) {
		println(v)
	} else {
		println(err)
	}
	big := [p, q].filter(it.x > 1)
	println(total(1, 2, 3))
	println(os.args.len)
	mut sb := strings.new_builder(8)
	sb.write_string('\${c} \${res} \${big.len}')
	println(sb.str())
	println(Perm.read | .write)
}

@[flag]
enum Perm {
	read
	write
}
"

// A client asks for completion right after a dot with a placeholder name there,
// `p.zz`, as the code has to parse.
const completion_program = 'module main

import strings

struct Point {
	x int
mut:
	y int
}

fn (p Point) sum() int {
	return p.x + p.y
}

fn main() {
	p := Point{}
	println(p.zz)
	println(strings.zz)
	println(p.sum())
}
'

// Every kind of name a declaration introduces, for go-to-definition on the
// declaration itself: VLS asks it to learn which occurrences a rename changes.
const declarations_program = 'module main

import time

struct Base {
	id int
}

fn (b Base) ident() int {
	return b.id
}

struct Job {
	Base
	time int
	lead Base
}

interface Named {
	name() string
}

const answer = 42

fn find(n int) ?int {
	return if n > 0 { n } else { none }
}

fn shadowed() {
	time := [1, 2]
	println(time.len)
}

fn main() {
	offset := 7
	mut out := []int{}
	for item in [1, 2] {
		out << item + offset
	}
	for i, value in out {
		println(i + value)
	}
	for k := 0; k < 2; k++ {
		println(k)
	}
	if found := find(1) {
		println(found)
	}
	job := Job{
		time: 3
	}
	println(job.ident() + job.time + answer)
	println(job.id)
	println(time.now().year > 0)
	shadowed()
}
fn slices(text string) string {
	close := 3
	return text[0..close + 1] + text[close..] + text[..close]
}

fn loops(content string) int {
	mut i := 0
	mut n := 0
	for i < content.len {
		c := content[i]
		if c == `a` {
			n += 1
		}
		i++
	}
	for x in [1, 2] {
		y := x * 2
		n += y
	}
	return n
}

fn (b Base) twice[T](x T) int {
	return b.ident() * 2
}

fn (j Job) run[T](x T) int {
	return j.ident()
}
'

// Locals and a parameter named like the variables the language declares,
// `err`, `it` and `a`, in the functions where those are declared too.
const shadowing_program = "module main

fn fails() !int {
	return error('x')
}

fn demo(err string) {
	println(err)
	if value := fails() {
		println(value)
	} else {
		println(err)
	}
	n := fails() or {
		println(err)
		0
	}
	println(n)
}

fn items() {
	it := 'five'
	println([1, 2].map(it + 1))
	println(it)
	a := 'one'
	mut xs := [3, 1]
	xs.sort(a < b)
	println(a)
}

fn captured(err string) {
	n := fails() or {
		f := fn [err] () string {
			return err.msg()
		}
		println(f())
		0
	}
	println(n)
	println(err)
}

fn main() {
	demo('outer')
	items()
	captured('outer')
}
"

fn testsuite_begin() {
	res := os.execute('${os.quoted_path(vexe)} -gc none -path ${os.quoted_path('${vlib_dir}|@vlib|@vmodules')} -o ${os.quoted_path(line_info_v3_bin)} ${os.quoted_path(v3_src)}')
	assert res.exit_code == 0, res.output
	os.mkdir_all(os.join_path(work_dir, 'completion')) or { panic(err) }
	os.mkdir_all(os.join_path(work_dir, 'declarations')) or { panic(err) }
	os.mkdir_all(os.join_path(work_dir, 'shadowing')) or { panic(err) }
	os.write_file(os.join_path(work_dir, 'shadowing', 'main.v'), shadowing_program) or {
		panic(err)
	}
	os.write_file(os.join_path(work_dir, 'main.v'), program) or { panic(err) }
	os.write_file(os.join_path(work_dir, 'completion', 'main.v'), completion_program) or {
		panic(err)
	}
	os.write_file(os.join_path(work_dir, 'declarations', 'main.v'), declarations_program) or {
		panic(err)
	}
}

fn testsuite_end() {
	os.rm(line_info_v3_bin) or {}
	os.rmdir_all(work_dir) or {}
}

// ask asks `-line-info` for `main.v` of `dir` with the cursor on the second
// byte of the `nth` identifier or number `word` of `line`, or just past it for a
// one-byte one. The column VLS sends is the 0-based byte of the cursor.
fn ask(dir string, code string, line int, word string, nth int) string {
	source := os.read_file(os.join_path(dir, 'main.v')) or { panic(err) }
	text := source.split('\n')[line - 1]
	mut found := -1
	mut seen := 0
	for i := 0; i + word.len <= text.len; i++ {
		if text[i..i + word.len] == word && (i == 0 || !is_name_byte(text[i - 1]))
			&& (i + word.len == text.len || !is_name_byte(text[i + word.len])) {
			if seen == nth {
				found = i
				break
			}
			seen++
		}
	}
	assert found >= 0, '`${word}` is not on line ${line}: ${text}'
	return ask_at(dir, '${line}:${code}${found + 1}')
}

fn ask_at(dir string, spec string) string {
	res := os.execute('cd ${os.quoted_path(dir)} && ${os.quoted_path(line_info_v3_bin)} -w -check -nocolor -vls-mode -line-info "main.v:${spec}" main.v')
	assert res.exit_code == 0, res.output
	return res.output.trim_space()
}

fn is_name_byte(c u8) bool {
	return c.is_letter() || c.is_digit() || c == `_`
}

fn hover(line int, word string, nth int) string {
	answer := ask(work_dir, 'hv^', line, word, nth)
	if answer == '' {
		return ''
	}
	prefix := '{"contents":{"kind":"markdown","value":"```v\\n'
	suffix := '\\n```"}}'
	assert answer.starts_with(prefix) && answer.ends_with(suffix), answer
	return answer[prefix.len..answer.len - suffix.len]
}

fn definition(line int, word string, nth int) string {
	return ask(work_dir, 'gd^', line, word, nth)
}

fn test_hover_writes_declarations_as_v1_did() {
	assert ask(work_dir, 'hv^', 35, 'p', 0) == '{"contents":{"kind":"markdown","value":"```v\\np main.Point\\n```"}}'
	assert hover(35, 'Point', 0) == 'struct Point'
	assert hover(40, 'Color', 0) == 'enum Color'
	// An enum value, with its value: the next one, or the bit of a flag.
	assert hover(40, 'blue', 0) == 'Color.blue = 6'
	assert hover(14, 'red', 0) == 'Color.red = 0'
	assert hover(62, 'write', 0) == 'Perm.write = 0b10 (2)'
	assert hover(68, 'write', 0) == 'Perm.write = 0b10 (2)'
	assert hover(41, 'sum', 0) == 'fn sum() int'
	assert hover(44, 'limit', 0) == 'const limit int'
	assert hover(57, 'total', 0) == 'fn total(base int, nums ...int) int'
	assert hover(59, 'new_builder', 0) == 'fn new_builder(initial_size int) strings.Builder'
	assert hover(60, 'write_string', 0) == 'fn write_string(s string)'
	assert hover(58, 'args', 0) == 'const args []string'
	// A field, and a field named in a struct literal.
	assert hover(56, 'x', 0) == 'x int'
	assert hover(37, 'y', 0) == 'y int'
	// The variables the language declares: `err`, `it`, and a guard's own.
	assert hover(54, 'err', 0) == 'err IError'
	assert hover(56, 'it', 0) == 'it main.Point'
	assert hover(51, 'v', 0) == 'v int'
}

fn test_definition_prints_the_position_of_the_declaration() {
	assert definition(42, 'y', 0) == 'main.v:41:1'
	// `x := y` declares `x`: the `y` on its right is a use.
	assert definition(43, 'y', 0) == 'main.v:41:1'
	assert definition(45, 'i', 0) == 'main.v:44:5'
	assert definition(41, 'sum', 0) == 'main.v:19:13'
	assert definition(40, 'blue', 0) == 'main.v:16:1'
	assert definition(35, 'Point', 0) == 'main.v:8:7'
	assert definition(38, 'x', 0) == 'main.v:9:1'
	assert definition(44, 'limit', 0) == 'main.v:6:6'
	assert definition(52, 'v', 0) == 'main.v:51:4'
	// `err` comes from the `{` of its block, and `it` from its method.
	assert definition(48, 'err', 0) == 'main.v:47:25'
	assert definition(54, 'err', 0) == 'main.v:53:8'
	assert definition(56, 'it', 0) == 'main.v:56:15'
}

fn declaration(line int, word string, nth int) string {
	return ask(os.join_path(work_dir, 'declarations'), 'gd^', line, word, nth)
}

fn test_definition_of_a_declaration_is_the_declaration() {
	// A field, and one named like an imported module.
	assert declaration(6, 'id', 0) == 'main.v:6:1'
	assert declaration(15, 'time', 0) == 'main.v:15:1'
	assert declaration(16, 'lead', 0) == 'main.v:16:1'
	// A method's name and its receiver.
	assert declaration(9, 'ident', 0) == 'main.v:9:12'
	assert declaration(9, 'b', 0) == 'main.v:9:4'
	// A method of an interface, and a const.
	assert declaration(20, 'name', 0) == 'main.v:20:1'
	assert declaration(23, 'answer', 0) == 'main.v:23:6'
	// Locals, also one named like a function of an imported module, and one
	// named like the module itself.
	assert declaration(35, 'offset', 0) == 'main.v:35:1'
	assert declaration(36, 'out', 0) == 'main.v:36:5'
	assert declaration(30, 'time', 0) == 'main.v:30:1'
	// The variables of loops and of an `if x :=` guard.
	assert declaration(37, 'item', 0) == 'main.v:37:5'
	assert declaration(40, 'i', 0) == 'main.v:40:5'
	assert declaration(40, 'value', 0) == 'main.v:40:8'
	assert declaration(43, 'k', 0) == 'main.v:43:5'
	assert declaration(46, 'found', 0) == 'main.v:46:4'
	// What already led to a declaration still does: the uses, the type written
	// in a field, and an embedded struct, which is its type.
	assert declaration(38, 'item', 0) == 'main.v:37:5'
	assert declaration(38, 'offset', 0) == 'main.v:35:1'
	assert declaration(31, 'time', 0) == 'main.v:30:1'
	assert declaration(47, 'found', 0) == 'main.v:46:4'
	assert declaration(50, 'time', 0) == 'main.v:15:1'
	assert declaration(52, 'ident', 0) == 'main.v:9:12'
	// A field of an embedded struct, through the struct that embeds it.
	assert declaration(53, 'id', 0) == 'main.v:6:1'
	assert declaration(16, 'Base', 0) == 'main.v:5:7'
	assert declaration(14, 'Base', 0) == 'main.v:5:7'
	// A local in the bounds of a slice, `s[a..b]`, whose parser node is not the
	// parent the index of parents names.
	for nth in 0 .. 3 {
		assert declaration(59, 'close', nth) == 'main.v:58:1', 'close ${nth}'
	}
	assert ask(os.join_path(work_dir, 'declarations'), 'hv^', 59, 'close', 0).contains('close int')
	// A local that the body of a loop declares, used further on in that body.
	assert declaration(67, 'c', 0) == 'main.v:66:2'
	assert declaration(74, 'y', 0) == 'main.v:73:2'
	// A method called in the body of a generic function, which the checker does
	// not type: the method of the receiver's declared type, or of the struct
	// that type embeds.
	assert declaration(80, 'ident', 0) == 'main.v:9:12'
	assert declaration(84, 'ident', 0) == 'main.v:9:12'
}

fn shadowing(code string, line int, word string) string {
	return ask(os.join_path(work_dir, 'shadowing'), code, line, word, 0)
}

fn test_a_name_stands_for_its_nearest_declaration_implicit_ones_too() {
	// The parameter `err`, and the `err` an `else` and an `or {}` declare in
	// its function: a rename of the parameter changes the occurrences whose
	// definition is the parameter only.
	assert shadowing('gd^', 8, 'err') == 'main.v:7:8'
	assert shadowing('gd^', 12, 'err') == 'main.v:11:8'
	assert shadowing('gd^', 15, 'err') == 'main.v:14:17'
	assert shadowing('hv^', 8, 'err').contains('err string')
	assert shadowing('hv^', 12, 'err').contains('err IError')
	assert shadowing('hv^', 15, 'err').contains('err IError')
	// Locals `it` and `a`, and those `.map()` and `.sort()` declare.
	assert shadowing('gd^', 23, 'it') == 'main.v:23:16'
	assert shadowing('hv^', 23, 'it').contains('it int')
	assert shadowing('gd^', 24, 'it') == 'main.v:22:1'
	assert shadowing('hv^', 24, 'it').contains('it string')
	assert shadowing('gd^', 27, 'a') == 'main.v:27:4'
	assert shadowing('gd^', 28, 'a') == 'main.v:25:1'
	// A closure's capture list names the `err` of the `or {}` around it.
	assert shadowing('gd^', 33, 'err') == 'main.v:32:17'
	assert shadowing('gd^', 34, 'err') == 'main.v:32:17'
	assert shadowing('hv^', 34, 'err').contains('err IError')
	assert shadowing('gd^', 40, 'err') == 'main.v:31:12'
}

fn test_signature_help_marks_the_argument_under_the_cursor() {
	total_sig := '{"signatures":[{"label":"total(base int, nums ...int) int","parameters":[{"label":"base int"},{"label":"nums ...int"}]}],"activeSignature":0,'
	assert ask(work_dir, 'fn^', 57, 'total', 0) == total_sig + '"activeParameter":0}'
	assert ask(work_dir, 'fn^', 57, '2', 0) == total_sig + '"activeParameter":1}'
	assert ask(work_dir, 'fn^', 47, '2', 0) == '{"signatures":[{"label":"divide(a int, b int) !int","parameters":[{"label":"a int"},{"label":"b int"}]}],"activeSignature":0,"activeParameter":1}'
}

struct Details {
	details []Detail
}

struct Detail {
	kind        int
	label       string
	detail      string
	declaration string
}

fn completion(line int, col int) []Detail {
	answer := ask_at(os.join_path(work_dir, 'completion'), '${line}:${col}')
	return (json2.decode[Details](answer) or { panic('${err}: ${answer}') }).details
}

fn test_completion_lists_members_after_a_dot() {
	// `p.zz`, the cursor right after the dot.
	members := completion(17, 11).map('${it.kind} ${it.label} ${it.detail}')
	assert members == ['2 sum int', '5 x int', '5 y int']
	// `strings.zz`: the public functions, types and consts of the module.
	module_items := completion(18, 17)
	builder := module_items.filter(it.label == 'new_builder')
	assert builder.len == 1
	assert builder[0].kind == 3
	assert builder[0].declaration == 'fn new_builder(initial_size int) strings.Builder'
	assert module_items.any(it.label == 'Builder')
	// On the name of a called function: that function.
	called := completion(19, 3)
	assert called.len == 1 && called[0].label == 'println'
}

struct InlayHints {
	inlay_hints []InlayHint
}

struct InlayHint {
	line    int
	col     int
	label   string
	kind    int
	tooltip string
}

fn test_inlay_hints_of_the_whole_file() {
	answer := ask_at(work_dir, '1:ih^1')
	hints := (json2.decode[InlayHints](answer) or { panic('${err}: ${answer}') }).inlay_hints
	// `Lline:col kind label`, 1-based.
	got := hints.map('L${it.line + 1}:${it.col + 1} ${it.kind} ${it.label}')
	assert got == ['L6:12 1 : int', 'L14:5 1  = 0', 'L16:6 1  = 6', 'L25:16 2 message: ',
		'L35:3 1 : Point', 'L35:13 2 x: ', 'L35:16 2 y: ', 'L36:3 1 : Point', 'L38:3 2 ⚠ ',
		'L40:3 1 : Color', 'L41:3 1 : int', 'L42:3 1 : int', 'L43:10 2 s: ', 'L44:7 1 : int',
		'L45:11 2 s: ', 'L47:5 1 : int', 'L47:16 2 a: ', 'L47:20 2 b: ', 'L47:27 2  err →',
		'L48:11 2 s: ', 'L51:6 1 : int', 'L51:17 2 a: ', 'L51:20 2 b: ', 'L52:11 2 s: ',
		'L53:10 2  err →', 'L54:11 2 s: ', 'L56:5 1 : []Point', 'L56:23 2 predicate: ', 'L57:10 2 s: ',
		'L57:16 2 base: ', 'L57:19 2 nums: ', 'L58:10 2 s: ', 'L59:8 1 : strings.Builder',
		'L59:32 2 initial_size: ', 'L60:18 2 s: ', 'L61:10 2 s: ', 'L62:10 2 s: ', 'L67:6 1  = 0b01 (1)',
		'L68:7 1  = 0b10 (2)']
	warning := hints.filter(it.label == '⚠ ')
	assert warning.len == 1 && warning[0].tooltip == 'Field "x" is out of declaration order'
}

// ask_many asks several questions at once, `line:<code><column>` each: the
// answers come one per line, after the index of their question.
fn ask_many(dir string, questions []string) []string {
	spec := questions.map('main.v:${it}').join('\t')
	res := os.execute('cd ${os.quoted_path(dir)} && ${os.quoted_path(line_info_v3_bin)} -w -check -nocolor -vls-mode -line-info "${spec}" main.v')
	assert res.exit_code == 0, res.output
	return res.output.trim_right('\n').split('\n')
}

fn test_several_questions_get_an_answer_each() {
	// A hover, a definition, and a column past the end of its line.
	assert ask_many(work_dir, ['41:hv^9', '42:gd^7', '43:hv^40']) == [
		'0\t{"contents":{"kind":"markdown","value":"```v\\nfn sum() int\\n```"}}',
		'1\tmain.v:41:1',
		'2\t',
	]
}

fn test_a_column_past_the_end_of_its_line_has_no_answer() {
	// Line 43 is `\tprintln(y + x)`: 15 bytes.
	assert ask_at(work_dir, '43:hv^14') != ''
	assert ask_at(work_dir, '43:hv^40') == ''
}

// read_until collects what the server prints until `marker`, or gives up after
// a while: stdout_read does not wait for output.
fn read_until(mut p os.Process, marker string) string {
	mut out := ''
	for _ in 0 .. 60000 {
		if out.contains(marker) || !p.is_alive() {
			break
		}
		chunk := p.stdout_read()
		if chunk == '' {
			time.sleep(time.millisecond)
			continue
		}
		out += chunk
	}
	return out
}

fn test_the_diagnostics_server_answers_queries_between_checks() {
	$if !linux {
		return
	}
	mut p := os.new_process(line_info_v3_bin)
	p.set_args(['-no-memory-limit', '-w', '-check', '-nocolor', '.'])
	p.set_work_folder(work_dir)
	mut env := os.environ()
	env['V_DIAGNOSTICS_SERVER'] = '1'
	p.set_environment(env)
	p.set_redirect_stdio()
	p.run()
	defer {
		p.close()
	}
	assert read_until(mut p, 'v-diagnostics-server: ready').contains('v-diagnostics-server: ready')
	p.stdin_write('query t1 main.v:41:hv^2\n')
	assert read_until(mut p, 'v-diagnostics-server: end 0 t1').contains('"value":"```v\\ny int\\n```"')
	p.stdin_write('check t2\n')
	checked := read_until(mut p, 'v-diagnostics-server: end ')
	assert checked.contains('v-diagnostics-server: end 0 t2'), checked
	// The files of `.` as V1 wrote them.
	p.stdin_write('query t3 main.v:42:gd^7\n')
	assert read_until(mut p, 'v-diagnostics-server: end 0 t3').contains('./main.v:41:1')
	// Several questions in one query: an answer each, after its index.
	p.stdin_write('query t5 main.v:41:hv^9\tmain.v:42:gd^7\n')
	several := read_until(mut p, 'v-diagnostics-server: end 0 t5')
	assert several.contains('0\t{"contents":{"kind":"markdown","value":"```v\\nfn sum() int\\n```"}}\n1\t./main.v:41:1\n'), several
	p.stdin_write('query t4\n')
	assert read_until(mut p, 'v-diagnostics-server: end 2').contains('unknown request `query t4`')
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

// start_server starts a diagnostics server that checks `dir`, as VLS starts the
// one it asks its questions, with the environment variables `vars` besides.
fn start_server(dir string, vars map[string]string) &os.Process {
	return start_server_with(dir, [], vars)
}

// start_server_with is start_server with the options `options` besides.
fn start_server_with(dir string, options []string, vars map[string]string) &os.Process {
	mut p := os.new_process(line_info_v3_bin)
	mut args := ['-no-memory-limit', '-w', '-check', '-nocolor']
	args << options
	args << '.'
	p.set_args(args)
	p.set_work_folder(dir)
	mut env := os.environ()
	env['V_DIAGNOSTICS_SERVER'] = '1'
	for name, value in vars {
		env[name] = value
	}
	p.set_environment(env)
	p.set_redirect_stdio()
	p.run()
	assert read_until(mut p, 'v-diagnostics-server: ready').contains('v-diagnostics-server: ready')
	return p
}

// query asks the server the questions of `spec`, and returns the pid of the
// child that answered and what it answered.
fn query(mut p os.Process, token string, spec string) (int, string) {
	p.stdin_write('query ${token} ${spec}\n')
	end := 'v-diagnostics-server: end 0 ${token}'
	out := read_until(mut p, end)
	assert out.contains(end), out
	// The child may print before the line that names it.
	marker := 'v-diagnostics-server: child '
	start := out.index(marker) or { panic(out) }
	child_line := out[start..].all_before('\n')
	assert child_line.ends_with(' ${token}'), out
	answer := out.replace_once(child_line + '\n', '').all_before(end).trim_space()
	return child_line.all_after(marker).all_before(' ').int(), answer
}

// ask_once answers `questions` about main.v of `dir` in a compiler process of its
// own that checks `.`, as the server does.
fn ask_once(dir string, questions []string) []string {
	return ask_once_with(dir, '', questions)
}

// ask_once_with is ask_once with the options `options` besides.
fn ask_once_with(dir string, options string, questions []string) []string {
	spec := questions.map('main.v:${it}').join('\t')
	res := os.execute('cd ${os.quoted_path(dir)} && ${os.quoted_path(line_info_v3_bin)} -w -check -nocolor ${options} -vls-mode -line-info "${spec}" .')
	assert res.exit_code == 0, res.output
	return res.output.trim_right('\n').split('\n').map(it.all_after('\t'))
}

fn test_a_server_child_answers_again_while_the_files_stay_the_same() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'again')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'main.v'), program)!
	// A hover, a definition, signature help, completion, inlay hints, and a
	// definition in another function.
	questions := ['41:hv^2', '42:gd^7', '47:fn^16', '60:4', '1:ih^0', '47:hv^9', '57:gd^10']
	expected := ask_once(dir, questions)
	mut p := start_server(dir, {})
	defer {
		p.close()
	}
	first, _ := query(mut p, 'a', 'main.v:41:hv^2')
	// The child that checked the program answers the next questions from it,
	// as a new check would.
	for i, question in questions {
		child, answer := query(mut p, 'q${i}', 'main.v:${question}')
		assert child == first, question
		assert answer == expected[i], question
	}
	child, several := query(mut p, 'all', questions.map('main.v:${it}').join('\t'))
	assert child == first
	assert several.split('\n').map(it.all_after('\t')) == expected
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

fn test_a_server_child_answers_no_more_once_a_file_changes() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'changes')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'main.v'), program)!
	mut p := start_server(dir, {})
	defer {
		p.close()
	}
	first, definition := query(mut p, 'a', 'main.v:42:gd^7')
	assert definition == './main.v:41:1'
	// Another content, as a client writes between two questions: a new child
	// checks it.
	os.write_file(os.join_path(dir, 'main.v'), '// One line more.\n' + program)!
	changed, moved := query(mut p, 'b', 'main.v:43:gd^7')
	assert changed != first
	assert moved == './main.v:42:1'
	// A file added next to it changes the program too.
	os.write_file(os.join_path(dir, 'extra.v'), 'module main\n\nfn extra() int {\n\treturn 1\n}\n')!
	added, _ := query(mut p, 'c', 'main.v:43:gd^7')
	assert added != changed
	// A check in between, in a child of its own, leaves the one that answers.
	p.stdin_write('check d\n')
	assert read_until(mut p, 'v-diagnostics-server: end ').contains('v-diagnostics-server: end 0 d')
	kept, _ := query(mut p, 'e', 'main.v:43:gd^7')
	assert kept == added
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
	// No child outlives the server.
	for _ in 0 .. 200 {
		if !os.exists('/proc/${kept}') {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert !os.exists('/proc/${kept}')
}

fn test_a_server_child_answers_no_more_once_an_import_resolves_elsewhere() {
	$if !linux {
		return
	}
	// `helper` is only in the second of two roots of modules, and nothing of
	// the first one is read.
	dir := os.join_path(work_dir, 'search_roots')
	first_root := os.join_path(dir, 'first')
	second_root := os.join_path(dir, 'second')
	project := os.join_path(dir, 'project')
	os.mkdir_all(first_root)!
	os.mkdir_all(os.join_path(second_root, 'helper'))!
	os.mkdir_all(project)!
	os.write_file(os.join_path(second_root, 'helper', 'helper.v'), 'module helper\n\npub fn answer() int {\n\treturn 1\n}\n')!
	os.write_file(os.join_path(project, 'main.v'), 'module main\n\nimport helper\n\nfn main() {\n\tprintln(helper.answer())\n}\n')!
	roots := '${first_root}|${second_root}|@vlib'
	mut p := start_server_with(project, ['-path', roots], {})
	defer {
		p.close()
	}
	first, before := query(mut p, 'a', 'main.v:6:gd^17')
	assert before == os.join_path(second_root, 'helper', 'helper.v') + ':3:7'
	// A module of that name added to the first root: the import is that one now.
	os.mkdir_all(os.join_path(first_root, 'helper'))!
	os.write_file(os.join_path(first_root, 'helper', 'helper.v'), 'module helper\n\n// Another one.\npub fn answer() int {\n\treturn 2\n}\n')!
	child, after := query(mut p, 'b', 'main.v:6:gd^17')
	assert after == ask_once_with(project, '-path ${os.quoted_path(roots)}', ['6:gd^17'])[0]
	assert after == os.join_path(first_root, 'helper', 'helper.v') + ':4:7'
	assert child != first
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

fn test_a_server_child_answers_no_more_once_a_module_appears_at_the_project_root() {
	$if !linux {
		return
	}
	// A program below the v.mod root imports `hash`, a module of vlib, and
	// nothing of the root itself is read.
	root := os.join_path(work_dir, 'project_root')
	app := os.join_path(root, 'cmd', 'app')
	os.mkdir_all(app)!
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'proj'\n}\n")!
	os.write_file(os.join_path(app, 'main.v'), "module main\n\nimport hash\n\nfn main() {\n\tprintln(hash.sum64_string('a', 0))\n}\n")!
	mut p := start_server(app, {})
	defer {
		p.close()
	}
	first, before := query(mut p, 'a', 'main.v:6:gd^15')
	assert before.contains(os.join_path('vlib', 'hash')), before
	// A module of that name at the root: the project's own comes before vlib's.
	os.mkdir_all(os.join_path(root, 'hash'))!
	os.write_file(os.join_path(root, 'hash', 'hash.v'), "module hash\n\n// The project's own.\npub fn sum64_string(s string, seed u64) u64 {\n\treturn 0\n}\n")!
	child, after := query(mut p, 'b', 'main.v:6:gd^15')
	assert after == ask_once(app, ['6:gd^15'])[0]
	assert after == os.join_path(root, 'hash', 'hash.v') + ':4:7'
	assert child != first
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

fn test_a_server_child_that_grew_answers_its_last_question_and_leaves() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'retire')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'main.v'), program)!
	// A child may grow by nothing after its first answer.
	mut p := start_server(dir, {
		'V_DIAGNOSTICS_RETIRE_MB': '0'
	})
	defer {
		p.close()
	}
	first, _ := query(mut p, 'a', 'main.v:42:gd^7')
	// Its next answer, complete, is its last one.
	last, definition := query(mut p, 'b', 'main.v:42:gd^7')
	assert last == first
	assert definition == './main.v:41:1'
	next, answer := query(mut p, 'c', 'main.v:42:gd^7')
	assert next != first
	assert answer == './main.v:41:1'
	assert !os.exists('/proc/${first}')
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

// Functions and methods named where they are declared and where they are
// called, by name or through a value that holds one.
const functions_program = "module main

struct User {
	name string
}

struct Box[T] {
	item T
}

fn (b Box[T]) label() string {
	return b.item.name
}

fn User.new(name string) User {
	return User{
		name: name
	}
}

fn (u User) greet() string {
	return u.name
}

fn longest[T](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn apply(f fn (int) int, x int) int {
	return f(x)
}

fn lengths[T](xs []T) []int {
	count := fn (x T) int {
		return x.name.len
	}
	println(xs.map(count))
	return xs.map(count)
}

fn shouted[T](mut xs []T) []string {
	println(xs.filter(it.name.len > 1))
	xs.sort(a.name < b.name)
	return xs.map(|x| x.name.to_upper())
}

fn tag[T](b Box[T]) string {
	return b.label()
}

fn main() {
	nums := [1, 2]
	println(nums.map(it * 2))
	println(nums.filter(it > 1))
	println([3, 4].map(it + 1))
	mut sorted := nums.clone()
	sorted.sort(a < b)
	double := fn (x int) int {
		return x * 2
	}
	println(double(4))
	println(apply(double, 5))
	u := User.new('eva')
	g := u.greet
	println(g())
	f := longest[User]
	println(f(u, User{ name: 'bo' }).name)
	println(longest[User](u, u).name)
	println(tag(Box[User]{ item: u }))
	mut people := [u]
	println(lengths(people))
	println(shouted(mut people))
}
"

// The locals of a generic body, which the checker does not type: each one of
// the ways a local is declared.
const locals_program = "module main

struct User {
	name string
}

fn pair[T](x T) (T, int) {
	return x, 1
}

fn locals[T](x T, xs []T, m map[string]T) int {
	lengths := xs.map(1)
	mut total := 0
	for n in lengths {
		total += n
	}
	same := x
	first := xs[0]
	one, two := x, 2
	word, size := pair(x)
	for i, item in xs {
		println(i)
		println(item)
	}
	for key, value in m {
		println(key)
		println(value)
	}
	for c in 'abc' {
		println(c)
	}
	for k in 0 .. 3 {
		println(k)
	}
	println(same)
	println(first)
	println(one)
	println(two)
	println(word)
	println(size)
	return total
}

fn main() {
	u := User{
		name: 'eva'
	}
	println(locals(u, [u], {
		'a': u
	}))
}
"

fn hover_of(text string) string {
	return '{"contents":{"kind":"markdown","value":"```v\\n${text}\\n```"}}'
}

// program_dir writes `source` as the main.v of the directory `name` of the
// work directory, once, and returns that directory.
fn program_dir(name string, source string) string {
	dir := os.join_path(work_dir, name)
	if !os.exists(os.join_path(dir, 'main.v')) {
		os.mkdir_all(dir) or { panic(err) }
		os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	}
	return dir
}

// line_of is the 1-based number of the line of `source` that reads `text`.
fn line_of(source string, text string) int {
	line := source.split('\n').index(text) + 1
	assert line > 0, '`${text}` is not a line'
	return line
}

// function asks about the `nth` `word` of the line of functions_program that
// reads `text`.
fn function(code string, text string, word string, nth int) string {
	return ask(program_dir('functions', functions_program), code, line_of(functions_program,
		text), word, nth)
}

fn test_a_member_of_a_generic_value_is_the_member_of_its_type() {
	// `xs.map()` over `xs []T` in a generic body calls the `map` of every array:
	// the declaration and the signature of `nums.map()` over an `[]int`.
	map_decl := function('gd^', '\tprintln(nums.map(it * 2))', 'map', 0)
	assert map_decl.contains('builtin/array.v:'), map_decl
	assert function('gd^', '\tprintln(xs.map(count))', 'map', 0) == map_decl
	assert function('gd^', '\treturn xs.map(|x| x.name.to_upper())', 'map', 0) == map_decl
	// Over an array literal, whose elements the checker keeps no type for.
	assert function('gd^', '\tprintln([3, 4].map(it + 1))', 'map', 0) == map_decl
	assert function('hv^', '\tprintln([3, 4].map(it + 1))', 'it', 0) == hover_of('it int')
	map_hover := function('hv^', '\tprintln(nums.map(it * 2))', 'map', 0)
	assert map_hover.contains('fn map('), map_hover
	assert function('hv^', '\tprintln(xs.map(count))', 'map', 0) == map_hover
	filter_decl := function('gd^', '\tprintln(nums.filter(it > 1))', 'filter', 0)
	assert filter_decl.contains('builtin/array.v:'), filter_decl
	assert function('gd^', '\tprintln(xs.filter(it.name.len > 1))', 'filter', 0) == filter_decl
	sort_decl := function('gd^', '\tsorted.sort(a < b)', 'sort', 0)
	assert sort_decl.contains('builtin/array.v:'), sort_decl
	assert function('gd^', '\txs.sort(a.name < b.name)', 'sort', 0) == sort_decl
	// A method of a generic struct, called on a `Box[T]`.
	label := line_of(functions_program, 'fn (b Box[T]) label() string {')
	assert function('gd^', '\treturn b.label()', 'label', 0) == 'main.v:${label}:14'
	assert function('hv^', '\treturn b.label()', 'label', 0) == hover_of('fn label() string')
}

fn test_the_callee_of_a_call_through_a_value_is_that_value() {
	// A local that holds a function literal, a method value, an instance of a
	// generic function and a parameter of a function type are described where
	// they are called as where they are declared.
	assert function('hv^', '\tprintln(double(4))', 'double', 0) == hover_of('double fn (int) int')
	assert function('hv^', '\tprintln(g())', 'g', 0) == hover_of('g fn () string')
	instance := function('hv^', '\tf := longest[User]', 'f', 0)
	assert instance.contains('f fn ('), instance
	assert function('hv^', "\tprintln(f(u, User{ name: 'bo' }).name)", 'f', 0) == instance
	assert function('hv^', '\treturn f(x)', 'f', 0) == hover_of('f fn (int) int')
	// A generic function called with its type arguments is that function.
	longest := line_of(functions_program, 'fn longest[T](a T, b T) T {')
	assert function('hv^', '\tprintln(longest[User](u, u).name)', 'longest', 0) == hover_of('fn longest(a T, b T) T')
	assert function('gd^', '\tprintln(longest[User](u, u).name)', 'longest', 0) == 'main.v:${longest}:3'
	// A function literal in a generic body takes a `T`, written by its name.
	assert function('hv^', '\tprintln(xs.map(count))', 'count', 0) == hover_of('count fn (T) int')
	assert function('hv^', '\tcount := fn (x T) int {', 'count', 0) == hover_of('count fn (T) int')
}

fn test_a_static_method_is_declared_by_its_name() {
	new := line_of(functions_program, 'fn User.new(name string) User {')
	assert function('gd^', 'fn User.new(name string) User {', 'new', 0) == 'main.v:${new}:8'
	assert function('gd^', "\tu := User.new('eva')", 'new', 0) == 'main.v:${new}:8'
}

fn test_a_chain_of_array_methods_in_a_generic_body_keeps_its_array() {
	// In a body the checker does not type, `xs.filter()` gives a `[]T`, whose
	// `map` is that of every array.
	dir := program_dir('chains', 'module main

fn plain[T](xs []T) int {
	kept := xs.filter(true).map(it)
	println(kept)
	return kept.len
}

fn main() {
	nums := [1, 2]
	println(nums.map(it * 2))
	println(plain(nums))
}
')
	map_decl := ask(dir, 'gd^', 11, 'map', 0)
	assert map_decl.contains('builtin/array.v:'), map_decl
	assert ask(dir, 'gd^', 4, 'map', 0) == map_decl
}

fn test_a_local_of_a_generic_body_has_the_type_of_its_value() {
	// The type its value has, with a type parameter by its name; a variable of
	// a `for ... in` loop has what its container holds.
	dir := program_dir('locals', locals_program)
	for text, wants in {
		'\tlengths := xs.map(1)':  ['lengths []int']
		'\tfor n in lengths {':    ['n int', 'lengths []int']
		'\t\ttotal += n':          ['total int']
		'\tsame := x':             ['same T']
		'\tfirst := xs[0]':        ['first T']
		'\tone, two := x, 2':      ['one T', 'two int']
		'\tword, size := pair(x)': ['word T', 'size int']
		'\tfor i, item in xs {':   ['i int', 'item T']
		'\tfor key, value in m {': ['key string', 'value T']
		"\tfor c in 'abc' {":      ['c u8']
		'\tfor k in 0 .. 3 {':     ['k int']
	} {
		for want in wants {
			name := want.all_before(' ')
			got := ask(dir, 'hv^', line_of(locals_program, text), name, 0)
			assert got == hover_of(want), '${text}: ${got}'
		}
	}
}

fn test_a_local_of_no_known_type_has_no_hover() {
	// The value of a call of a function that does not exist yet, as one is being
	// written: the checker gives it no type, which is no answer, not `()`.
	dir := program_dir('unknown_local', 'module main

fn main() {
	value := missing_function(1)
	println(value)
}
')
	assert ask(dir, 'hv^', 4, 'value', 0) == ''
	assert ask(dir, 'hv^', 5, 'value', 0) == ''
	// Where it is declared still is.
	assert ask(dir, 'gd^', 5, 'value', 0) == 'main.v:4:1'
}

// not_array_methods_program calls methods of a struct named like the array
// methods whose argument declares `it`, `a` and `b`, with locals of those names.
const not_array_methods_program = 'module main

struct Sorter {}

fn (s Sorter) sort(value int) int {
	return value
}

fn (s Sorter) map(value int) int {
	return value * 10
}

fn firsts[T](xs []T) []T {
	mut ys := xs.clone()
	ys.sort(a == b)
	return xs.filter(it == ys[0])
}

fn main() {
	a := 7
	s := Sorter{}
	println(s.sort(a))
	it := 3
	println(s.map(it))
	nums := [3, 1, 2]
	mut sorted := nums.clone()
	sorted.sort(a < b)
	println(sorted)
	println(nums.map(it * 2))
}
'

fn test_a_method_named_like_an_array_one_declares_no_variable() {
	// `s.sort(a)` of a user's `fn (s Sorter) sort(value int)` passes the local
	// `a`, no comparator, and `s.map(it)` the local `it`.
	dir := program_dir('not_array_methods', not_array_methods_program)
	src := not_array_methods_program
	a := line_of(src, '\ta := 7')
	it := line_of(src, '\tit := 3')
	assert ask(dir, 'gd^', line_of(src, '\tprintln(s.sort(a))'), 'a', 0) == 'main.v:${a}:1'
	assert ask(dir, 'gd^', line_of(src, '\tprintln(s.map(it))'), 'it', 0) == 'main.v:${it}:1'
	// The methods of an array declare them, over an `[]int` and over the `[]T`
	// of a generic body, and over a value there that nothing types.
	for text, word in {
		'\tsorted.sort(a < b)':            'a'
		'\tprintln(nums.map(it * 2))':     'it'
		'\tys.sort(a == b)':               'a'
		'\treturn xs.filter(it == ys[0])': 'it'
	} {
		line := line_of(src, text)
		method := if word == 'it' { text.all_after('.').all_before('(') } else { 'sort' }
		// The 0-based column of the method's name, after its dot.
		col := text.index('.${method}(') or { -1 } + 1
		assert ask(dir, 'gd^', line, word, 0) == 'main.v:${line}:${col}', text
	}
}

fn test_a_warm_child_reads_the_directory_the_client_writes_again() {
	$if !linux {
		return
	}
	// The client may remove the input directory and write it again with the
	// same files: a child that stays warm answers questions about a relative
	// path from the new one, as a new child does.
	dir := os.join_path(work_dir, 'written_again')
	os.mkdir_all(dir)!
	source := 'module main\n\nfn main() {\n\tanswer := 42\n\tprintln(answer)\n}\n'
	os.write_file(os.join_path(dir, 'main.v'), source)!
	expected := ask_once(dir, ['5:hv^10'])[0]
	assert expected.contains('answer int'), expected
	mut p := start_server(dir, {})
	defer {
		p.close()
	}
	first, answer := query(mut p, 'a', 'main.v:5:hv^10')
	assert answer == expected
	os.rmdir_all(dir)!
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'main.v'), source)!
	child, again := query(mut p, 'b', 'main.v:5:hv^10')
	assert again == expected
	// Same files: the child that checked them still answers.
	assert child == first
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}
