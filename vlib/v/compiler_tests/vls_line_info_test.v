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
	mut p := os.new_process(line_info_v3_bin)
	p.set_args(['-no-memory-limit', '-w', '-check', '-nocolor', '.'])
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
	spec := questions.map('main.v:${it}').join('\t')
	res := os.execute('cd ${os.quoted_path(dir)} && ${os.quoted_path(line_info_v3_bin)} -w -check -nocolor -vls-mode -line-info "${spec}" .')
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
