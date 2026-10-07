// Tests for `-line-info`, the questions of the mini-VLS protocol that V1 used to
// answer for the V language server: hover (`hv^`), go-to-definition (`gd^`),
// signature help (`fn^`), completion (a bare column) and inlay hints (`ih^`).
// V3 answers them from the checked program, in V1's formats.
import os
import v.cmdexec
import time
import x.json2
import v.compiler_tests.method_form

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

// Generic functions whose type parameters name a constraint: an interface, or
// a sum type, for its variants. A client asks after a dot with a placeholder
// name, `a.zz`.
const constraints_program = "module main

interface Named {
	name string
	greet() string
}

struct User {
	name string
	age  int
}

fn (u User) greet() string {
	return u.name
}

type Number = int | f64

fn longest[T Named](a T, b T) T {
	if a.name.len >= b.name.len {
		return a
	}
	println(a.zz)
	return b
}

fn describe[T Number](x T) string {
	println(x.zz)
	return x.str()
}

fn main() {
	println(longest(User{ name: 'a' }, User{ name: 'bb' }).age)
	println(describe(1))
}

fn half[T Number](x T) T {
	\$if T is f64 {
		return x / 2.0
	} \$else {
		return x / 2
	}
}

struct Box[T Named] {
	item T
}

fn (b Box[T]) label() string {
	return b.item.name + b.item.zz
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

// The implicit variables of array methods and the parameters of lambdas, in a
// constrained body and outside one.
const closures_program = "module main

interface Named {
	name string
	greet() string
}

struct User {
	name string
	age  int
}

fn (u User) greet() string {
	return u.name
}

fn names[T Named](mut xs []T) []string {
	println(xs.map(it.zz))
	xs.sort(a.name < b.zz)
	println(xs.filter(it.name.len > 0))
	return xs.map(|x| x.zz)
}

fn main() {
	users := [User{ name: 'a' }]
	println(users.map(|u| u.name))
	mut people := users.clone()
	println(names(mut people))
}

fn fails() !int {
	return error('x')
}

fn guarded[T Named](x T) int {
	return fails() or {
		println(err.zz)
		x.name.len
	}
}
"

// Functions and methods named where they are declared and where they are
// called, by name or through a value that holds one.
const functions_program = "module main

interface Named {
	name string
}

struct User {
	name string
}

struct Box[T Named] {
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

fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn apply(f fn (int) int, x int) int {
	return f(x)
}

fn lengths[T Named](xs []T) []int {
	count := fn (x T) int {
		return x.name.len
	}
	println(xs.map(count))
	return xs.map(count)
}

fn shouted[T Named](mut xs []T) []string {
	println(xs.filter(it.name.len > 1))
	xs.sort(a.name < b.name)
	return xs.map(|x| x.name.to_upper())
}

fn tag[T Named](b Box[T]) string {
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

interface Named {
	name string
}

struct User {
	name string
}

fn pair[T Named](x T) (string, int) {
	return x.name, x.name.len
}

fn locals[T Named](x T, xs []T, m map[string]T) int {
	lengths := xs.map(it.name.len)
	mut total := 0
	for n in lengths {
		total += n
	}
	label := x.name
	same := x
	first := xs[0]
	one, two := x.name, 2
	word, size := pair(x)
	for i, item in xs {
		println('\${i} \${item.name}')
	}
	for key, value in m {
		println('\${key} \${value.name}')
	}
	for c in label {
		println(c)
	}
	for k in 0 .. 3 {
		println(k)
	}
	println('\${same.name} \${first.name} \${one} \${two} \${word} \${size}')
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

fn testsuite_begin() {
	res := os.exec([vexe, '-gc', 'none', '-prealloc', '-path', '${'${vlib_dir}|@vlib|@vmodules'}',
		'-o', line_info_v3_bin, '${v3_src}'])
	assert res.exit_code == 0, res.output
	os.mkdir_all(os.join_path(work_dir, 'completion')) or { panic(err) }
	os.mkdir_all(os.join_path(work_dir, 'declarations')) or { panic(err) }
	os.mkdir_all(os.join_path(work_dir, 'shadowing')) or { panic(err) }
	os.write_file(os.join_path(work_dir, 'shadowing', 'main.v'), shadowing_program) or {
		panic(err)
	}
	os.mkdir_all(os.join_path(work_dir, 'constraints')) or { panic(err) }
	os.mkdir_all(os.join_path(work_dir, 'closures')) or { panic(err) }
	os.write_file(os.join_path(work_dir, 'closures', 'main.v'), closures_program) or {
		panic(err)
	}
	os.mkdir_all(os.join_path(work_dir, 'functions')) or { panic(err) }
	os.mkdir_all(os.join_path(work_dir, 'locals')) or { panic(err) }
	os.write_file(os.join_path(work_dir, 'locals', 'main.v'), locals_program) or { panic(err) }
	os.write_file(os.join_path(work_dir, 'functions', 'main.v'), functions_program) or {
		panic(err)
	}
	os.write_file(os.join_path(work_dir, 'main.v'), program) or { panic(err) }
	os.write_file(os.join_path(work_dir, 'completion', 'main.v'), completion_program) or {
		panic(err)
	}
	os.write_file(os.join_path(work_dir, 'declarations', 'main.v'), declarations_program) or {
		panic(err)
	}
	os.write_file(os.join_path(work_dir, 'constraints', 'main.v'), constraints_program) or {
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

// ask_at asks `-line-info` about `spec` of main.v of `dir`, and asks its method
// form the same (see same_answer_in_method_form).
fn ask_at(dir string, spec string) string {
	answer := ask_at_only(dir, spec)
	same_answer_in_method_form(dir, spec, answer)
	return answer
}

fn ask_at_only(dir string, spec string) string {
	res := cmdexec.run_in(line_info_v3_bin, ['-w', '-check', '-nocolor', '-vls-mode', '-line-info',
		'main.v:' + '${spec}', 'main.v'], dir)
	assert res.exit_code == 0, res.output
	return res.output.trim_space()
}

// same_answer_in_method_form asks the question `spec` of the method form of the
// program of `dir` too, when that program declares a generic function with a
// constraint: its generic functions are methods of `Host`, with the same type
// parameters (see method_form), and a method's own type parameters have to
// behave as those of a function, in the editor too. The method form answers
// what `dir` answers, where it moves it.
fn same_answer_in_method_form(dir string, spec string, answer string) {
	source := os.read_file(os.join_path(dir, 'main.v')) or { return }
	if !declares_a_constrained_generic_fn(source) {
		return
	}
	form := method_form.of(source) or { return }
	twin := dir + '_methods'
	twin_file := os.join_path(twin, 'main.v')
	if (os.read_file(twin_file) or { '' }) != form.source {
		os.mkdir_all(twin) or { panic(err) }
		os.write_file(twin_file, form.source) or { panic(err) }
	}
	// Several questions at once are separated by tabs, and name their file from
	// the second one on.
	moved_spec := spec.split('\t').map(moved_question(form, it)).join('\t')
	method_answer := without_host(ask_at_only(twin, moved_spec))
	expected := positions_in_method_form(form, answer)
	assert method_answer == expected, 'the method form of `${os.file_name(dir)}` answers `${spec}` (`${moved_spec}` there) with\n${method_answer}\nnot\n${expected}'
}

// moved_question is `question`, `line:code col` of main.v, where the method form
// moves it. The column of a question is the 0-based byte of the cursor.
fn moved_question(form method_form.MethodForm, question string) string {
	file := if question.starts_with('main.v:') { 'main.v:' } else { '' }
	line := question[file.len..].all_before(':').int()
	rest := question[file.len..].all_after(':')
	mut code_len := 0
	for code_len < rest.len && !rest[code_len].is_digit() {
		code_len++
	}
	return '${file}${line}:${rest[..code_len]}${form.col(line, rest[code_len..].int() + 1) - 1}'
}

// declares_a_constrained_generic_fn reports whether `source` declares a generic
// function, not a method, with a constraint on a type parameter: `[T Named]`.
fn declares_a_constrained_generic_fn(source string) bool {
	for line in source.split('\n') {
		rest := if line.starts_with('pub fn ') {
			line['pub fn '.len..]
		} else if line.starts_with('fn ') && !line.starts_with('fn (') {
			line['fn '.len..]
		} else {
			continue
		}
		if !rest.contains('[') || rest.index_u8(`[`) > rest.index_u8(`(`) {
			continue
		}
		params := rest.all_after('[').all_before(']')
		if params.split(',').any(it.trim_space().contains(' ')) {
			return true
		}
	}
	return false
}

// positions_in_method_form is `answer`, of the program, with the positions of
// main.v it names where the method form moves them: `main.v:19:5`, a 0-based
// column.
fn positions_in_method_form(form method_form.MethodForm, answer string) string {
	return answer.split('\n').map(position_in_method_form(form, it)).join('\n')
}

// position_in_method_form is positions_in_method_form for one line of an answer:
// the answer to one of several questions comes after its number and a tab.
fn position_in_method_form(form method_form.MethodForm, text string) string {
	number := text.all_before('\t')
	head := if text.contains('\t') && number.len > 0 && number.bytes().all(it.is_digit()) {
		number + '\t'
	} else {
		''
	}
	answer := text[head.len..]
	for prefix in ['./main.v:', 'main.v:'] {
		if answer.starts_with(prefix) {
			parts := answer[prefix.len..].split(':')
			if parts.len == 2 && parts.all(it.len > 0 && it.bytes().all(it.is_digit())) {
				line := parts[0].int()
				return '${head}${prefix}${line}:${form.col(line, parts[1].int() + 1) - 1}'
			}
		}
	}
	return text
}

// without_host is an answer of the method form as the program would give it:
// without the receiver `Host` that the method form gives each generic function.
fn without_host(answer string) string {
	return answer.replace('(host_ Host) ', '').replace('(host_ main.Host) ', '')
}

// ask_in asks `-line-info` about `spec`, a file of the program of `dir` with a
// position (`models/models.v:10:10`), checking the program from its main.v.
fn ask_in(dir string, spec string) string {
	res := cmdexec.run_in(line_info_v3_bin, ['-w', '-check', '-nocolor', '-vls-mode', '-line-info',
		'${spec}', 'main.v'], dir)
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

fn test_completion_leaves_out_the_fields_the_module_cannot_use() {
	// `models.User` has a private field: `main` cannot use it, but through an
	// alias `main` declares it can, as the checker says; `models` uses all.
	dir := os.join_path(work_dir, 'private_fields')
	os.mkdir_all(os.join_path(dir, 'models')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main

import models

type Account = models.User

fn main() {
	u := models.User{}
	println(u.zz)
	a := Account{}
	println(a.zz)
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'models', 'models.v'), 'module models

pub struct User {
	hidden int
pub:
	visible int
}

pub fn (u User) peek() int {
	return u.zz
}
') or { panic(err) }
	for spec, want in {
		'main.v:9:11':           ['2 peek int', '5 visible int']
		'main.v:11:11':          ['5 hidden int', '2 peek int', '5 visible int']
		'models/models.v:10:10': ['5 hidden int', '2 peek int', '5 visible int']
	} {
		answer := ask_in(dir, spec)
		details := (json2.decode[Details](answer) or { panic('${err}: ${answer}') }).details
		assert details.map('${it.kind} ${it.label} ${it.detail}') == want, spec
	}
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
	res := cmdexec.run_in(line_info_v3_bin, ['-w', '-check', '-nocolor', '-vls-mode', '-line-info',
		'${spec}', 'main.v'], dir)
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
	res := cmdexec.run_in(line_info_v3_bin, ['-w', '-check', '-nocolor',
		...(os.split_args(options) or { panic(err) }), '-vls-mode', '-line-info', '${spec}', '.'], dir)
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

// release_child writes to `fifo` once a child reads it, without waiting for a
// child that never does: a FIFO opened to write without a reader fails at once.
// `C.O_NONBLOCK` is POSIX, so the body is wrapped in `$if linux` rather than
// guarded by an early return: `$if !linux { return }` still compiles everything
// after it, which is why the unguarded version failed to build off Linux.
fn release_child(fifo string) {
	$if linux {
		for _ in 0 .. 500 {
			fd := C.open(&char(fifo.str), C.O_WRONLY | C.O_NONBLOCK)
			if fd >= 0 {
				C.write(fd, c'go', 2)
				C.close(fd)
				return
			}
			time.sleep(10 * time.millisecond)
		}
	}
}

fn test_a_child_whose_server_ended_before_the_child_followed_it_ends() {
	$if !linux {
		return
	}
	// Right after its fork, a child asks the kernel to end it with the server;
	// a server that ended in between leaves it running, checking for nobody. A
	// FIFO holds the child there while the test kills the server: the child has
	// to end then, before it checks and prints the error of this program.
	dir := program_dir('orphan_child', 'module main\n\nfn main() {\n\tprintln(missing)\n}\n')
	fifo := os.join_path(work_dir, 'orphan_child.fifo')
	assert os.exec(['mkfifo', '${fifo}']).exit_code == 0
	mut p := start_server(dir, {
		'V_DIAGNOSTICS_CHILD_PAUSE': fifo
	})
	defer {
		p.close()
	}
	p.stdin_write('check t0\n')
	started := read_until(mut p, ' t0\n')
	assert started.contains('v-diagnostics-server: child '), started
	child := started.all_after('v-diagnostics-server: child ').all_before(' ').int()
	assert child > 0, started
	p.signal_kill()
	p.wait()
	// Writing to the FIFO lets the child go on.
	release_child(fifo)
	for _ in 0 .. 1000 {
		if !os.exists('/proc/${child}') {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert !os.exists('/proc/${child}'), 'the child ${child} is still running'
	mut printed := ''
	for {
		chunk := p.stdout_read()
		if chunk == '' {
			break
		}
		printed += chunk
	}
	assert !printed.contains('missing'), printed
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

fn constrained(code string, line int, word string, nth int) string {
	return ask(os.join_path(work_dir, 'constraints'), code, line, word, nth)
}

fn constrained_completion(line int, col int) []Detail {
	answer := ask_at(os.join_path(work_dir, 'constraints'), '${line}:${col}')
	if answer == '' {
		return []
	}
	return (json2.decode[Details](answer) or { panic('${err}: ${answer}') }).details
}

fn test_a_constrained_value_has_the_members_of_its_constraint() {
	// `a.zz` in `longest[T Named]`: what `Named` declares.
	members := constrained_completion(23, 11).map('${it.kind} ${it.label} ${it.detail}')
	assert members == ['2 greet string', '5 name string'], members.str()
	assert constrained('hv^', 20, 'name', 0) == '{"contents":{"kind":"markdown","value":"```v\\nname string\\n```"}}'
	// The field of the interface that declares it.
	assert constrained('gd^', 20, 'name', 0) == 'main.v:4:1'
	// `x.zz` in `describe[T Number]`: what `int` and `f64` both have.
	labels := constrained_completion(28, 11).map(it.label)
	assert 'str' in labels, labels.str()
	assert 'hex' !in labels, labels.str()
}

fn closure(code string, line int, word string, nth int) string {
	return ask(os.join_path(work_dir, 'closures'), code, line, word, nth)
}

fn closure_completion(line int, col int) []string {
	answer := ask_at(os.join_path(work_dir, 'closures'), '${line}:${col}')
	if answer == '' {
		return []
	}
	return (json2.decode[Details](answer) or { panic('${err}: ${answer}') }).details.map('${it.kind} ${it.label} ${it.detail}')
}

fn hover_of(text string) string {
	return '{"contents":{"kind":"markdown","value":"```v\\n${text}\\n```"}}'
}

fn test_the_implicit_variables_and_lambdas_of_a_constrained_body_have_its_members() {
	// `it`, `a` and `b` of an array method over a `[]T`, and the parameter of a
	// lambda there, are values of `T`: they have what `Named` declares.
	named := ['2 greet string', '5 name string']
	assert closure_completion(18, 19) == named
	assert closure_completion(19, 20) == named
	assert closure_completion(21, 21) == named
	assert closure('hv^', 20, 'it', 0) == hover_of('it T\\nT: implements main.Named')
	assert closure('hv^', 19, 'a', 0) == hover_of('a T\\nT: implements main.Named')
	assert closure('hv^', 21, 'x', 1) == hover_of('x T\\nT: implements main.Named')
	// The parameter of a lambda is where its uses are declared, in any body.
	assert closure('gd^', 21, 'x', 1) == 'main.v:21:16'
	assert closure('gd^', 26, 'u', 1) == 'main.v:26:20'
	assert closure('hv^', 26, 'u', 1) == hover_of('u main.User')
	// `err` of an `or {}` in a generic body: an `IError`.
	errs := closure_completion(37, 14).map(it.all_after(' ').all_before(' '))
	assert 'msg' in errs && 'code' in errs, errs.str()
}

fn test_a_field_of_a_constrained_type_has_the_members_of_its_constraint() {
	// `b.item.name` in `fn (b Box[T]) label()`, `item T` of `Box[T Named]`.
	assert constrained('hv^', 50, 'name', 0) == '{"contents":{"kind":"markdown","value":"```v\\nname string\\n```"}}'
	assert constrained('gd^', 50, 'name', 0) == 'main.v:4:1'
	members := constrained_completion(50, 29).map('${it.kind} ${it.label} ${it.detail}')
	assert members == ['2 greet string', '5 name string'], members.str()
}

fn test_a_constraint_name_has_a_definition_and_a_hover() {
	// `Number` in `describe[T Number]`: the sum type, `type Number = int | f64`.
	assert constrained('gd^', 27, 'Number', 0) == 'main.v:17:5'
	assert constrained('hv^', 27, 'Number', 0) == '{"contents":{"kind":"markdown","value":"```v\\ntype Number = int | f64\\n```"}}'
	// On the name it declares: that declaration.
	assert constrained('gd^', 17, 'Number', 0) == 'main.v:17:5'
	// `Named` in `longest[T Named]`: the interface, as any type written there.
	assert constrained('gd^', 19, 'Named', 0) == 'main.v:3:10'
}

fn test_a_compile_time_branch_in_a_constrained_body_leaves_the_queries_working() {
	// `$if T is f64 {` in `half[T Number]`: the walk of the body narrows `T` in
	// each branch, and the program still answers the editor's queries.
	assert constrained('hv^', 37, 'Number', 0) == '{"contents":{"kind":"markdown","value":"```v\\ntype Number = int | f64\\n```"}}'
	assert constrained('gd^', 39, 'x', 0) == 'main.v:37:18'
}

fn test_many_compile_time_branches_in_constrained_bodies_leave_the_queries_working() {
	// A dozen `$if T is ...` chains: the walk narrows `T` in each branch without
	// closures, which fail in the runtime of the compiler once there are enough.
	dir := os.join_path(work_dir, 'comptime_many')
	os.mkdir_all(dir) or { panic(err) }
	mut src := 'module main\n\ntype Number = int | i8 | i64 | f32 | f64\n\n'
	for i in 0 .. 12 {
		src += "fn f${i}[T Number](x T) string {\n\t\$if T is f64 {\n\t\treturn 'f64'\n\t} \$else \$if T is f32 || T is i8 {\n\t\treturn 'f32'\n\t} \$else \$if T in [int, i64] {\n\t\treturn 'int'\n\t} \$else {\n\t\treturn x.str()\n\t}\n}\n\n"
	}
	src += 'fn main() {}\n'
	os.write_file(os.join_path(dir, 'main.v'), src) or { panic(err) }
	assert ask_at(dir, '3:hv^6') == '{"contents":{"kind":"markdown","value":"```v\\ntype Number = int | i8 | i64 | f32 | f64\\n```"}}'
}

fn test_a_compile_time_is_narrows_what_a_constrained_value_offers() {
	// In `$if T is f32 || T is f64 {`, a `T` offers what `f32` and `f64` both
	// have, not only what every type of its set has; `$if value is f64 {` asks the
	// same of `value T`, and its `$else` has the rest. With an interface, `$if a is
	// User {` makes `a` a `User`: its members, and its field on F12.
	dir := os.join_path(work_dir, 'comptime_members')
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main

interface Named {
	name string
}

struct User {
	name string
	age  int
}

type Numeric = int | i8 | f32 | f64

fn test[T Numeric](value T) string {
	\$if T is f32 || T is f64 {
		println(value.zz)
	} \$else {
		println(value.zz)
	}
	\$if value is f64 {
		println(value.zz)
	} \$else \$if value is int {
		println(value.zz)
	}
	return value.str()
}

fn named[T Named](a T) {
	\$if a is User {
		println(a.zz)
		println(a.name)
	} \$else {
		println(a.zz)
		println(a.name)
	}
}

fn main() {}
') or {
		panic(err)
	}
	labels := fn [dir] (line int, col int) []string {
		answer := ask_at(dir, '${line}:${col}')
		assert answer != '', '${line}:${col}'
		return (json2.decode[Details](answer) or { panic('${err}: ${answer}') }).details.map(it.label)
	}
	floats := ['eq_epsilon', 'str', 'strg', 'strlong', 'strsci']
	assert labels(16, 16) == floats
	assert labels(18, 16) == ['hex', 'hex_full', 'str']
	assert labels(21, 16) == floats
	assert labels(23, 16) == ['hex', 'hex2', 'hex_full', 'str']
	assert labels(30, 12) == ['age', 'name']
	assert labels(33, 12) == ['name']
	assert ask(dir, 'gd^', 31, 'name', 0) == 'main.v:8:1'
	assert ask(dir, 'gd^', 34, 'name', 0) == 'main.v:4:1'
}

// function asks about the `nth` `word` of the line of functions_program that
// reads `text`.
fn function(code string, text string, word string, nth int) string {
	dir := os.join_path(work_dir, 'functions')
	line := functions_line(text)
	return ask(dir, code, line, word, nth)
}

// functions_line is the 1-based number of the line of functions_program that
// reads `text`.
fn functions_line(text string) int {
	line := functions_program.split('\n').index(text) + 1
	assert line > 0, '`${text}` is not a line of functions_program'
	return line
}

// decl_in_builtin_array reports whether a go-to-definition result points into
// `builtin/array.v`. The server reports the declaration at its real path, so the
// separators differ per platform and are normalised before the comparison.
fn decl_in_builtin_array(decl string) bool {
	return decl.replace('\\', '/').contains('builtin/array.v:')
}

fn test_a_member_of_a_generic_value_is_the_member_of_its_type() {
	// `xs.map()` over `xs []T` in a generic body calls the `map` of every array:
	// the declaration and the signature of `nums.map()` over an `[]int`.
	map_decl := function('gd^', '\tprintln(nums.map(it * 2))', 'map', 0)
	assert decl_in_builtin_array(map_decl), map_decl
	assert function('gd^', '\tprintln(xs.map(count))', 'map', 0) == map_decl
	assert function('gd^', '\treturn xs.map(|x| x.name.to_upper())', 'map', 0) == map_decl
	// Over an array literal, whose elements the checker keeps no type for.
	assert function('gd^', '\tprintln([3, 4].map(it + 1))', 'map', 0) == map_decl
	assert function('hv^', '\tprintln([3, 4].map(it + 1))', 'it', 0) == hover_of('it int')
	map_hover := function('hv^', '\tprintln(nums.map(it * 2))', 'map', 0)
	assert map_hover.contains('fn map('), map_hover
	assert function('hv^', '\tprintln(xs.map(count))', 'map', 0) == map_hover
	filter_decl := function('gd^', '\tprintln(nums.filter(it > 1))', 'filter', 0)
	assert decl_in_builtin_array(filter_decl), filter_decl
	assert function('gd^', '\tprintln(xs.filter(it.name.len > 1))', 'filter', 0) == filter_decl
	sort_decl := function('gd^', '\tsorted.sort(a < b)', 'sort', 0)
	assert decl_in_builtin_array(sort_decl), sort_decl
	assert function('gd^', '\txs.sort(a.name < b.name)', 'sort', 0) == sort_decl
	// A method of a generic struct, called on a `Box[T]`.
	label := functions_line('fn (b Box[T]) label() string {')
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
	longest := functions_line('fn longest[T Named](a T, b T) T {')
	assert function('hv^', '\tprintln(longest[User](u, u).name)', 'longest', 0) == hover_of('fn longest[T Named](a T, b T) T')
	assert function('gd^', '\tprintln(longest[User](u, u).name)', 'longest', 0) == 'main.v:${longest}:3'
	// A function literal in a generic body takes a `T`, written by its name.
	assert function('hv^', '\tprintln(xs.map(count))', 'count', 0) == hover_of('count fn (T) int\\nT: implements main.Named')
	assert function('hv^', '\tcount := fn (x T) int {', 'count', 0) == hover_of('count fn (T) int\\nT: implements main.Named')
}

fn test_a_static_method_is_declared_by_its_name() {
	new := functions_line('fn User.new(name string) User {')
	assert function('gd^', 'fn User.new(name string) User {', 'new', 0) == 'main.v:${new}:8'
	assert function('gd^', "\tu := User.new('eva')", 'new', 0) == 'main.v:${new}:8'
}

fn test_a_chain_of_array_methods_in_a_generic_body_keeps_its_elements() {
	// In a body without constraints, which the checker does not type:
	// `xs.filter()` gives a `[]T`, whose `map` is that of every array, and whose
	// elements are the `it` of that `map`.
	dir := os.join_path(work_dir, 'chains')
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main

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
') or {
		panic(err)
	}
	map_decl := ask(dir, 'gd^', 11, 'map', 0)
	assert decl_in_builtin_array(map_decl), map_decl
	assert ask(dir, 'gd^', 4, 'map', 0) == map_decl
	assert ask(dir, 'hv^', 4, 'it', 0) == hover_of('it T')
}

// local asks about the first `word` of the line of locals_program that reads
// `text`.
fn local(code string, text string, word string) string {
	line := locals_program.split('\n').index(text) + 1
	assert line > 0, '`${text}` is not a line of locals_program'
	return ask(os.join_path(work_dir, 'locals'), code, line, word, 0)
}

fn test_a_local_of_a_generic_body_has_the_type_of_its_value() {
	// The type its value has, with a type parameter by its name; a variable of
	// a `for ... in` loop has what its container holds.
	for text, want in {
		'\tlengths := xs.map(it.name.len)': 'lengths []int'
		'\tfor n in lengths {':             'n int'
		'\tlabel := x.name':                'label string'
		'\tsame := x':                      'same T\\nT: implements main.Named'
		'\tfirst := xs[0]':                 'first T\\nT: implements main.Named'
		'\tone, two := x.name, 2':          'one string'
		'\tword, size := pair(x)':          'word string'
		'\tfor i, item in xs {':            'i int'
		'\tfor key, value in m {':          'key string'
		'\tfor c in label {':               'c u8'
		'\tfor k in 0 .. 3 {':              'k int'
		'\t\ttotal += n':                   'total int'
	} {
		name := want.all_before(' ')
		assert local('hv^', text, name) == hover_of(want), '${text}: ${local('hv^', text, name)}'
	}
	for text, want in {
		'\tfor n in lengths {':    'lengths []int'
		'\tone, two := x.name, 2': 'two int'
		'\tword, size := pair(x)': 'size int'
		'\tfor i, item in xs {':   'item T\\nT: implements main.Named'
		'\tfor key, value in m {': 'value T\\nT: implements main.Named'
	} {
		name := want.all_before(' ')
		assert local('hv^', text, name) == hover_of(want), '${text}: ${local('hv^', text, name)}'
	}
	// A local that holds a `T` has what its constraint declares, as the `T` does.
	line := locals_program.split('\n').index("\tprintln('\${same.name} \${first.name} \${one} \${two} \${word} \${size}')") + 1
	assert line > 0
	text := locals_program.split('\n')[line - 1]
	for receiver in ['same', 'first'] {
		col := text.index('${receiver}.') or { -1 } + receiver.len + 2
		answer := ask_at(os.join_path(work_dir, 'locals'), '${line}:${col}')
		labels := (json2.decode[Details](answer) or { panic('${err}: ${answer}') }).details.map(it.label)
		assert labels == ['name'], '${receiver}: ${labels}'
	}
}

fn test_a_local_of_no_known_type_has_no_hover() {
	// The value of a call of a function that does not exist yet, as one is being
	// written: the checker gives it no type, which is no answer, not `()`.
	dir := os.join_path(work_dir, 'unknown_local')
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main

fn main() {
	value := missing_function(1)
	println(value)
}
') or {
		panic(err)
	}
	assert ask(dir, 'hv^', 4, 'value', 0) == ''
	assert ask(dir, 'hv^', 5, 'value', 0) == ''
	// Where it is declared still is.
	assert ask(dir, 'gd^', 5, 'value', 0) == 'main.v:4:1'
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

fn test_a_call_of_a_generic_method_on_a_struct_literal_has_the_type_it_returns() {
	// `Host{}.first_of(values)` in a generic body is what `first_of` returns for
	// the `A` of `values`, as `h.first_of(values)` is: the receiver is a value of
	// the type that the literal writes.
	source := 'module main\n\nstruct Host {}\n\nfn (h Host) first_of[T](xs []T) T {\n\treturn xs[0]\n}\n\nfn inside[A](values []A) A {\n\tinferred := Host{}.first_of(values)\n\ttyped := Host{}.first_of[A](values)\n\tprintln(typed)\n\treturn inferred\n}\n\nfn main() {\n\tprintln(inside([1]))\n}\n'
	dir := program_dir('literal_receiver', source)
	assert ask(dir, 'hv^', line_of(source, '\tinferred := Host{}.first_of(values)'), 'inferred',
		0) == hover_of('inferred A')
	assert ask(dir, 'hv^', line_of(source, '\ttyped := Host{}.first_of[A](values)'), 'typed', 0) == hover_of('typed A')
}

// line_of is the 1-based number of the line of `source` that reads `text`.
fn line_of(source string, text string) int {
	line := source.split('\n').index(text) + 1
	assert line > 0, '`${text}` is not a line'
	return line
}

fn test_a_type_name_in_a_comment_or_in_the_text_of_a_string_names_no_type() {
	// Only code refers to a type: a word of a comment or of the text of a
	// string that spells one is no reference; an interpolated expression is code.
	dir := program_dir('type_words_in_text', "module main

struct Marker {}

fn main() {
	println('Marker')
	// Marker is just text.
	println(Marker{})
	println('\${Marker{}}')
}
")
	for line in [6, 7] {
		assert ask(dir, 'gd^', line, 'Marker', 0) == '', 'line ${line}'
		assert ask(dir, 'hv^', line, 'Marker', 0) == '', 'line ${line}'
	}
	for line in [8, 9] {
		assert ask(dir, 'gd^', line, 'Marker', 0) == 'main.v:3:7', 'line ${line}'
		assert ask(dir, 'hv^', line, 'Marker', 0) == hover_of('struct Marker'), 'line ${line}'
	}
}

fn test_a_cursor_right_after_a_type_name_that_ends_a_comment_names_no_type() {
	// A cursor right after a name is on that name: after the last word of a
	// line comment, before its newline or at the end of the file, it is still
	// in the comment. In a declaration, the same cursor names the type.
	before_newline := program_dir('type_word_ends_comment', 'module main

struct Marker {}

struct Holder {
	m Marker
}

fn main() {
// Marker
}
')
	at_end := program_dir('type_word_ends_file', 'module main

struct Marker {}

fn main() {}

// Marker')
	for dir, line in {
		before_newline: 10
		at_end:         7
	} {
		assert ask_at(dir, '${line}:gd^9') == '', dir
		assert ask_at(dir, '${line}:hv^9') == '', dir
	}
	assert ask_at(before_newline, '6:gd^9') == 'main.v:3:7'
	assert ask_at(before_newline, '6:hv^9') == hover_of('struct Marker')
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
	assert decl_in_builtin_array(map_decl), map_decl
	assert ask(dir, 'gd^', 4, 'map', 0) == map_decl
}

const prepared_program = "module main

import os

fn unused() {}

fn count(names []string) int {
	return names.len + os.args.len
}

fn main() {
	x := count(['a']) + 'b'
	println(x)
}
"

// check_as_prepared_server checks `dir` with a diagnostics server that prepares
// the modules builtin imports before its first check, as VLS starts the one of
// its diagnostics, after `change` runs on `dir`, and returns what the check
// printed and what the server traced.
fn check_as_prepared_server(dir string, change fn (string)) (string, string) {
	trace := os.join_path(dir, 'trace.txt')
	os.rm(trace) or {}
	mut p := start_server_with(dir, [], {
		'V_DIAGNOSTICS_PREPARE': '1'
		'V_DIAGNOSTICS_TRACE':   trace
	})
	defer {
		p.close()
	}
	change(dir)
	p.stdin_write('check c\n')
	out := read_until(mut p, 'v-diagnostics-server: end ')
	p.stdin_write('quit\n')
	p.wait()
	mut lines := []string{}
	for line in out.split_into_lines() {
		if !line.starts_with('v-diagnostics-server: ') {
			lines << line
		}
	}
	code := out.all_after('v-diagnostics-server: end ').all_before(' ')
	return 'exit ${code}\n' + lines.join('\n').trim_space(), os.read_file(trace) or { '' }
}

// one_shot_check checks `dir` as a one-shot check of the same command line.
fn one_shot_check(dir string) string {
	res := cmdexec.run_in('env', ['V_CHECK_SELECTED_FILES_ONLY=1', line_info_v3_bin, '-no-memory-limit',
		'-w', '-check', '-nocolor', '.'], dir)
	return 'exit ${res.exit_code}\n' + res.output.trim_space()
}

fn test_a_prepared_server_checks_as_a_one_shot_check_does() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'prepared')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'main.v'), prepared_program)!
	checked, trace := check_as_prepared_server(dir, fn (_ string) {})
	assert checked == one_shot_check(dir)
	assert checked.contains('error: '), checked
	// The server parsed the modules builtin imports once, and collected their
	// declarations: the check continued from them.
	assert trace.contains('v-diagnostics-server: prepared '), trace
	assert trace.contains(' strconv'), trace
	assert trace.contains(', collected'), trace
	assert !trace.contains('anew'), trace
	assert !trace.contains('one-shot check'), trace
}

fn test_a_prepared_server_collects_anew_a_program_that_declares_a_name_of_a_prepared_module() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'prepared_names')
	os.mkdir_all(dir)!
	// strconv declares `f64_from_bits` too: collected after strconv, as the
	// prepared declarations would have it, the function of the program would
	// lose the entries whose first declaration wins.
	os.write_file(os.join_path(dir, 'main.v'), prepared_program +
		'\nfn f64_from_bits(b u64) f64 {\n\treturn f64(b)\n}\n')!
	checked, trace := check_as_prepared_server(dir, fn (_ string) {})
	assert checked == one_shot_check(dir)
	assert trace.contains('collecting every declaration anew: the name `f64_from_bits`'), trace
}

fn test_a_prepared_server_checks_as_a_one_shot_check_does_once_a_module_shadows_a_prepared_one() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'prepared_shadow')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'main.v'), prepared_program)!
	// A module of the project named like one builtin imports, after the server
	// prepared the one of vlib: an import of it from builtin resolves to it now.
	checked, trace := check_as_prepared_server(dir, fn (dir string) {
		os.mkdir_all(os.join_path(dir, 'strings')) or { panic(err) }
		os.write_file(os.join_path(dir, 'strings', 'strings.v'), 'module strings\n\npub fn shadow() {}\n') or {
			panic(err)
		}
	})
	assert checked == one_shot_check(dir)
	assert trace.contains('one-shot check: the prepared module strings resolves to another directory'), trace
}

const shadowed_program = 'module main

fn twice(x int) int {
	return x * 2
}

fn main() {
	y := twice(3)
	println(y)
}
'

fn test_a_prepared_server_answers_a_question_once_a_module_shadows_a_prepared_one() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'prepared_shadow_query')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'main.v'), shadowed_program)!
	trace := os.join_path(dir, 'trace.txt')
	mut p := start_server(dir, {
		'V_DIAGNOSTICS_PREPARE': '1'
		'V_DIAGNOSTICS_TRACE':   trace
	})
	defer {
		p.close()
	}
	// A module of the project named like one builtin imports, after the server
	// prepared the one of vlib: the child checks as a one-shot run does, and
	// still answers the questions, not with the diagnostics of a check.
	os.mkdir_all(os.join_path(dir, 'strings'))!
	os.write_file(os.join_path(dir, 'strings', 'strings.v'), 'module strings\n\npub fn shadow() {}\n')!
	questions := ['9:hv^10', '8:gd^7']
	expected := ask_once(dir, questions)
	assert expected.all(it != ''), expected.str()
	_, answer := query(mut p, 'a', questions.map('main.v:${it}').join('\t'))
	assert answer.split('\n').map(it.all_after('\t')) == expected
	assert (os.read_file(trace) or { '' }).contains('one-shot check: the prepared module strings resolves to another directory')
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

// server_check asks the server to check its program, and returns the pid of the
// child that answered, and what it answered, as one_shot_check writes it.
fn server_check(mut p os.Process, token string) (int, string) {
	p.stdin_write('check ${token}\n')
	out := read_until(mut p, 'v-diagnostics-server: end ')
	marker := 'v-diagnostics-server: child '
	start := out.index(marker) or { panic(out) }
	child := out[start + marker.len..].all_before(' ').int()
	mut lines := []string{}
	for line in out.split_into_lines() {
		if !line.starts_with('v-diagnostics-server: ') {
			lines << line
		}
	}
	code := out.all_after('v-diagnostics-server: end ').all_before(' ')
	assert out.contains('v-diagnostics-server: end ${code} ${token}'), out
	return child, 'exit ${code}\n' + lines.join('\n').trim_space()
}

const shared_program = "module main

fn unused() {}

fn twice(x int) int {
	return x * 2
}

fn main() {
	y := twice(3) + 'a'
	println(y)
}
"

fn test_a_shared_server_child_answers_the_checks_and_the_questions_of_its_program() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'shared')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'main.v'), shared_program)!
	expected := one_shot_check(dir)
	assert expected.contains('error: '), expected
	questions := ['10:hv^2', '10:gd^8']
	answers := ask_once(dir, questions)
	mut p := start_server(dir, {
		'V_DIAGNOSTICS_SHARED': '1'
	})
	defer {
		p.close()
	}
	// The child of the check answers the questions, and the next checks, from
	// the program it checked.
	first, checked := server_check(mut p, 'a')
	assert checked == expected
	child, answer := query(mut p, 'b', questions.map('main.v:${it}').join('\t'))
	assert child == first
	assert answer.split('\n').map(it.all_after('\t')) == answers
	again, rechecked := server_check(mut p, 'c')
	assert again == first
	assert rechecked == expected
	// Another content: a new child checks it.
	os.write_file(os.join_path(dir, 'main.v'), shared_program.replace(" + 'a'", ''))!
	changed, fixed := server_check(mut p, 'd')
	assert changed != first
	assert fixed == one_shot_check(dir)
	assert !fixed.contains('error: '), fixed
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

fn test_a_shared_server_child_made_for_a_question_answers_the_checks() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'shared_question')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'main.v'), shared_program)!
	expected := one_shot_check(dir)
	mut p := start_server(dir, {
		'V_DIAGNOSTICS_SHARED':  '1'
		'V_DIAGNOSTICS_PREPARE': '1'
	})
	defer {
		p.close()
	}
	first, answer := query(mut p, 'a', 'main.v:10:hv^2')
	assert answer == ask_once(dir, ['10:hv^2'])[0]
	for token in ['b', 'c'] {
		child, checked := server_check(mut p, token)
		assert child == first
		assert checked == expected
	}
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

fn test_a_server_keeps_the_children_of_the_last_versions_of_a_program() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'versions')
	os.mkdir_all(dir)!
	path := os.join_path(dir, 'main.v')
	versions := [shared_program, shared_program.replace(" + 'a'", ''),
		shared_program.replace('twice(3)', 'twice(4)')]
	mut expected := []string{}
	for version in versions {
		os.write_file(path, version)!
		expected << one_shot_check(dir)
	}
	mut p := start_server(dir, {
		'V_DIAGNOSTICS_SHARED':        '1'
		'V_DIAGNOSTICS_WARM_CHILDREN': '2'
	})
	defer {
		p.close()
	}
	mut children := []int{}
	for i in 0 .. 2 {
		os.write_file(path, versions[i])!
		child, checked := server_check(mut p, 'v${i}')
		assert checked == expected[i]
		children << child
	}
	assert children[0] != children[1]
	// Back to the first version, as when an edit is undone: its child answers
	// the check and the questions.
	os.write_file(path, versions[0])!
	back, checked := server_check(mut p, 'back')
	assert back == children[0]
	assert checked == expected[0]
	asked, _ := query(mut p, 'q', 'main.v:10:hv^2')
	assert asked == children[0]
	// A third version gets a child of its own, and the child of the version
	// asked about longest ago leaves: two stay at most.
	os.write_file(path, versions[2])!
	third, checked_third := server_check(mut p, 'v2')
	assert third !in children
	assert checked_third == expected[2]
	for _ in 0 .. 200 {
		if !os.exists('/proc/${children[1]}') {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert !os.exists('/proc/${children[1]}')
	os.write_file(path, versions[1])!
	again, checked_again := server_check(mut p, 'v1')
	assert again !in [children[0], children[1], third]
	assert checked_again == expected[1]
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

fn test_the_child_of_a_question_answers_again_once_its_version_comes_back() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'version_back')
	os.mkdir_all(dir)!
	path := os.join_path(dir, 'main.v')
	os.write_file(path, program)!
	mut p := start_server(dir, {})
	defer {
		p.close()
	}
	first, _ := query(mut p, 'a', 'main.v:42:gd^7')
	os.write_file(path, '// One line more.\n' + program)!
	second, moved := query(mut p, 'b', 'main.v:43:gd^7')
	assert second != first
	assert moved == './main.v:42:1'
	os.write_file(path, program)!
	back, answer := query(mut p, 'c', 'main.v:42:gd^7')
	assert back == first
	assert answer == './main.v:41:1'
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

fn test_a_prepared_server_scans_the_prepared_modules_for_implicit_imports_once() {
	$if !linux {
		return
	}
	// A program with no value that may be a closure: the scan of the prepared
	// modules reads the field index, which the program's declarations join.
	dir := os.join_path(work_dir, 'prepared_scan')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'main.v'), shared_program)!
	checked, trace := check_as_prepared_server(dir, fn (_ string) {})
	assert checked == one_shot_check(dir)
	assert checked.contains('error: '), checked
	assert trace.contains('replaying the scan of the prepared modules'), trace
	assert !trace.contains('scanning the prepared modules anew'), trace
	// One that may need the closure runtime already: the scan reads no index.
	os.write_file(os.join_path(dir, 'main.v'), prepared_program)!
	checked_closure, trace_closure := check_as_prepared_server(dir, fn (_ string) {})
	assert checked_closure == one_shot_check(dir)
	assert trace_closure.contains('replaying the scan of the prepared modules'), trace_closure
}

fn test_a_prepared_server_scans_the_prepared_modules_anew_for_a_program_that_changes_what_they_read() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'prepared_scan_anew')
	os.mkdir_all(dir)!
	// strconv calls a function `tos`: a program's own `tos` could change what
	// the scan of strconv finds.
	os.write_file(os.join_path(dir, 'main.v'), shared_program +
		'\nfn tos(x int) int {\n\treturn x\n}\n')!
	checked, trace := check_as_prepared_server(dir, fn (_ string) {})
	assert checked == one_shot_check(dir)
	assert trace.contains('scanning the prepared modules anew: the program writes `r:tos`'), trace
}

// generic_heavy_program has many instances of generic functions, which the end
// of a check checks after the rest, and an error the rest finds.
fn generic_heavy_program() string {
	mut source := 'module main\n\nstruct Box[T] {\n\tvalue T\n}\n'
	for i in 0 .. 40 {
		source += '\nfn wrap_${i}[T](x T) Box[T] {\n\treturn Box[T]{\n\t\tvalue: x\n\t}\n}\n'
		source += '\nfn unwrap_${i}[T](b Box[T]) T {\n\treturn b.value\n}\n'
	}
	source += "\nfn main() {\n\tbroken := 1 + 'a'\n\tprintln(broken)\n"
	for i in 0 .. 40 {
		for typ in ['1', "'s'", '1.5', 'true', 'u8(1)'] {
			source += '\tprintln(unwrap_${i}(wrap_${i}(${typ})))\n'
		}
	}
	return source + '}\n'
}

fn test_a_shared_server_sends_the_errors_of_a_long_check_before_its_end() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'partial')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'main.v'), generic_heavy_program())!
	expected := one_shot_check(dir)
	assert expected.contains('error: '), expected
	for vars in [{
		'V_DIAGNOSTICS_SHARED': '1'
	}, {
		'V_DIAGNOSTICS_SHARED':  '1'
		'V_DIAGNOSTICS_PARTIAL': '1'
	}] {
		mut p := start_server(dir, vars)
		p.stdin_write('check t\n')
		out := read_until(mut p, 'v-diagnostics-server: end ')
		p.stdin_write('quit\n')
		p.wait()
		p.close()
		marker := 'v-diagnostics-server: partial 1 t\n'
		if 'V_DIAGNOSTICS_PARTIAL' !in vars {
			// A client that takes no partial answers gets none.
			assert !out.contains('v-diagnostics-server: partial'), out
			continue
		}
		assert out.contains(marker), out
		// The error the rest of the check found comes first, and the whole
		// answer after it.
		first := out.all_before(marker)
		assert first.contains('cannot use `string`') || first.contains('error: '), first
		rest := out.all_after(marker)
		code := rest.all_after('v-diagnostics-server: end ').all_before(' ')
		assert 'exit ${code}\n' + rest.all_before('v-diagnostics-server: end ').trim_space() == expected
	}
}

const incremental_program = "module main

import os { join_path }

struct Point {
	x int
	y int
}

fn helper(p Point) int {
	return p.x + p.y
}

fn callback(x int) int {
	return x + 1
}

fn apply(f fn (int) int, x int) int {
	return f(x)
}

fn fails() !int {
	return error('no')
}

fn never() {}

fn first() int {
	mut unused_local := 1
	return helper(Point{1, 2})
}

fn second() string {
	fails()
	return join_path('a', 'b')
}

fn third() int {
	return apply(callback, 2) + 'x'
}

fn main() {
	println(first())
	println(second())
	println(third())
}
"

// IncrementalStep is an edit of the program of an incremental check, and what
// the check that follows it does: `checked` function bodies of the program's
// 9, or all of them, anew.
struct IncrementalStep {
	from    string
	to      string
	checked int = -1
}

// final_server_check checks with a server that may send a partial answer first,
// and returns the child and the answer after it.
fn final_server_check(mut p os.Process, token string) (int, string) {
	p.stdin_write('check ${token}\n')
	out := read_until(mut p, 'v-diagnostics-server: end ')
	marker := 'v-diagnostics-server: child '
	start := out.index(marker) or { panic(out) }
	child := out[start + marker.len..].all_before(' ').int()
	partial := out.index('v-diagnostics-server: partial ') or { -1 }
	final := if partial >= 0 { out[partial..].all_after('\n') } else { out }
	mut lines := []string{}
	for line in final.split_into_lines() {
		if !line.starts_with('v-diagnostics-server: ') {
			lines << line
		}
	}
	code := out.all_after('v-diagnostics-server: end ').all_before(' ')
	assert out.contains('v-diagnostics-server: end ${code} ${token}'), out
	return child, 'exit ${code}\n' + lines.join('\n').trim_space()
}

fn test_a_shared_server_checks_again_only_the_bodies_that_changed() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'incremental')
	os.mkdir_all(dir)!
	path := os.join_path(dir, 'main.v')
	os.write_file(path, incremental_program)!
	trace := os.join_path(dir, 'trace.txt')
	os.rm(trace) or {}
	// The server leaves out the bodies of a program this small too.
	mut p := start_server(dir, {
		'V_DIAGNOSTICS_SHARED':                '1'
		'V_DIAGNOSTICS_PREPARE':               '1'
		'V_DIAGNOSTICS_PARTIAL':               '1'
		'V_DIAGNOSTICS_TRACE':                 trace
		'V_DIAGNOSTICS_INCREMENTAL_MIN_NODES': '0'
	})
	defer {
		p.close()
	}
	steps := [
		// The first check has no earlier one to take anything from.
		IncrementalStep{},
		// A line more in `first` moves the diagnostics of the bodies after it.
		IncrementalStep{
			from:    '\tmut unused_local := 1\n'
			to:      "\tprintln('first')\n\tmut unused_local := 1\n"
			checked: 1
		},
		// With no error left, the rest of the check needs the bodies it left out:
		// markused looks for what uses `never`.
		IncrementalStep{
			from:    " + 'x'"
			to:      ' + 1'
			checked: 1
		},
		IncrementalStep{
			from:    '\tfails()\n'
			to:      ''
			checked: 1
		},
		// A declaration changes what every body may report.
		IncrementalStep{
			from: '\ty int\n}'
			to:   '\ty int\n\tz int\n}'
		},
		IncrementalStep{
			from:    '\tprintln(third())\n'
			to:      '\tprintln(third())\n\tprintln(helper(Point{}) + first())\n'
			checked: 1
		},
		IncrementalStep{
			from:    '\treturn helper(Point{1, 2})\n'
			to:      "\treturn helper(Point{1, 2}) + 'y'\n"
			checked: 1
		},
	]
	mut source := incremental_program
	for i, step in steps {
		if step.from != '' {
			assert source.contains(step.from), step.from
			source = source.replace_once(step.from, step.to)
			os.write_file(path, source)!
		}
		traced := (os.read_file(trace) or { '' }).len
		_, checked := final_server_check(mut p, 's${i}')
		assert checked == one_shot_check(dir), 'step ${i}'
		said := (os.read_file(trace) or { '' })[traced..]
		if step.checked < 0 {
			assert said.contains('incremental: every body checked'), 'step ${i}: ${said}'
		} else {
			assert said.contains('incremental: ${step.checked} of 9 bodies checked'), 'step ${i}: ${said}'
		}
		if i == 1 {
			// A body left out answers a question as a one-shot check does.
			line := source.split_into_lines().index("\treturn apply(callback, 2) + 'x'") + 1
			_, answer := query(mut p, 'q${i}', 'main.v:${line}:hv^10')
			assert answer.all_after('\t') == ask_once(dir, ['${line}:hv^10'])[0]
		}
	}
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

fn test_a_shared_server_checks_every_body_of_a_small_program() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'incremental_small')
	os.mkdir_all(dir)!
	path := os.join_path(dir, 'main.v')
	os.write_file(path, incremental_program)!
	trace := os.join_path(dir, 'trace.txt')
	os.rm(trace) or {}
	mut p := start_server(dir, {
		'V_DIAGNOSTICS_SHARED':  '1'
		'V_DIAGNOSTICS_PREPARE': '1'
		'V_DIAGNOSTICS_TRACE':   trace
	})
	defer {
		p.close()
	}
	_, _ = final_server_check(mut p, 'a')
	os.write_file(path, incremental_program.replace('\tfails()\n', ''))!
	traced := (os.read_file(trace) or { '' }).len
	_, checked := final_server_check(mut p, 'b')
	assert checked == one_shot_check(dir)
	// Leaving out its unchanged bodies would save less than it costs.
	said := (os.read_file(trace) or { '' })[traced..]
	assert said.contains('incremental: every body checked ('), said
	assert said.contains(' nodes '), said
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

const incremental_instance_errors_program = "module main

struct User {
	name string
}

fn plain() int {
	return 1
}

fn show[T](x T) string {
	return x.nme
}

fn other() int {
	return 2
}

fn caller() string {
	return show(User{
		name: 'a'
	})
}

fn main() {
	println(plain())
	println(other())
	println(caller())
}
"

// IncrementalInstanceStep is an edit of the program of an incremental check,
// and whether the check that follows it puts back the errors of the instances.
struct IncrementalInstanceStep {
	from     string
	to       string
	put_back bool
}

// The instances of the program's generic functions are those of the check
// before when no body checked again is generic or touches anything generic, now
// or before: a shared server puts their errors back instead of checking every
// instance again. With V_DIAGNOSTICS_INCREMENTAL_VERIFY, it checks them all the
// same, and finds what it put back.
fn test_a_shared_server_puts_back_the_errors_of_the_instances_when_the_bodies_it_checks_touch_nothing_generic() {
	$if !linux {
		return
	}
	check_incremental_instance_steps(false)!
	check_incremental_instance_steps(true)!
}

// check_incremental_instance_steps edits the program of
// incremental_instance_errors_program step by step, and checks it with a shared
// server after each step, which verifies what it puts back with `verify`.
fn check_incremental_instance_steps(verify bool) ! {
	steps := [
		IncrementalInstanceStep{},
		// A line more in a body that touches nothing generic moves the error of
		// the instance a line down.
		IncrementalInstanceStep{
			from:     '\treturn 1\n'
			to:       '\tone := 1\n\treturn one\n'
			put_back: true
		},
		// A body that asks for an instance.
		IncrementalInstanceStep{
			from: "name: 'a'"
			to:   "name: 'b'"
		},
		// The body of the generic function.
		IncrementalInstanceStep{
			from: 'x.nme'
			to:   'x.nam'
		},
		IncrementalInstanceStep{
			from:     '\treturn 2\n'
			to:       '\treturn 20\n'
			put_back: true
		},
		// A body that asked for an instance asks for none.
		IncrementalInstanceStep{
			from: "\treturn show(User{\n\t\tname: 'b'\n\t})\n"
			to:   "\treturn 'c'\n"
		},
		IncrementalInstanceStep{
			from:     '\treturn one\n'
			to:       '\treturn one + 1\n'
			put_back: true
		},
	]
	dir := os.join_path(work_dir, 'incremental_instance_errors_${verify}')
	os.mkdir_all(dir)!
	path := os.join_path(dir, 'main.v')
	os.write_file(path, incremental_instance_errors_program)!
	trace := os.join_path(dir, 'trace.txt')
	os.rm(trace) or {}
	mut vars := {
		'V_DIAGNOSTICS_SHARED':                '1'
		'V_DIAGNOSTICS_PREPARE':               '1'
		'V_DIAGNOSTICS_PARTIAL':               '1'
		'V_DIAGNOSTICS_TRACE':                 trace
		'V_DIAGNOSTICS_INCREMENTAL_MIN_NODES': '0'
	}
	if verify {
		vars['V_DIAGNOSTICS_INCREMENTAL_VERIFY'] = '1'
	}
	mut p := start_server(dir, vars)
	defer {
		p.close()
	}
	mut source := incremental_instance_errors_program
	for i, step in steps {
		if step.from != '' {
			assert source.contains(step.from), step.from
			source = source.replace_once(step.from, step.to)
			os.write_file(path, source)!
		}
		traced := (os.read_file(trace) or { '' }).len
		_, checked := final_server_check(mut p, 'i${i}')
		assert checked == one_shot_check(dir), 'verify ${verify}, step ${i}'
		said := (os.read_file(trace) or { '' })[traced..]
		if i > 0 {
			assert said.contains('incremental: 1 of 5 bodies checked'), 'verify ${verify}, step ${i}: ${said}'
		}
		assert said.contains('errors of the instances put back') == step.put_back, 'verify ${verify}, step ${i}: ${said}'
		assert !said.contains('incremental: the instances found'), 'verify ${verify}, step ${i}: ${said}'
		// The instances have errors, then none.
		assert checked.contains('`nme`') == (i < 3), 'verify ${verify}, step ${i}: ${checked}'
		assert checked.contains('`nam`') == (i in [3, 4]), 'verify ${verify}, step ${i}: ${checked}'
	}
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

// incremental_many_program is incremental_program with 30 functions more: work
// enough for a check that splits its bodies among threads.
fn incremental_many_program() string {
	mut source := incremental_program
	for i in 0 .. 30 {
		source += '\nfn filler_${i}(n int) int {\n\tmut total := 0\n\tfor j in 0 .. n {\n\t\tif j % 3 == ${i % 3} {\n\t\t\ttotal += j * ${i}\n\t\t}\n\t}\n\treturn total\n}\n'
	}
	return source
}

fn test_a_shared_server_that_checks_on_threads_checks_again_only_the_bodies_that_changed() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'incremental_threads')
	os.mkdir_all(dir)!
	path := os.join_path(dir, 'main.v')
	source := incremental_many_program()
	os.write_file(path, source)!
	trace := os.join_path(dir, 'trace.txt')
	os.rm(trace) or {}
	// Threads check the bodies in batches of their own.
	mut p := start_server(dir, {
		'V_DIAGNOSTICS_SHARED':                '1'
		'V_DIAGNOSTICS_PREPARE':               '1'
		'V_DIAGNOSTICS_PARTIAL':               '1'
		'V_DIAGNOSTICS_TRACE':                 trace
		'V_DIAGNOSTICS_INCREMENTAL_MIN_NODES': '0'
		'VJOBS':                               '4'
	})
	defer {
		p.close()
	}
	_, checked := final_server_check(mut p, 't0')
	assert checked == one_shot_check(dir)
	os.write_file(path, source.replace_once('\tfor j in 0 .. n {\n\t\tif j % 3 == 2 {',
		'\tfor j in 1 .. n {\n\t\tif j % 3 == 2 {'))!
	traced := (os.read_file(trace) or { '' }).len
	_, rechecked := final_server_check(mut p, 't1')
	assert rechecked == one_shot_check(dir)
	said := (os.read_file(trace) or { '' })[traced..]
	assert said.contains('incremental: 1 of 39 bodies checked'), said
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

const incremental_generic_program = "module main

struct User {
	name string
}

type Thing = User | int

fn name_of[T](x T) string {
	return x.name
}

fn first() string {
	return name_of(User{'a'})
}

fn second(t Thing) int {
	match t {
		int { _ = name_of(t) }
		User {}
	}
	return 2
}

fn main() {
	println(first())
	println(second(Thing(1)))
}
"

fn test_an_incremental_check_checks_the_instances_that_the_bodies_it_left_out_ask_for() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'incremental_generic')
	os.mkdir_all(dir)!
	path := os.join_path(dir, 'main.v')
	os.write_file(path, incremental_generic_program)!
	trace := os.join_path(dir, 'trace.txt')
	os.rm(trace) or {}
	// The server leaves out the bodies of a program this small too.
	mut p := start_server(dir, {
		'V_DIAGNOSTICS_SHARED':                '1'
		'V_DIAGNOSTICS_PREPARE':               '1'
		'V_DIAGNOSTICS_PARTIAL':               '1'
		'V_DIAGNOSTICS_TRACE':                 trace
		'V_DIAGNOSTICS_INCREMENTAL_MIN_NODES': '0'
	})
	defer {
		p.close()
	}
	_, checked := final_server_check(mut p, 'g0')
	expected := one_shot_check(dir)
	assert checked == expected
	// The instance `name_of[int]` comes from `t`, an `int` only in its branch of
	// the `match`: the check of `second` tells, and the check of `first` leaves
	// `second` out.
	assert expected.contains('`int` has no property `name`'), expected
	os.write_file(path, incremental_generic_program.replace("\treturn name_of(User{'a'})",
		"\t// first\n\treturn name_of(User{'a'})"))!
	traced := (os.read_file(trace) or { '' }).len
	_, rechecked := final_server_check(mut p, 'g1')
	assert rechecked == one_shot_check(dir)
	said := (os.read_file(trace) or { '' })[traced..]
	assert said.contains('incremental: 1 of 4 bodies checked'), said
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

const incremental_instances_program = "module main

struct User {
	name string
}

struct Box[T] {
	value T
}

interface Labeled {
	label() string
}

fn name_of_call[T](x T) string {
	return x.name
}

fn name_of_value[T](x T) string {
	return x.name
}

fn (b Box[T]) get_name() string {
	return b.value.name
}

fn (b Box[T]) str() string {
	return b.value.name
}

fn (b Box[T]) label() string {
	return b.value.name
}

fn int_box() Box[int] {
	return Box[int]{
		value: 1
	}
}

fn by_call() string {
	return name_of_call(3)
}

fn by_value() string {
	f := name_of_value[int]
	return f(1)
}

fn by_method() string {
	return int_box().get_name()
}

fn by_str() string {
	return '\${int_box()}'
}

fn by_interface() string {
	l := Labeled(int_box())
	return l.label()
}

fn filler_a() int {
	mut t := 0
	for i in 0 .. 10 {
		t += i
	}
	return t
}

fn filler_b() string {
	return 'b'
}

fn filler_c(xs []int) int {
	return xs.len
}

fn filler_d(s string) string {
	return s.to_upper()
}

fn main() {
	println(by_call())
	println(by_value())
	println(by_method())
	println(by_str())
	println(by_interface())
	println(filler_a())
	println(filler_b())
	println(filler_c([1]))
	println(filler_d('d'))
	println(name_of_call(User{'u'}))
}
"

fn test_an_incremental_check_checks_for_the_instances_only_the_bodies_that_can_ask_for_one() {
	$if !linux {
		return
	}
	dir := os.join_path(work_dir, 'incremental_instances')
	os.mkdir_all(dir)!
	path := os.join_path(dir, 'main.v')
	os.write_file(path, incremental_instances_program)!
	trace := os.join_path(dir, 'trace.txt')
	os.rm(trace) or {}
	mut p := start_server(dir, {
		'V_DIAGNOSTICS_SHARED':                '1'
		'V_DIAGNOSTICS_PREPARE':               '1'
		'V_DIAGNOSTICS_PARTIAL':               '1'
		'V_DIAGNOSTICS_TRACE':                 trace
		'V_DIAGNOSTICS_INCREMENTAL_MIN_NODES': '0'
	})
	defer {
		p.close()
	}
	_, checked := final_server_check(mut p, 'i0')
	expected := one_shot_check(dir)
	assert checked == expected
	// Each instance comes from one body: a call, a function value, a method, and
	// the `str()` of an interpolation.
	assert expected.count('`int` has no property `name`') == 4, expected
	// A body that touches nothing generic changed: the errors of the instances
	// are put back.
	mut source := incremental_instances_program.replace('\tmut t := 0\n', '\tmut t := 1\n')
	os.write_file(path, source)!
	mut traced := (os.read_file(trace) or { '' }).len
	_, put_back := final_server_check(mut p, 'i1')
	assert put_back == one_shot_check(dir)
	mut said := (os.read_file(trace) or { '' })[traced..]
	assert said.contains('incremental: 1 of 16 bodies checked'), said
	assert said.contains('errors of the instances put back'), said
	// A body that asks for an instance changed: the instances are checked, and
	// for them, the bodies left out that touch nothing generic are not checked
	// again: `filler_a`, `filler_b`, `filler_c` and `filler_d`.
	source = source.replace('\treturn name_of_call(3)\n', '\treturn name_of_call(4)\n')
	os.write_file(path, source)!
	traced = (os.read_file(trace) or { '' }).len
	_, rechecked := final_server_check(mut p, 'i2')
	assert rechecked == one_shot_check(dir)
	said = (os.read_file(trace) or { '' })[traced..]
	assert said.contains('incremental: 1 of 16 bodies checked'), said
	assert said.contains('incremental: 11 of 15 bodies left out checked for the instances'), said
	p.stdin_write('quit\n')
	p.wait()
	assert p.code == 0
}

const narrowing_program = "module main

interface Named {
	name string
}

struct User {
	name string
	age  int
}

type Number3 = int | i64 | f64

struct Circle {
	r f64
}

struct Square {
	side f64
}

type Shape = Circle | Square

fn half[T Number3](x T, y T, xs []T) f64 {
	same := x
	\$if T is f64 {
		println(xs)
		zero := T(0)
		return x + y + same + zero
	} \$else {
		return f64(x)
	}
}

fn listed[T Number3](x T) f64 {
	\$if T in [i64, f64] {
		return f64(x) * 2.0
	}
	return 0.0
}

fn nested_tests[T Number3](t T) f64 {
	// the value of t
	\$if T in [i64, f64] {
		\$if t is f64 {
			return t
		}
	}
	return 0.0
}

fn tested[T Number3](x T) f64 {
	\$if x is f64 {
		return x
	}
	return 0.0
}

fn named[T Named](a T) int {
	\$if a is User {
		return a.age
	}
	return a.name.len
}

fn family[T User](u T) string {
	return u.name
}

fn locals[T Named](x T) int {
	y := x
	mut all := []T{}
	all << y
	\$if y is User {
		mine := []T{}
		return y.age + mine.len + all.len
	}
	return all.len
}

fn area(s Shape) f64 {
	if s is Circle {
		return s.r
	}
	return match s {
		Square { s.side }
		else { 0.0 }
	}
}

fn age_of(n Named) int {
	if n is User {
		return n.age
	}
	return 0
}

fn main() {
	println(half(1.0, 2.0, [3.0]))
	println(tested(1))
	println(listed(2))
	println(nested_tests(3))
	println(named(User{'ana', 3}))
	println(family(User{'bo', 1}))
	println(locals(User{'cy', 2}))
	println(area(Circle{1.0}))
	println(age_of(User{'eva', 4}))
}
"

// narrowed asks about the `nth` `word` of the line of narrowing_program that
// reads `text`.
fn narrowed(code string, text string, word string, nth int) string {
	dir := os.join_path(work_dir, 'narrowing')
	if !os.exists(dir) {
		os.mkdir_all(dir) or { panic(err) }
		os.write_file(os.join_path(dir, 'main.v'), narrowing_program) or { panic(err) }
	}
	line := narrowing_program.split('\n').index(text) + 1
	assert line > 0, '`${text}` is not a line of narrowing_program'
	return ask(dir, code, line, word, nth)
}

fn test_a_value_of_a_type_parameter_shows_what_the_type_parameter_is_there() {
	// Outside the `$if`s that decide it, a type parameter stays, with what it
	// can be on a line of its own.
	number3 := 'T: int | i64 | f64'
	assert narrowed('hv^', 'fn half[T Number3](x T, y T, xs []T) f64 {', 'x', 0) == hover_of('x T\\n${number3}')
	assert narrowed('hv^', '\tsame := x', 'same', 0) == hover_of('same T\\n${number3}')
	// In the branch of `$if T is f64 {` it is `f64`, for every value of it.
	assert narrowed('hv^', '\t\tprintln(xs)', 'xs', 0) == hover_of('xs []f64')
	for word in ['x', 'y', 'same', 'zero'] {
		assert narrowed('hv^', '\t\treturn x + y + same + zero', word, 0) == hover_of('${word} f64')
	}
	// Its `$else` leaves the rest of the set.
	assert narrowed('hv^', '\t\treturn f64(x)', 'x', 0) == hover_of('x T\\nT: int | i64')
	// `$if x is f64 {` asks the same of a value; `$if T in [i64, f64] {` leaves
	// each type of its list.
	assert narrowed('hv^', '\t\treturn x', 'x', 0) == hover_of('x f64')
	assert narrowed('hv^', '\t\treturn f64(x) * 2.0', 'x', 0) == hover_of('x T\\nT: i64 | f64')
	// An interface: what implements it; in the branch of `$if a is User {`, `User`.
	assert narrowed('hv^', '\t\treturn a.age', 'a', 0) == hover_of('a main.User')
	assert narrowed('hv^', '\treturn a.name.len', 'a', 0) == hover_of('a T\\nT: implements main.Named')
	// A struct as the constraint stands for itself and the structs that embed it.
	assert narrowed('hv^', '\treturn u.name', 'u', 0) == hover_of('u T\\nT: main.User or a struct that embeds it')
	// A local that holds a `T`: a `$if` on it decides `T`, and a literal that
	// writes its type, `[]T{}`, has it.
	named := 'T: implements main.Named'
	assert narrowed('hv^', '\tmut all := []T{}', 'all', 0) == hover_of('all []T\\n${named}')
	assert narrowed('hv^', '\t\treturn y.age + mine.len + all.len', 'y', 0) == hover_of('y main.User')
	assert narrowed('hv^', '\t\treturn y.age + mine.len + all.len', 'age', 0) == hover_of('age int')
	assert narrowed('hv^', '\t\treturn y.age + mine.len + all.len', 'mine', 0) == hover_of('mine []main.User')
	assert narrowed('hv^', '\treturn all.len', 'all', 0) == hover_of('all []T\\n${named}')
}

fn test_a_type_parameter_of_an_unknown_constraint_says_nothing_more() {
	// `[T Nope]` is reported: its hover does not tell what `T` can be.
	dir := os.join_path(work_dir, 'unknown_constraint')
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\nfn wrong[T Nope](a T) T {\n\treturn a\n}\n\nfn main() {}\n') or {
		panic(err)
	}
	assert ask(dir, 'hv^', 3, 'T', 0) == hover_of('[T Nope]')
	assert ask(dir, 'hv^', 4, 'a', 0) == hover_of('a T')
}

fn test_a_type_parameter_shows_its_constraint_and_what_it_is_there() {
	half := 'fn half[T Number3](x T, y T, xs []T) f64 {'
	all := '[T Number3]\\nT: int | i64 | f64'
	// Where it is declared, in the types of the parameters and in a `$if`.
	assert narrowed('hv^', half, 'T', 0) == hover_of(all)
	assert narrowed('hv^', half, 'T', 3) == hover_of(all)
	assert narrowed('hv^', '\t\$if T is f64 {', 'T', 0) == hover_of(all)
	// In the branch of that `$if`, `f64`.
	assert narrowed('hv^', '\t\tzero := T(0)', 'T', 0) == hover_of('[T Number3]\\nT: f64')
	// A function writes its type parameters, with their constraints.
	assert narrowed('hv^', '\tprintln(half(1.0, 2.0, [3.0]))', 'half', 0) == hover_of('fn half[T Number3](x T, y T, xs []T) f64')
}

fn test_a_value_in_the_condition_of_a_compile_time_if_is_a_value_there() {
	// The condition keeps `t` as text: it is the parameter, with what the `$if`s
	// around it leave of `T`, not yet what this one decides.
	assert narrowed('hv^', '\t\t\$if t is f64 {', 't', 0) == hover_of('t T\\nT: i64 | f64')
	// A local that holds a `T`, and a parameter with no `$if` around it.
	assert narrowed('hv^', '\t\$if y is User {', 'y', 0) == hover_of('y T\\nT: implements main.Named')
	assert narrowed('hv^', '\t\$if x is f64 {', 'x', 0) == hover_of('x T\\nT: int | i64 | f64')
	// The same word in a comment is no value.
	assert narrowed('hv^', '\t// the value of t', 't', 0) == ''
}

fn test_a_type_parameter_is_declared_in_the_list_of_its_declaration() {
	// `T` of `take[T Named]`, of `plain[T]`, of `Box[T Named]` and of a method of
	// `Box[T]` is the type parameter, which hides the struct `T` of the module.
	dir := os.join_path(work_dir, 'type_param_definition')
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), "module main\n\ninterface Named {\n\tname string\n}\n\nstruct T {\n\tname string\n}\n\nfn take[T Named](x T) T {\n\treturn x\n}\n\nfn plain[T](x T) T {\n\treturn x\n}\n\nstruct Box[T Named] {\n\titem T\n}\n\nfn (b Box[T]) get() T {\n\treturn T(b.item)\n}\n\nfn main() {\n\tt := T{\n\t\tname: 'a'\n\t}\n\tprintln(take(t).name)\n\tprintln(plain(t).name)\n}\n") or {
		panic(err)
	}
	for nth in 0 .. 3 {
		assert ask(dir, 'gd^', 11, 'T', nth) == 'main.v:11:8'
		assert ask(dir, 'gd^', 15, 'T', nth) == 'main.v:15:9'
	}
	assert ask(dir, 'gd^', 20, 'T', 0) == 'main.v:19:11'
	assert ask(dir, 'gd^', 23, 'T', 1) == 'main.v:23:10'
	assert ask(dir, 'gd^', 24, 'T', 0) == 'main.v:23:10'
	// The struct `T` where a type parameter does not hide it.
	assert ask(dir, 'gd^', 28, 'T', 0) == 'main.v:7:7'
}

fn test_a_parameter_is_the_type_that_is_or_match_makes_it() {
	// As a local is: `if s is Circle {` and a branch of `match s {`.
	assert narrowed('hv^', '\t\treturn s.r', 's', 0) == hover_of('s main.Circle')
	assert narrowed('hv^', '\t\tSquare { s.side }', 's', 0) == hover_of('s main.Square')
	// An interface narrowed to a struct refers to the object the interface holds
	// (master's #29058): `if n is User {` makes `n` a `&User`.
	assert narrowed('hv^', '\t\treturn n.age', 'n', 0) == hover_of('n &main.User')
	// Where it is declared, and outside those branches, its declared type.
	assert narrowed('hv^', 'fn area(s Shape) f64 {', 's', 0) == hover_of('s main.Shape')
	assert narrowed('hv^', '\treturn match s {', 's', 0) == hover_of('s main.Shape')
}

const skipped_branches_program = "module main

type Number = int | i64 | f32 | f64

struct Separator {
	integer string
}

fn format[T Number](number T, sep string) string {
	\$if js {
		\$if T is f32 || T is f64 {
			return number.str() + sep
		} \$else {
			return number.str() + sep + '.'
		}
	} \$else \$if T is f64 {
		return number.str()
	} \$else {
		return number.str() + sep + ','
	}
}

fn joined(parts []string, sep string) string {
	separator := Separator{
		integer: sep
	}
	\$if js {
		return parts.join(separator.integer)
	}
	return parts.join(sep)
}

fn flagged(count int) int {
	total := count * 2
	\$if vls_extra ? {
		doubled := total * 2
		return doubled + count
	}
	return total
}

fn main() {
	println(format(1.5, ','))
	println(joined(['a', 'b'], ','))
	println(flagged(2))
}
"

// skipped asks about the `nth` `word` of the line of skipped_branches_program
// that reads `text`.
fn skipped(code string, text string, word string, nth int) string {
	dir := os.join_path(work_dir, 'skipped_branches')
	if !os.exists(dir) {
		os.mkdir_all(dir) or { panic(err) }
		os.write_file(os.join_path(dir, 'main.v'), skipped_branches_program) or { panic(err) }
	}
	line := skipped_branches_program.split('\n').index(text) + 1
	assert line > 0, '`${text}` is not a line of skipped_branches_program'
	return ask(dir, code, line, word, nth)
}

fn test_a_branch_that_the_parse_left_out_answers_as_the_others_do() {
	// The branch of `$if js {` of a check for C, where a `$if` on `T` decides
	// what the value of `T` is, as in the branches that are checked.
	then_line := '\t\t\treturn number.str() + sep'
	else_line := "\t\t\treturn number.str() + sep + '.'"
	assert skipped('hv^', then_line, 'number', 0) == hover_of('number T\\nT: f32 | f64')
	assert skipped('hv^', else_line, 'number', 0) == hover_of('number T\\nT: int | i64')
	assert skipped('gd^', then_line, 'number', 0) == 'main.v:9:20'
	// A type parameter in the condition of a `$if` of that branch.
	condition := '\t\t\$if T is f32 || T is f64 {'
	assert skipped('hv^', condition, 'T', 0) == hover_of('[T Number]\\nT: int | i64 | f32 | f64')
	assert skipped('gd^', condition, 'T', 0) == 'main.v:9:10'
	// A local of the code that is checked, a field of its type, and a parameter.
	joined_line := '\t\treturn parts.join(separator.integer)'
	assert skipped('hv^', joined_line, 'separator', 0) == hover_of('separator main.Separator')
	assert skipped('hv^', joined_line, 'integer', 0) == hover_of('integer string')
	assert skipped('hv^', joined_line, 'parts', 0) == hover_of('parts []string')
	assert skipped('gd^', joined_line, 'separator', 0) == 'main.v:24:1'
	// The branch of a flag that is not defined: a parameter, a local declared
	// before it, and one declared in it.
	flag_line := '\t\treturn doubled + count'
	assert skipped('hv^', flag_line, 'count', 0) == hover_of('count int')
	assert skipped('hv^', '\t\tdoubled := total * 2', 'total', 0) == hover_of('total int')
	assert skipped('gd^', '\t\tdoubled := total * 2', 'total', 0) == 'main.v:34:1'
	assert skipped('gd^', flag_line, 'doubled', 0) == 'main.v:36:2'
	// The code that is checked answers as before, also in a process that added
	// the nodes of the left-out branches for an earlier question: its own nodes
	// come first.
	live_line := "\t\treturn number.str() + sep + ','"
	live := hover_of('number T\\nT: int | i64 | f32')
	assert skipped('hv^', live_line, 'number', 0) == live
	assert skipped('hv^', '\ttotal := count * 2', 'total', 0) == hover_of('total int')
	lines := skipped_branches_program.split('\n')
	both := '${lines.index(then_line) + 1}:hv^14\tmain.v:${lines.index(live_line) + 1}:hv^13'
	assert ask_at(os.join_path(work_dir, 'skipped_branches'), both) == '0\t${hover_of('number T\\nT: f32 | f64')}\n1\t${live}'
}

const generic_locals_program = "module main

type Number = int | f64

struct Separator {
	integer string
}

fn format[T Number](number T, sep string) string {
	separator := match sep {
		'' { Separator{} }
		else { Separator{
			integer: sep
		} }
	}
	local := sep + '!'
	\$if js {
		return separator.integer + local
	}
	same := number
	return number.str() + separator.integer + local + same.str()
}

fn plain[T](x T, sep string) string {
	local := sep + '?'
	count := local.len
	return '\${x}' + local + count.str()
}

fn main() {
	println(format(1.5, ','))
	println(plain(2, ';'))
}
"

// generic_local asks about the `nth` `word` of the line of
// generic_locals_program that reads `text`.
fn generic_local(text string, word string, nth int) string {
	dir := os.join_path(work_dir, 'generic_locals')
	if !os.exists(dir) {
		os.mkdir_all(dir) or { panic(err) }
		os.write_file(os.join_path(dir, 'main.v'), generic_locals_program) or { panic(err) }
	}
	line := generic_locals_program.split('\n').index(text) + 1
	assert line > 0, '`${text}` is not a line of generic_locals_program'
	return ask(dir, 'hv^', line, word, nth)
}

fn test_a_local_of_a_generic_body_that_no_type_parameter_decides_has_its_type() {
	// The check leaves the body of a generic function to its instances: a local
	// whose value does not depend on the type parameters has the type that
	// every instance gives it, with or without constraints.
	ret := '\treturn number.str() + separator.integer + local + same.str()'
	assert generic_local('\tseparator := match sep {', 'separator', 0) == hover_of('separator main.Separator')
	assert generic_local(ret, 'separator', 0) == hover_of('separator main.Separator')
	assert generic_local("\tlocal := sep + '!'", 'local', 0) == hover_of('local string')
	assert generic_local(ret, 'local', 0) == hover_of('local string')
	assert generic_local("\tlocal := sep + '?'", 'local', 0) == hover_of('local string')
	assert generic_local('\tcount := local.len', 'count', 0) == hover_of('count int')
	// Also in a branch that the parse left out, asked alone.
	js := '\t\treturn separator.integer + local'
	assert generic_local(js, 'separator', 0) == hover_of('separator main.Separator')
	assert generic_local(js, 'local', 0) == hover_of('local string')
	// One that depends on them stays as it was.
	assert generic_local('\tsame := number', 'same', 0) == hover_of('same T\\nT: int | f64')
}

const joined_program = "module main

interface Named {
	name string
}

struct User {
	name string
	age  int
}

struct Admin {
	name  string
	level int
}

type Number = i8 | i16 | int | u8 | f32 | f64

fn all_f64[A Number, B Number, C Number](a A, b B, c C) f64 {
	\$if a is f64 && b is f64 && c is f64 {
		total := a + b + c
		return total
	}
	return f64(a) + f64(b) + f64(c)
}

fn all_f64_types[A Number, B Number, C Number](a A, b B, c C) f64 {
	\$if A is f64 && B is f64 && C is f64 {
		sum := A(a) + B(b) + C(c)
		return sum
	} \$else {
		return f64(a) * f64(b) * f64(c)
	}
}

fn in_lists[A Number, B Number](a A, b B) f64 {
	\$if a in [f32, f64] && b in [i8, u8] {
		return f64(a) + f64(b)
	} \$else {
		return f64(a) - f64(b)
	}
}

fn not_in_lists[A Number, B Number](a A, b B) f64 {
	\$if a !in [f32, f64] && b !in [i8, u8] {
		return f64(a) / f64(b)
	} \$else {
		return f64(a) - f64(b) - 1.0
	}
}

fn either_f64[A Number, B Number](a A, b B) f64 {
	\$if a is f64 || b is f64 {
		return f64(a) + f64(b) + 2.0
	} \$else {
		return f64(a) + f64(b) + 3.0
	}
}

fn either_in[A Number, B Number](a A, b B) f64 {
	\$if a in [i8, u8] || b !in [f32, f64] {
		return f64(a) + f64(b) + 4.0
	} \$else {
		return f64(a) + f64(b) + 5.0
	}
}

fn not_both_f64[A Number, B Number](a A, b B) f64 {
	\$if a !is f64 || b !is f64 {
		return f64(a) + f64(b) + 6.0
	} \$else {
		return a + b
	}
}

fn chain[A Number, B Number](a A, b B) f64 {
	\$if a is f64 && b is f64 {
		return a * b
	} \$else \$if a is f32 && b is f32 {
		return f64(a * b)
	} \$else \$if a is f64 {
		return a + f64(b)
	} \$else {
		return f64(a) * f64(b) * 7.0
	}
}

fn three_tests[T Number](x T) f64 {
	\$if x !is f64 && x !is f32 && x !is int {
		return f64(x) + 8.0
	}
	return 0.0
}

fn grouped[T Number](y T) f64 {
	\$if (y is f64 || y is f32) && y !is f32 {
		return y
	}
	return 1.0
}

fn negated[T Number](z T) f64 {
	\$if !(z is f64) {
		return f64(z) + 9.0
	} \$else {
		return z
	}
}

fn groups[T Number](v T) f64 {
	\$if T in [\$float, i8] {
		return f64(v) + 10.0
	} \$else \$if T !in [int] {
		return f64(v) + 11.0
	} \$else {
		return f64(v) + 12.0
	}
}

fn local_and_param[A Named, B Named](a A, b B) int {
	x := a
	\$if x is User && b is Admin {
		return x.age + b.level
	}
	return x.name.len + b.name.len
}

fn listed_names[A Named, B Named](a A, b B) int {
	\$if a in [User, Admin] && b !in [User] {
		return a.name.len + b.name.len + 1
	} \$else \$if a !in [User, Admin] {
		return a.name.len + b.name.len + 2
	} \$else {
		return a.name.len + b.name.len + 3
	}
}

fn free_param[A Number, U](a A, u U) f64 {
	\$if a is f64 && u is int {
		return a + f64(u)
	}
	return 14.0
}

fn nested[A Number, B Number, C Number](a A, b B, c C) f64 {
	\$if A is \$float {
		\$if b is f64 && c in [i8, i16] {
			return f64(a) + b + f64(c)
		}
	}
	return 15.0
}

fn unfollowed[A Number, B Number](a A, b B) f64 {
	\$if a is f64 && sizeof(B) == 8 {
		return a + f64(b) * 2.0
	}
	return 16.0
}

fn main() {
	println(all_f64(1.0, 2.0, 3.0))
	println(all_f64_types(1.0, 2.0, 3.0))
	println(in_lists(f32(1), u8(2)))
	println(not_in_lists(1, 2))
	println(either_f64(1, 2.0))
	println(either_in(1.0, 2.0))
	println(not_both_f64(1.0, 2.0))
	println(chain(f32(1), f32(2)))
	println(three_tests(i16(3)))
	println(grouped(4.0))
	println(negated(5))
	println(groups(6))
	println(local_and_param(User{'u', 1}, Admin{'a', 2}))
	println(listed_names(User{'v', 3}, User{'w', 4}))
	println(free_param(1.0, 2))
	println(nested(1.0, 2.0, i8(3)))
	println(unfollowed(1.0, 2.0))
}
"

// joined asks for the hover of the `nth` `word` of the line of joined_program
// that reads `text`.
fn joined(text string, word string, nth int) string {
	return ask(program_dir('joined', joined_program), 'hv^', line_of(joined_program, text), word,
		nth)
}

const all_numbers = 'i8 | i16 | int | u8 | f32 | f64'

fn test_values_tested_together_with_and_are_each_the_type_they_are_tested_for() {
	// `$if a is f64 && b is f64 && c is f64 {`: in its branch each of them is an
	// `f64`, and so is a local that adds them; after it, any type of `Number`.
	for word in ['a', 'b', 'c'] {
		assert joined('\t\ttotal := a + b + c', word, 0) == hover_of('${word} f64')
	}
	assert joined('\t\ttotal := a + b + c', 'total', 0) == hover_of('total f64')
	assert joined('\t\treturn total', 'total', 0) == hover_of('total f64')
	assert joined('\treturn f64(a) + f64(b) + f64(c)', 'a', 0) == hover_of('a A\\nA: ${all_numbers}')
	// The same tests on the type parameters, which are `f64` there too.
	sum := '\t\tsum := A(a) + B(b) + C(c)'
	assert joined(sum, 'A', 0) == hover_of('[A Number]\\nA: f64')
	assert joined(sum, 'C', 0) == hover_of('[C Number]\\nC: f64')
	assert joined(sum, 'b', 0) == hover_of('b f64')
	assert joined(sum, 'sum', 0) == hover_of('sum f64')
	// Its `$else` is where one of them is not: each can be any type.
	assert joined('\t\treturn f64(a) * f64(b) * f64(c)', 'b', 0) == hover_of('b B\\nB: ${all_numbers}')
}

fn test_values_tested_together_against_lists_keep_what_their_lists_leave() {
	// `in` keeps the types of its list, `!in` the others, for each value.
	assert joined('\t\treturn f64(a) + f64(b)', 'a', 0) == hover_of('a A\\nA: f32 | f64')
	assert joined('\t\treturn f64(a) + f64(b)', 'b', 0) == hover_of('b B\\nB: i8 | u8')
	assert joined('\t\treturn f64(a) / f64(b)', 'a', 0) == hover_of('a A\\nA: i8 | i16 | int | u8')
	assert joined('\t\treturn f64(a) / f64(b)', 'b', 0) == hover_of('b B\\nB: i16 | int | f32 | f64')
	// Their `$else`s: one value may be in its list, when the other is not.
	for line in ['\t\treturn f64(a) - f64(b)', '\t\treturn f64(a) - f64(b) - 1.0'] {
		assert joined(line, 'a', 0) == hover_of('a A\\nA: ${all_numbers}')
		assert joined(line, 'b', 0) == hover_of('b B\\nB: ${all_numbers}')
	}
}

fn test_values_tested_together_with_or_are_decided_in_the_else() {
	// `$if a is f64 || b is f64 {`: either may be an `f64`, so in its branch each
	// can be any type; in its `$else` neither is one.
	assert joined('\t\treturn f64(a) + f64(b) + 2.0', 'a', 0) == hover_of('a A\\nA: ${all_numbers}')
	assert joined('\t\treturn f64(a) + f64(b) + 2.0', 'b', 0) == hover_of('b B\\nB: ${all_numbers}')
	assert joined('\t\treturn f64(a) + f64(b) + 3.0', 'a', 0) == hover_of('a A\\nA: i8 | i16 | int | u8 | f32')
	assert joined('\t\treturn f64(a) + f64(b) + 3.0', 'b', 0) == hover_of('b B\\nB: i8 | i16 | int | u8 | f32')
	// `in` and `!in` joined with `||`: its `$else` is where `a` is not in its
	// list and `b` is in its.
	assert joined('\t\treturn f64(a) + f64(b) + 4.0', 'a', 0) == hover_of('a A\\nA: ${all_numbers}')
	assert joined('\t\treturn f64(a) + f64(b) + 5.0', 'a', 0) == hover_of('a A\\nA: i16 | int | f32 | f64')
	assert joined('\t\treturn f64(a) + f64(b) + 5.0', 'b', 0) == hover_of('b B\\nB: f32 | f64')
	// `!is` joined with `||`: its `$else` is where both are `f64`.
	assert joined('\t\treturn f64(a) + f64(b) + 6.0', 'b', 0) == hover_of('b B\\nB: ${all_numbers}')
	assert joined('\t\treturn a + b', 'a', 0) == hover_of('a f64')
	assert joined('\t\treturn a + b', 'b', 0) == hover_of('b f64')
}

fn test_each_branch_of_a_chain_of_tests_on_several_values_leaves_what_the_others_did_not_take() {
	assert joined('\t\treturn a * b', 'a', 0) == hover_of('a f64')
	assert joined('\t\treturn a * b', 'b', 0) == hover_of('b f64')
	assert joined('\t\treturn f64(a * b)', 'a', 0) == hover_of('a f32')
	assert joined('\t\treturn f64(a * b)', 'b', 0) == hover_of('b f32')
	// `$else $if a is f64 {`: `a` is an `f64`, so `b` is not, or the first
	// branch would have taken them.
	assert joined('\t\treturn a + f64(b)', 'a', 0) == hover_of('a f64')
	assert joined('\t\treturn a + f64(b)', 'b', 0) == hover_of('b B\\nB: i8 | i16 | int | u8 | f32')
	// The last `$else`: `a` is not an `f64`; `b` can be any type.
	last := '\t\treturn f64(a) * f64(b) * 7.0'
	assert joined(last, 'a', 0) == hover_of('a A\\nA: i8 | i16 | int | u8 | f32')
	assert joined(last, 'b', 0) == hover_of('b B\\nB: ${all_numbers}')
	// A value in the condition of a later test is what the tests before leave.
	assert joined('\t} \$else \$if a is f32 && b is f32 {', 'a', 0) == hover_of('a A\\nA: ${all_numbers}')
}

fn test_a_value_tested_three_times_in_parentheses_or_after_not_is_what_the_tests_leave() {
	assert joined('\t\treturn f64(x) + 8.0', 'x', 0) == hover_of('x T\\nT: i8 | i16 | u8')
	assert joined('\t\treturn y', 'y', 0) == hover_of('y f64')
	assert joined('\t\treturn f64(z) + 9.0', 'z', 0) == hover_of('z T\\nT: i8 | i16 | int | u8 | f32')
	assert joined('\t\treturn z', 'z', 0) == hover_of('z f64')
	// A group in a list, and `!in` in an `$else $if`.
	assert joined('\t\treturn f64(v) + 10.0', 'v', 0) == hover_of('v T\\nT: i8 | f32 | f64')
	assert joined('\t\treturn f64(v) + 11.0', 'v', 0) == hover_of('v T\\nT: i16 | u8')
	assert joined('\t\treturn f64(v) + 12.0', 'v', 0) == hover_of('v int')
}

fn test_values_of_interfaces_tested_together_are_the_types_they_are_tested_for() {
	// A local that holds a value of a type parameter, tested with a parameter.
	assert joined('\t\treturn x.age + b.level', 'x', 0) == hover_of('x main.User')
	assert joined('\t\treturn x.age + b.level', 'b', 0) == hover_of('b main.Admin')
	named := 'implements main.Named'
	assert joined('\treturn x.name.len + b.name.len', 'x', 0) == hover_of('x A\\nA: ${named}')
	assert joined('\treturn x.name.len + b.name.len', 'b', 0) == hover_of('b B\\nB: ${named}')
	// `in` keeps the types of its list; `!in` on an interface leaves any other
	// type that implements it, and its `$else` the types of its list.
	first := '\t\treturn a.name.len + b.name.len + 1'
	assert joined(first, 'a', 0) == hover_of('a A\\nA: main.User | main.Admin')
	assert joined(first, 'b', 0) == hover_of('b B\\nB: ${named}')
	second := '\t\treturn a.name.len + b.name.len + 2'
	assert joined(second, 'a', 0) == hover_of('a A\\nA: ${named}')
	assert joined(second, 'b', 0) == hover_of('b B\\nB: ${named}')
	// The last `$else`: `a` is in the list, so `b` is a `User`, or the first
	// branch would have taken them.
	third := '\t\treturn a.name.len + b.name.len + 3'
	assert joined(third, 'a', 0) == hover_of('a A\\nA: main.User | main.Admin')
	assert joined(third, 'b', 0) == hover_of('b main.User')
}

fn test_a_test_that_cannot_be_followed_leaves_the_others_their_types() {
	// A test that the editor cannot follow, `sizeof(B) == 8`, may go either way,
	// and the other tests still decide their values; one of a type parameter
	// without a constraint decides it too.
	assert joined('\t\treturn a + f64(u)', 'a', 0) == hover_of('a f64')
	assert joined('\t\treturn a + f64(u)', 'u', 0) == hover_of('u int')
	assert joined('\t\treturn a + f64(b) * 2.0', 'a', 0) == hover_of('a f64')
	assert joined('\t\treturn a + f64(b) * 2.0', 'b', 0) == hover_of('b B\\nB: ${all_numbers}')
	// Nested tests: the outer one on a type parameter, the inner one on two
	// values.
	inner := '\t\t\treturn f64(a) + b + f64(c)'
	assert joined(inner, 'a', 0) == hover_of('a A\\nA: f32 | f64')
	assert joined(inner, 'b', 0) == hover_of('b f64')
	assert joined(inner, 'c', 0) == hover_of('c C\\nC: i8 | i16')
}

const operations_program = "module main

type Signed = i8 | int | f64

type Whole = i8 | int

fn shifts[A Whole](a A) string {
	shifted := a << 2
	widened := 1 << a
	text := a.str() + '!'
	return '\${shifted} \${widened} \${text}'
}

fn operations[A Signed, B Signed](a A, b B) f64 {
	cast := A(a)
	twice := a + a
	casts := A(a) * A(a)
	mixed := f64(a) + f64(b)
	left := 2 * a
	grouped := (a + a) / a
	negative := -a
	compared := a < a
	both := compared && twice > a
	\$if A is f64 && B is f64 {
		joined := a + b
		return joined + twice + cast + casts + mixed + left + grouped + negative
	}
	return if both { 1.0 } else { 0.0 }
}

fn main() {
	println(operations(1.0, 2.0))
	println(shifts(3))
}
"

// operation asks for the hover of the first `word` of the line of
// operations_program that reads `text`.
fn operation(text string, word string) string {
	return ask(program_dir('operations', operations_program), 'hv^', line_of(operations_program,
		text), word, 0)
}

fn test_a_local_whose_value_is_an_operation_on_values_of_type_parameters_has_its_type() {
	// An operation on numbers has the type of its operands, the first one that
	// is not a literal; a comparison and a logical operation are a `bool`.
	signed := 'A: i8 | int | f64'
	assert operation('\tcast := A(a)', 'cast') == hover_of('cast A\\n${signed}')
	assert operation('\ttwice := a + a', 'twice') == hover_of('twice A\\n${signed}')
	assert operation('\tcasts := A(a) * A(a)', 'casts') == hover_of('casts A\\n${signed}')
	assert operation('\tmixed := f64(a) + f64(b)', 'mixed') == hover_of('mixed f64')
	assert operation('\tleft := 2 * a', 'left') == hover_of('left A\\n${signed}')
	assert operation('\tgrouped := (a + a) / a', 'grouped') == hover_of('grouped A\\n${signed}')
	assert operation('\tnegative := -a', 'negative') == hover_of('negative A\\n${signed}')
	assert operation('\tcompared := a < a', 'compared') == hover_of('compared bool')
	assert operation('\tboth := compared && twice > a', 'both') == hover_of('both bool')
	// In the branch of `$if A is f64 && B is f64 {` they are `f64`s.
	ret := '\t\treturn joined + twice + cast + casts + mixed + left + grouped + negative'
	assert operation('\t\tjoined := a + b', 'joined') == hover_of('joined f64')
	assert operation(ret, 'twice') == hover_of('twice f64')
	assert operation(ret, 'negative') == hover_of('negative f64')
	// A shift has the type of what it shifts, not of how far; `+` on a value of
	// no known type has the type of the other operand.
	assert operation('\tshifted := a << 2', 'shifted') == hover_of('shifted A\\nA: i8 | int')
	assert operation('\twidened := 1 << a', 'widened') == hover_of('widened int')
	assert operation("\ttext := a.str() + '!'", 'text') == hover_of('text string')
}

const joined_members_program = 'module main

interface Named {
	name string
}

struct User {
	name string
	age  int
}

struct Admin {
	name  string
	level int
}

type Numeric = int | i8 | f32 | f64

fn both[A Named, B Named](a A, b B) {
	\$if a is User && b is Admin {
		println(a.zz)
		println(b.zz)
	} \$else \$if a is User {
		println(b.yy)
	}
}

fn numbers[A Numeric, B Numeric](x A, y B) {
	\$if x in [f32, f64] && y !in [f32, f64] {
		println(x.zz)
		println(y.zz)
	}
}

fn main() {}
'

// joined_members lists the labels that completion offers after the `.` of the
// line of joined_members_program that reads `text`.
fn joined_members(text string) []string {
	dir := program_dir('joined_members', joined_members_program)
	dot := text.index('.') or { panic('`${text}` has no `.`') }
	answer := ask_at(dir, '${line_of(joined_members_program, text)}:${dot + 1}')
	assert answer != '', text
	return (json2.decode[Details](answer) or { panic('${err}: ${answer}') }).details.map(it.label)
}

fn test_values_tested_together_offer_what_their_tests_leave_them() {
	// `$if a is User && b is Admin {`: `a` offers the members of a `User`, `b`
	// those of an `Admin`. In `$else $if a is User {`, `b` is no `Admin`.
	assert joined_members('\t\tprintln(a.zz)') == ['age', 'name']
	assert joined_members('\t\tprintln(b.zz)') == ['level', 'name']
	assert joined_members('\t\tprintln(b.yy)') == ['name']
	// `in` and `!in`: `x` offers what `f32` and `f64` both have, `y` what `int`
	// and `i8` both have.
	assert joined_members('\t\tprintln(x.zz)') == ['eq_epsilon', 'str', 'strg', 'strlong', 'strsci']
	assert joined_members('\t\tprintln(y.zz)') == ['hex', 'hex_full', 'str']
}

const generic_forms_program = "module main

interface Named {
	name string
	greet() string
}

struct User {
	name string
	age  int
}

fn (u User) greet() string {
	return 'hi \${u.name}'
}

struct Box[T] {
	item T
}

fn (b Box[T]) get() T {
	return b.item
}

type Signed = i8 | int | f64

fn identity[T](x T) T {
	return x
}

fn wrap_list[T](x T) []T {
	return [x]
}

fn count_of[T](x T) int {
	_ = x
	return 1
}

fn find[T](x T) ?T {
	return x
}

fn first_of[T](xs []T) T {
	return xs[0]
}

fn sets[A Signed, B Signed](a A, b B) f64 {
	bigger := if a > a { a } else { a }
	label := if a > a { 'big' } else { 'small' }
	kind := match a {
		0 { 'zero' }
		else { 'other' }
	}
	picked := match a {
		0 { a }
		else { a }
	}
	values := [a, a]
	first := values[0]
	fixed := [a, a]!
	nested := [[a]]
	pairs := {
		'x': a
	}
	boxed := Box[A]{
		item: a
	}
	inferred := Box{
		item: a
	}
	inner := boxed.item
	got := boxed.get()
	converted := a.str()
	same := identity(a)
	twice_same := identity(identity(a))
	listed := wrap_list(a)
	count := count_of(a)
	head := first_of(values)
	head_typed := first_of[A](values)
	maybe := find(a) or { a }
	if found := find(a) {
		println(found)
	}
	for elem in values {
		println(elem)
	}
	dumped := dump(a)
	ref := &a
	ts := typeof(a).name
	doubled := values.map(it * 2)
	println(typeof(inferred).name)
	println('\${bigger} \${label} \${kind} \${picked} \${first} \${fixed} \${nested} \${pairs} \${inner} \${got}')
	println('\${converted} \${same} \${twice_same} \${listed} \${count} \${head} \${head_typed} \${maybe}')
	println('\${dumped} \${ref} \${ts} \${doubled} \${b}')
	return 0.0
}

fn named[T Named](x T) string {
	greeting := x.greet()
	boxed := Box[T]{
		item: x
	}
	held := boxed.item
	names := [x.name]
	return greeting + held.name + names.len.str()
}

fn loose[U](u U) string {
	copy := identity(u)
	\$if u is int {
		n := u + 1
		k := U(0)
		return n.str() + k.str() + u.hex()
	} \$else \$if U is string {
		return u
	} \$else \$if u in [i8, i16] {
		small := u
		return small.str()
	} \$else \$if u !in [f32, f64] {
		return 'other \${copy}'
	} \$else {
		float := u
		return float.str()
	}
}

fn plain_int(i int) string {
	return i.hex()
}

fn main() {
	println(sets(1, 2))
	println(named(User{'ana', 3}))
	println(loose(4))
	println(loose('x'))
	println(loose(i8(5)))
	println(loose(true))
	println(loose(1.5))
	println(plain_int(6))
}
"

// generic_form asks for the hover of the `nth` `word` of the line of
// generic_forms_program that reads `text`.
fn generic_form(text string, word string, nth int) string {
	return ask(program_dir('generic_forms', generic_forms_program), 'hv^', line_of(generic_forms_program,
		text), word, nth)
}

fn test_a_local_of_a_generic_body_has_the_type_the_check_gives_its_value() {
	// What a value of a type parameter makes of a local is written with the type
	// parameter, and what it can be on a line of its own: an `if` or a `match`
	// used as a value, an array, its element, a map, a generic struct and its
	// field, and what `or {}`, `dump()` and `&` give.
	signed := 'A: i8 | int | f64'
	for local, typ in {
		'\tbigger := if a > a { a } else { a }': 'A'
		'\tpicked := match a {':                 'A'
		'\tvalues := [a, a]':                    '[]A'
		'\tfirst := values[0]':                  'A'
		'\tfixed := [a, a]!':                    '[2]A'
		'\tnested := [[a]]':                     '[][]A'
		'\tpairs := {':                          'map[string]A'
		'\tboxed := Box[A]{':                    'Box[A]'
		'\tinferred := Box{':                    'Box[A]'
		'\tinner := boxed.item':                 'A'
		'\tmaybe := find(a) or { a }':           'A'
		'\tdumped := dump(a)':                   'A'
		'\tref := &a':                           '&A'
		'\tdoubled := values.map(it * 2)':       '[]A'
	} {
		word := local.trim_space().all_before(' ')
		assert generic_form(local, word, 0) == hover_of('${word} ${typ}\\n${signed}'), local
	}
	// What the type parameters do not decide has its own type.
	for local, typ in {
		"\tlabel := if a > a { 'big' } else { 'small' }": 'string'
		'\tkind := match a {':                            'string'
		'\tconverted := a.str()':                         'string'
		'\tts := typeof(a).name':                         'string'
	} {
		word := local.trim_space().all_before(' ')
		assert generic_form(local, word, 0) == hover_of('${word} ${typ}'), local
	}
	// With an interface as the constraint.
	named := 'T: implements main.Named'
	assert generic_form('\tboxed := Box[T]{', 'boxed', 0) == hover_of('boxed Box[T]\\n${named}')
	assert generic_form('\theld := boxed.item', 'held', 0) == hover_of('held T\\n${named}')
	assert generic_form('\tnames := [x.name]', 'names', 0) == hover_of('names []string')
	assert generic_form('\tgreeting := x.greet()', 'greeting', 0) == hover_of('greeting string')
}

fn test_a_generic_call_in_a_generic_body_returns_what_its_arguments_bind() {
	// `identity(a)` returns what `a` is, an `A`, not the `T` that `identity`
	// declares; so do the other generic calls, with their type arguments
	// written or not, and the methods of a generic struct.
	signed := 'A: i8 | int | f64'
	for local, typ in {
		'\tsame := identity(a)':                 'A'
		'\ttwice_same := identity(identity(a))': 'A'
		'\tlisted := wrap_list(a)':              '[]A'
		'\thead := first_of(values)':            'A'
		'\thead_typed := first_of[A](values)':   'A'
		'\tgot := boxed.get()':                  'A'
	} {
		word := local.trim_space().all_before(' ')
		assert generic_form(local, word, 0) == hover_of('${word} ${typ}\\n${signed}'), local
	}
	assert generic_form('\tcount := count_of(a)', 'count', 0) == hover_of('count int')
	// The guard of an `if` and the variable of a `for` loop.
	assert generic_form('\t\tprintln(found)', 'found', 0) == hover_of('found A\\n${signed}')
	assert generic_form('\t\tprintln(elem)', 'elem', 0) == hover_of('elem A\\n${signed}')
	// A type parameter without a constraint.
	assert generic_form('\tcopy := identity(u)', 'copy', 0) == hover_of('copy U')
}

fn test_a_type_parameter_without_a_constraint_is_what_a_compile_time_test_makes_it() {
	// `$if u is int {`: `u` is an `int` there, and so is a local that holds it;
	// `$else $if U is string {`, a `string`. `in` leaves the types of its list
	// and the `$else` of `!in` the types of its. What no test decides stays `U`.
	assert generic_form('\t\tn := u + 1', 'u', 0) == hover_of('u int')
	assert generic_form('\t\tn := u + 1', 'n', 0) == hover_of('n int')
	assert generic_form('\t\tk := U(0)', 'U', 0) == hover_of('[U]\\nU: int')
	assert generic_form('\t\treturn u', 'u', 0) == hover_of('u string')
	assert generic_form('\t\tsmall := u', 'small', 0) == hover_of('small U\\nU: i8 | i16')
	assert generic_form('\t\tfloat := u', 'u', 0) == hover_of('u U\\nU: f32 | f64')
	assert generic_form("\t\treturn 'other \${copy}'", 'copy', 0) == hover_of('copy U')
	assert generic_form('\tcopy := identity(u)', 'u', 0) == hover_of('u U')
	// Completion offers there what an `int` offers.
	dir := program_dir('generic_forms', generic_forms_program)
	branch := '\t\treturn n.str() + k.str() + u.hex()'
	plain := '\treturn i.hex()'
	branch_col := branch.index('u.hex') or { panic(branch) } + 2
	plain_col := plain.index('i.hex') or { panic(plain) } + 2
	offered := ask_at(dir, '${line_of(generic_forms_program, branch)}:${branch_col}')
	assert offered != ''
	assert offered == ask_at(dir, '${line_of(generic_forms_program, plain)}:${plain_col}')
}

const method_values_program = "module main

struct Base {}

fn (b Base) describe() string {
	return 'base'
}

struct Plain {
	Base
	name string
}

fn (p Plain) label() string {
	return p.name
}

struct Box[T] {
	item T
}

fn (b Box[T]) label() string {
	return 'box'
}

fn (b Box[T]) twice() string {
	again := b.label
	return again() + again()
}

type Shown = Plain

fn (s Shown) describe() string {
	return 'shown'
}

fn main() {
	p := Plain{
		name: 'p'
	}
	b := Box[int]{
		item: 1
	}
	plain_label := p.label
	box_label := b.label
	println(plain_label() + box_label() + b.twice())
	println(p.label() + p.describe() + b.label())
	plain_describe := p.describe
	shown := Shown(p)
	shown_describe := shown.describe
	println(plain_describe() + shown.describe() + shown_describe())
}
"

// method_value asks `code` about the `nth` `word` of the line of
// method_values_program that reads `text`.
fn method_value(code string, text string, word string, nth int) string {
	return ask(program_dir('method_values', method_values_program), code, line_of(method_values_program,
		text), word, nth)
}

fn test_a_method_named_without_a_call_is_the_method() {
	// `p.label` without a call is the method `label`, of a struct or of a
	// generic struct, as `p.label()` is: its hover and its declaration.
	for code in ['hv^', 'gd^'] {
		plain := method_value(code, '\tprintln(p.label() + p.describe() + b.label())', 'label', 0)
		boxed := method_value(code, '\tprintln(p.label() + p.describe() + b.label())', 'label', 1)
		assert plain != '' && boxed != ''
		if code == 'gd^' {
			assert plain != boxed
		}
		assert method_value(code, '\tplain_label := p.label', 'label', 0) == plain
		assert method_value(code, '\tbox_label := b.label', 'label', 0) == boxed
		assert method_value(code, '\tagain := b.label', 'label', 0) == boxed
	}
}

fn test_a_method_named_without_a_call_is_the_one_its_call_is() {
	// A method promoted from an embedded struct, `p.describe`, and the own method
	// of an alias of that struct, `shown.describe`: without a call, the method
	// that the call is, not the one of the aliased struct.
	for code in ['hv^', 'gd^'] {
		promoted := method_value(code, '\tprintln(p.label() + p.describe() + b.label())',
			'describe', 0)
		shown := method_value(code, '\tprintln(plain_describe() + shown.describe() + shown_describe())',
			'describe', 0)
		assert promoted != '' && shown != ''
		if code == 'gd^' {
			assert promoted != shown
		}
		assert method_value(code, '\tplain_describe := p.describe', 'describe', 0) == promoted
		assert method_value(code, '\tshown_describe := shown.describe', 'describe', 0) == shown
	}
}

const interface_methods_program = "module main

interface Comparable[T] {
	less(other T) bool
}

struct Num {
	v int
}

fn (a Num) less(b Num) bool {
	return a.v < b.v
}

fn smallest[T Comparable[T]](a T, b T) T {
	return if a.less(b) { a } else { b }
}

interface Named {
	name string
	greet() string
}

struct User {
	name string
}

fn (u User) greet() string {
	return 'hi \${u.name}'
}

interface Shelf[T Named] {
	get() T
}

struct UserShelf {
	u User
}

fn (s UserShelf) get() User {
	return s.u
}

fn take_now[T Named](s Shelf[T]) T {
	return s.get()
}

fn shelf_name(s Shelf[User]) string {
	return s.get().name
}

fn greeting(n Named) string {
	greeter := n.greet
	return n.greet() + greeter()
}

fn named_greeting[T Named](x T) string {
	return x.greet()
}

fn main() {
	println(smallest(Num{1}, Num{2}).v)
	shelf := UserShelf{User{'ana'}}
	println(take_now[User](shelf).name + shelf_name(shelf))
	println(greeting(User{'bo'}) + named_greeting(User{'cy'}))
}
"

// interface_method asks `code` about the `nth` `word` of the line of
// interface_methods_program that reads `text`.
fn interface_method(code string, text string, word string, nth int) string {
	return ask(program_dir('interface_methods', interface_methods_program), code, line_of(interface_methods_program,
		text), word, nth)
}

fn test_a_method_of_an_interface_is_its_member_generic_or_not() {
	// `a.less(b)` with `[T Comparable[T]]` calls the `less` of that interface,
	// `s.get()` the `get` of `Shelf[T]` or of `Shelf[User]`, with the type
	// arguments of the value; and so does a method named without a call.
	less := '\treturn if a.less(b) { a } else { b }'
	assert interface_method('hv^', less, 'less', 0) == hover_of('fn less(other T) bool')
	assert interface_method('gd^', less, 'less', 0) == 'main.v:${line_of(interface_methods_program, '\tless(other T) bool')}:1'
	get_at := 'main.v:${line_of(interface_methods_program, '\tget() T')}:1'
	assert interface_method('hv^', '\treturn s.get()', 'get', 0) == hover_of('fn get() T')
	assert interface_method('gd^', '\treturn s.get()', 'get', 0) == get_at
	assert interface_method('hv^', '\treturn s.get().name', 'get', 0) == hover_of('fn get() main.User')
	assert interface_method('gd^', '\treturn s.get().name', 'get', 0) == get_at
	greet_at := 'main.v:${line_of(interface_methods_program, '\tgreet() string')}:1'
	assert interface_method('hv^', '\tgreeter := n.greet', 'greet', 0) == hover_of('fn greet() string')
	assert interface_method('gd^', '\tgreeter := n.greet', 'greet', 0) == greet_at
	// As before: a method of an interface, called on a value of it or of a type
	// parameter that it constrains.
	assert interface_method('hv^', '\treturn n.greet() + greeter()', 'greet', 0) == hover_of('fn greet() string')
	assert interface_method('gd^', '\treturn x.greet()', 'greet', 0) == greet_at
}
