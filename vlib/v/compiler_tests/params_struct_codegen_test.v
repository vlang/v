import os
import v.tempname

fn params_struct_run_good(name string, source string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_params_struct_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_${name}')
	src := bin + '.v'
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed')

	run := os.exec([bin])
	assert run.exit_code == 0
	return run.output.trim_space()
}

// A `@[params]` struct argument can be supplied as trailing `key: value` call args.
// Regression: the args used to be dropped, generating `make(1, )` (empty arg, trailing comma).
fn test_direct_params_struct_calls_codegen() {
	source := '@[params]
struct Cfg {
	a int
	b string
	c bool
}

fn make(x int, cfg Cfg) string {
	return \'\${x}|\${cfg.a}|\${cfg.b}|\${cfg.c}\'
}

@[params]
struct PoolConfig {
	max_open int
}

fn open_pool(name string, cfg PoolConfig) string {
	return \'\${name}|\${cfg.max_open}\'
}

@[params]
struct Config {
	host   string
	port   int
	family int
}

fn choose_family(host string) !int {
	return if host.contains(":") { 6 } else { 4 }
}

fn run(prefix string, config Config) string {
	return "\${prefix}|\${config.host}|\${config.port}|\${config.family}"
}

fn main() {
	println(make(1, a: 10, b: \'hi\', c: true))
	println(make(2, b: \'yo\'))
	println(make(3, Cfg{a: 5, b: \'z\', c: true}))

	// The f64 conversion exercises the trailing params argument to strconv.atof64.
	// Numeric accuracy is covered by the strconv tests.
	println(\'\${\'3.0\'.f64() > 0.0}\')
	println(\'\${\'2.0\'.f32() > 0.0}\')

	println(open_pool(\'db\'))
	println(open_pool(\'db\', max_open: 4))

	host := "::1"
	println(run("listen", host: host, port: 8787, family: choose_family(host) or { 0 }))
}
'
	out := params_struct_run_good('direct_params_struct_calls', source)
	assert out == '1|10|hi|true\n2|0|yo|false\n3|5|z|true\ntrue\ntrue\ndb|0\ndb|4\nlisten|::1|8787|6'
}

fn test_non_veb_reflected_calls_synthesize_omitted_trailing_args() {
	source := '@[params]
struct Config {
	count int
}

struct Value {}

fn (Value) run(arg ?string) string {
	return arg or { "none" }
}

fn (Value) configure(config Config) string {
	return int_str(config.count)
}

fn invoke[T](value T) {
	$for method in T.methods {
		if method.name in ["run", "configure"] {
			println(value.$method())
		}
	}
}

fn main() {
	invoke(Value{})
}
'
	out := params_struct_run_good('non_veb_reflected_omitted_args', source)
	assert out == 'none\n0'
}

fn test_params_fields_belong_to_params_struct() {
	dir := os.join_path(os.vtmp_dir(), 'v3_params_field_owner_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	project_dir := os.join_path(dir, 'v3_params_field_owner_project')
	os.mkdir_all(os.join_path(project_dir, 'fixture')) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'main.v'), "module main\n\nimport fixture\n\nstruct CliOptions {\n\truntime_profile string\n}\n\nfn main() {\n\topts := CliOptions{runtime_profile: 'node'}\n\tctx := &fixture.Context{}\n\tprintln(fixture.compile(ctx, 'entry', runtime_profile: opts.runtime_profile, retries: 3))\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(project_dir, 'fixture', 'fixture.v'), "module fixture\n\npub struct Context {\npub:\n\tretries string\nmut:\n\truntime_profile string\n}\n\n@[params]\npub struct Options {\npub:\n\truntime_profile string\n\tretries         int\n}\n\npub fn compile(ctx &Context, path string, options Options) string {\n\t_ = ctx\n\treturn '\${path}:\${options.runtime_profile}:\${options.retries}'\n}\n") or {
		panic(err)
	}
	bin := os.join_path(dir, 'app')
	compile := os.exec([@VEXE, os.join_path(project_dir, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'entry:node:3'
}

fn test_interface_and_fixed_array_fields_in_struct_initializers() {
	source := 'interface Reader {
	read(mut buf []u8) !int
}

struct Conn {}

fn (c Conn) read(mut buf []u8) !int {
	return 0
}

struct Config {
	reader Reader
	cap int
}

fn make(cfg Config) int {
	return cfg.cap
}

interface Widget {
	kind() string
}

struct TextBox {}
struct Label {}
struct Button {}

fn (TextBox) kind() string { return "textbox" }
fn (Label) kind() string { return "label" }
fn (Button) kind() string { return "button" }

@[params]
struct RowConfig {
	children []Widget
}

fn row(config RowConfig) string {
	mut kinds := []string{}
	for child in config.children {
		kinds << child.kind()
	}
	return kinds.join(",")
}

struct Item {
	x int
}

struct Header {
mut:
	data [2]Item
	cur_pos int
}

struct Uniforms {
	lights [2][4]f32
}

fn light_row() [4]f32 {
	return [f32(1.1), 1.2, 1.3, 1.4]!
}

fn check_fixed_array_fields() {
	mut h := Header{}
	h.data[0] = Item{x: 4}
	h.data[1] = Item{x: 9}
	h.cur_pos = 2
	combined := Header{
		data: h.data
		cur_pos: h.cur_pos
	}
	println("\${combined.cur_pos}|\${combined.data[0].x}|\${combined.data[1].x}")
	mixed := Uniforms{
		lights: [
			light_row(),
			[f32(2.1), 2.2]!,
		]!
	}
	assert mixed.lights[0][0] == f32(1.1)
	assert mixed.lights[1][0] == f32(2.1)
	assert mixed.lights[1][1] == f32(2.2)
	assert mixed.lights[1][2] == 0
	assert mixed.lights[1][3] == 0
}

fn main() {
	println(make(reader: Conn{}, cap: 8))
	println(row(children: [TextBox{}, Label{}, Button{}]))
	check_fixed_array_fields()
}
'
	out := params_struct_run_good('params_interface_and_fixed_array_fields', source)
	assert out.split_into_lines() == ['8', 'textbox,label,button', '2|4|9'], out
}

fn test_collapsed_struct_call_feature_matrix() {
	source := 'enum Mode {
	fast
	slow
}

struct Embedded {
	embedded int
}

struct Plain {
	Embedded
	n int
}

type PlainAlias = Plain

fn default_mode() Mode {
	return .slow
}

@[params]
struct Params {
	n    int
	mode Mode = default_mode()
}

type ParamsAlias = Params

struct GenericParams[T] {
	value T
}

struct Builder {
	prefix string
}

fn plain(value Plain) string {
	return "plain:\${value.n}:\${value.embedded}"
}

fn alias_plain(value PlainAlias) string {
	return "alias:\${value.n}"
}

fn params(prefix string, value Params) string {
	return "\${prefix}:\${value.n}:\${value.mode}"
}

fn alias_params(value ParamsAlias) string {
	return "alias-params:\${value.n}:\${value.mode}"
}

fn pointer_params(value &Params) string {
	return "pointer:\${value.n}:\${value.mode}"
}

fn generic_params[T](value GenericParams[T]) T {
	return value.value
}

fn (builder Builder) make(value Plain) string {
	return builder.prefix + int_str(value.n)
}

fn total(values ...Plain) int {
	return values[0].n + values[0].embedded
}

fn call_with(callback fn (Plain) string) string {
	return callback(n: 9)
}

fn main() {
	println(plain(n: 1, embedded: 11))
	println(alias_plain(n: 2))
	println(params("params", n: 3, mode: .fast))
	println(alias_params(n: 4))
	println(alias_params())
	println(pointer_params(n: 5, mode: .fast))
	println(pointer_params())
	println(int_str(generic_params[int](value: 6)))
	println(int_str(generic_params(value: 7)))
	println(Builder{prefix: "method:"}.make(n: 8))
	println(int_str(total(n: 10, embedded: 20)))
	println(call_with(plain))
}
'
	out := params_struct_run_good('collapsed_struct_call_feature_matrix', source)
	assert out == 'plain:1:11\nalias:2\nparams:3:fast\nalias-params:4:slow\nalias-params:0:slow\npointer:5:fast\npointer:0:slow\n6\n7\nmethod:8\n30\nplain:9:0'
}
