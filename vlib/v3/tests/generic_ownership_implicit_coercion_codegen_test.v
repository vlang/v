import os

const generic_coercion_vexe = @VEXE
const generic_coercion_tests_dir = os.dir(@FILE)
const generic_coercion_v3_dir = os.dir(generic_coercion_tests_dir)
const generic_coercion_vlib_dir = os.dir(generic_coercion_v3_dir)
const generic_coercion_v3_src = os.join_path(generic_coercion_v3_dir, 'v3.v')

fn test_generic_ownership_implicit_reference_coercions() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_coercion_${pid}')
	source := os.join_path(os.temp_dir(), 'v3_generic_coercion_input_${pid}.v')
	output := os.join_path(os.temp_dir(), 'v3_generic_coercion_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(source) or {}
		os.rm(output) or {}
		os.rm(output + '.c') or {}
	}
	build :=
		os.execute('${generic_coercion_vexe} -gc none -d ownership -path "${generic_coercion_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${generic_coercion_v3_src}')
	assert build.exit_code == 0, build.output
	os.write_file(source, "interface Matcher {
\tfind() int
}

struct MatcherRef {}

fn (MatcherRef) find() int {
\treturn 1
}

struct Searcher {}

struct Path {
\tvalue string
}

fn (mut searcher Searcher) search(matcher &Matcher, path &Path) int {
\treturn matcher.find() + path.value.len
}

struct ColorSpec {
\tvalue int
}

struct Colors {
\tmatched ColorSpec
}

fn (colors &Colors) matched() &ColorSpec {
\treturn &colors.matched
}

fn set_color(spec ColorSpec) int {
\treturn spec.value
}

fn run[T](mut path Path, colors &Colors) int {
\tmut searcher := Searcher{}
\tmatcher := MatcherRef{}
\treturn searcher.search(matcher, &path) + set_color(colors.matched())
}

fn main() {
\tmut path := Path{value: 'abc'}
\tcolors := Colors{
\t\tmatched: ColorSpec{value: 2}
\t}
\tassert run[int](mut path, &colors) == 6
\tprintln('ok')
}
") or {
		panic(err)
	}
	compile :=
		os.execute('${v3_bin} -ownership -d ownership -nocache -no-parallel -o ${output} ${source}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(output)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
