import os
import v.cmdexec
import v.tempname

fn test_generic_ownership_implicit_reference_coercions() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_generic_coercion_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_generic_coercion_input.v')
	output := os.join_path(root, 'v3_generic_coercion_program')
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
		cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-nocache',
			'-no-parallel', '-o', output, source], '', {
			'VFLAGS':               ''
			'VTOOLS_CACHE_VERBOSE': ''
		})
	assert compile.exit_code == 0, compile.output
	run := os.exec([output])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
