import os

const pointer_map_lvalue_vexe = @VEXE
const pointer_map_lvalue_tests_dir = os.dir(@FILE)
const pointer_map_lvalue_v3_dir = os.dir(pointer_map_lvalue_tests_dir)
const pointer_map_lvalue_vlib_dir = os.dir(pointer_map_lvalue_v3_dir)
const pointer_map_lvalue_v3_src = os.join_path(pointer_map_lvalue_v3_dir, 'v3.v')

fn test_pointer_map_index_is_preserved_in_multi_return_lvalue() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_pointer_map_lvalue_${os.getpid()}')
	src := os.join_path(os.temp_dir(), 'v3_pointer_map_lvalue_${os.getpid()}.v')
	bin := os.join_path(os.temp_dir(), 'v3_pointer_map_lvalue_program_${os.getpid()}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(bin) or {}
		os.rm(bin + '.c') or {}
	}
	build :=
		os.execute('${pointer_map_lvalue_vexe} -gc none -prealloc -path "${pointer_map_lvalue_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${pointer_map_lvalue_v3_src}')
	assert build.exit_code == 0, build.output

	os.write_file(src, "fn pair() ([]int, int) {
	return [7], 9
}

struct Inner {
mut:
	values []int
}

struct Item {
	Inner
}

struct NestedItem {
mut:
	inner Inner
}

fn update(m_ref &map[string][]int, ch chan []int) int {
	mut x := 0
	unsafe {
		m_ref['key'], x = pair()
		select {
			m_ref['key'] = <-ch {}
		}
		m_ref['key'] << 11
	}
	return x
}

fn main() {
	mut values := {
		'key': [1]
	}
	ch := chan []int{cap: 1}
	ch <- [13]
	x := update(&values, ch)
	mut items := map[string]Item{}
	items['key'].values << 17
	assert items['key'].values == [17]
	mut nested := map[string]NestedItem{}
	nested['key'].inner.values << 23
	assert nested['key'].inner.values == [23]
	println('\${values['key'][0] + values['key'][1]}:\${x}')
}
") or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -nocache -no-parallel -keepc -b c ${src} -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '24:9', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('map__get_or_set(m_ref,'), generated
	assert compact.contains('.Inner.values'), generated
	assert compact.contains('map__get_or_set(&nested,'), generated
	assert !compact.contains('(m_ref)[_str_'), generated
}
