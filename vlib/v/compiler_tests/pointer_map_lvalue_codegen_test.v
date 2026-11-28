import os
import v.tempname

fn test_pointer_map_index_is_preserved_in_multi_return_lvalue() {
	root := os.join_path(os.vtmp_dir(), 'v3_pointer_map_lvalue_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_pointer_map_lvalue.v')
	bin := os.join_path(root, 'v3_pointer_map_lvalue_program')

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

struct MapState {
mut:
	values map[string]int
}

struct MapOwner {
mut:
	state MapState
}

fn (mut owner MapOwner) current_state() &MapState {
	return &owner.state
}

struct Link {
	value int
}

struct Holder {
	link &Link
}

fn (link &Link) read() int {
	return link.value
}

fn (holder &Holder) read_link() int {
	return holder.link.read()
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
	mut owner := MapOwner{}
	for i in 0 .. 100 {
		owner.current_state().values[i.str()] = i
	}
	assert owner.state.values.len == 100
	assert owner.state.values['99'] == 99
	link := &Link{
		value: 7
	}
	holder := Holder{
		link: link
	}
	assert holder.read_link() == 7
	println('\${values['key'][0] + values['key'][1]}:\${x}')
}
") or {
		panic(err)
	}
	compile := os.exec([@VEXE, '-nocache', '-no-parallel', '-keepc', '-b', 'c', src, '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '24:9', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('map__get_or_set(m_ref,'), generated
	assert compact.contains('.Inner.values'), generated
	assert compact.contains('map__get_or_set(&nested,'), generated
	assert !compact.contains('(m_ref)[_str_'), generated
}
