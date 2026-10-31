// vtest vflags: -enable-globals
// The default value of an enum is its first member, wherever V fills in a default:
// fixed array elements, struct fields, the elements of `[]T{len: n}` and the value
// of a missing map key.
import os

enum Mode {
	ready = 3
	busy
}

enum Zero {
	a
	b
}

enum Small as u8 {
	one = 1
	two
}

@[flag]
enum Perm {
	read
	write
}

struct Item {
	mode Mode
	n    int
}

struct WithFixed {
	modes  [2]Mode
	nested [2][2]Mode
	zeros  [2]Zero
}

struct Outer {
	item  Item
	modes [2]Mode
}

__global global_modes [2]Mode
__global global_nested [2][2]Mode
__global global_item Item
__global global_items [2]Item
__global global_with_fixed WithFixed
__global global_map map[string]Mode

fn test_fixed_arrays() {
	modes := [2]Mode{}
	assert modes == [Mode.ready, .ready]!
	nested := [2][2]Mode{}
	assert nested[1] == [Mode.ready, .ready]!
	mut changed := [3]Mode{}
	changed[1] = .busy
	assert changed == [Mode.ready, .busy, .ready]!
	items := [2]Item{}
	assert items[1].mode == .ready
	arrays := [][2]Mode{len: 2}
	assert arrays[1] == [Mode.ready, .ready]!
}

fn test_fixed_array_struct_fields() {
	w := WithFixed{}
	assert w.modes == [Mode.ready, .ready]!
	assert w.nested[1] == [Mode.ready, .ready]!
	assert w.zeros == [Zero.a, .a]!
	o := Outer{}
	assert o.item.mode == .ready
	assert o.modes == [Mode.ready, .ready]!
	set := WithFixed{
		modes: [Mode.busy, .busy]!
	}
	assert set.modes == [Mode.busy, .busy]!
	assert set.nested[0] == [Mode.ready, .ready]!
}

fn test_dynamic_arrays_of_structs() {
	items := []Item{len: 2}
	assert items[0].mode == .ready
	assert items[1].mode == .ready
	outers := []Outer{len: 1}
	assert outers[0].item.mode == .ready
	assert outers[0].modes == [Mode.ready, .ready]!
	with_fixed := []WithFixed{len: 1, cap: 4}
	assert with_fixed[0].nested[1] == [Mode.ready, .ready]!
}

fn mode_of(m map[string]Mode, key string) Mode {
	return m[key]
}

fn test_missing_map_keys() {
	m := map[string]Mode{}
	assert m['missing'] == .ready
	value := m['missing']
	assert value == .ready
	assert '${m['missing']}' == 'ready'
	assert mode_of(m, 'missing') == .ready
	mut set := map[string]Mode{}
	set['present'] = .busy
	assert set['present'] == .busy
	assert set['missing'] == .ready
	nested := map[string]map[string]Mode{}
	assert nested['a']['b'] == .ready
	items := map[string]Item{}
	assert items['missing'].mode == .ready
	fixed := map[string][2]Mode{}
	assert fixed['missing'] == [Mode.ready, .ready]!
	assert m['missing'] or { Mode.busy } == .busy
}

fn test_globals() {
	assert global_modes == [Mode.ready, .ready]!
	assert global_nested[1] == [Mode.ready, .ready]!
	assert global_item.mode == .ready
	assert global_items[1].mode == .ready
	assert global_with_fixed.modes == [Mode.ready, .ready]!
	assert global_with_fixed.nested[1] == [Mode.ready, .ready]!
	assert global_map['missing'] == .ready
}

fn test_backed_and_module_enums() {
	small := [2]Small{}
	assert small == [Small.one, .one]!
	small_map := map[string]Small{}
	assert small_map['missing'] == .one
	signals := [2]os.Signal{}
	assert signals == [os.Signal.hup, .hup]!
	signal_map := map[string]os.Signal{}
	assert signal_map['missing'] == .hup
}

fn test_zero_and_flag_enums_keep_zero() {
	zeros := [2]Zero{}
	assert zeros == [Zero.a, .a]!
	zero_map := map[string]Zero{}
	assert zero_map['missing'] == .a
	perms := [2]Perm{}
	assert int(perms[0]) == 0
	perm_map := map[string]Perm{}
	assert int(perm_map['missing']) == 0
}
