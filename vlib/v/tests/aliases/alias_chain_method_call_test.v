// A receiver typed with an alias of an alias (`type C = B`, `type B = A`) calls
// the methods declared on every type of the chain, like a one-level alias does.
// V3 only looked one level down and reported `unknown function: c.method`.
import alias_chain_style

struct Base {
mut:
	id int
}

fn (b Base) base_id() int {
	return b.id
}

fn (mut b Base) set_id(id int) {
	b.id = id
}

struct Cache {
mut:
	v int
}

struct Props {
	Base
mut:
	n     int
	cache &Cache = unsafe { nil }
}

type Values = Props
type MutValues = Values
type DeepValues = MutValues

fn (mut p Props) ensure() &Cache {
	if p.cache == unsafe { nil } {
		p.cache = &Cache{}
	}
	return p.cache
}

fn (p &Props) get() int {
	return p.n
}

fn (p Props) name() string {
	return 'props'
}

fn (p Props) props_only() string {
	return 'props ${p.n}'
}

fn (v Values) name() string {
	return 'values'
}

fn (v Values) mid() int {
	return v.n + 10
}

fn (mut v MutValues) set_n(n int) {
	v.n = n
}

fn modify(mut d DeepValues) {
	d.set_n(77)
	d.set_id(8)
	d.ensure().v = 9
}

fn test_methods_through_an_alias_pointer() {
	mut style := &Props{}
	mut values := unsafe { &MutValues(style) }
	values.ensure().v = 3
	values.set_n(4)
	assert values.get() == 4
	assert values.mid() == 14
	assert style.cache.v == 3
	assert style.n == 4
	assert values.n == 4
	assert values.cache.v == 3
}

fn test_methods_through_an_alias_value() {
	mut direct := MutValues{}
	direct.ensure().v = 5
	assert direct.cache.v == 5
	direct.set_n(6)
	assert direct.get() == 6
	assert direct.props_only() == 'props 6'
	// The nearest declaration of the chain wins.
	assert direct.name() == 'values'
}

fn test_methods_through_a_three_level_chain() {
	mut d := DeepValues{}
	d.set_n(2)
	assert d.get() == 2
	assert d.mid() == 12
	d.set_id(5)
	assert d.base_id() == 5
	assert d.id == 5
	modify(mut d)
	assert d.get() == 77
	assert d.base_id() == 8
	assert d.cache.v == 9
	mut p := &Props{}
	mut dp := unsafe { &DeepValues(p) }
	dp.set_n(11)
	dp.ensure().v = 12
	assert p.n == 11
	assert p.cache.v == 12
	assert dp.mid() == 21
	assert dp.name() == 'values'
}

type LocalValues = alias_chain_style.MutableValues
type LocalValues2 = LocalValues

fn test_methods_through_an_imported_chain() {
	mut v := alias_chain_style.MutableValues{
		width: 3
	}
	v.ensure_cache().direction = 42
	assert v.cache.direction == 42
	assert v.width_px() == 3
	assert v.doubled_width() == 6
	assert v.padded_width() == 103
	v.grow()
	assert v.width == 4
	mut props := &alias_chain_style.Properties{
		width: 6
	}
	mut pv := unsafe { &alias_chain_style.MutableValues(props) }
	pv.ensure_cache().direction = 7
	pv.grow()
	assert props.cache.direction == 7
	assert props.width == 7
	assert pv.doubled_width() == 14
}

fn test_methods_through_a_local_chain_of_an_imported_chain() {
	mut l := LocalValues2{
		width: 20
	}
	assert l.width_px() == 20
	assert l.padded_width() == 120
	l.grow()
	l.ensure_cache().direction = 1
	assert l.width == 21
	assert l.cache.direction == 1
}
