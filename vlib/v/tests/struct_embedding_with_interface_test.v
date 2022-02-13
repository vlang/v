fn test_struct_embedding_with_interface() {
	mut ll := LinearLayout{}
	mut lv := ListView{}
	ll.add(lv)
	ret := ll.layout()

	println(ret)
	assert ret.count('ListView') == 2
}

interface Container {
mut:
	layout() string
}

interface Layoutable {
	get_pos() (int, int)
mut:
	set_pos(int, int)
}

pub struct LayouterBase {
mut:
	layoutables []Layoutable
}

pub fn (mut lb LayouterBase) add(layoutable Layoutable) {
	lb.layoutables << layoutable
}

pub fn (lb LayouterBase) get_pos() (int, int) {
	return 0, 0
}

pub fn (mut lb LayouterBase) set_pos(x int, y int) {}

pub struct LinearLayout {
	LayouterBase
}

pub fn (mut ll LinearLayout) layout() string {
	mut output := ''
	for mut l in ll.layoutables {
		dump(l.type_name())
		output += '$l.type_name()\n'
		if l is Container {
			dump(l.type_name())
			output += '$l.type_name()\n'
		}
	}
	return output
}

pub struct ListView {
	LayouterBase
}

pub fn (mut lv ListView) layout() string {
	return ''
}
