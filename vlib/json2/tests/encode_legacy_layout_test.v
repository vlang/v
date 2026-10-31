import json2

// The expected strings are the output of `encode_pretty` in the removed cJSON based
// `json` module for the same values.

struct LayoutInner {
	a int
	b []int
}

struct LayoutEmpty {}

struct LayoutOuter {
	name   string
	inner  LayoutInner
	list   []LayoutInner
	empty  LayoutEmpty
	m      map[string]int
	em     map[string]int
	nested [][]int
	objs   map[string]LayoutInner
	strs   []string
	f      f64
}

fn layout_values() LayoutOuter {
	return LayoutOuter{
		name:   'x\ty'
		inner:  LayoutInner{1, [1, 2]}
		list:   [LayoutInner{2, []}, LayoutInner{3, [4]}]
		m:      {
			'k': 1
		}
		nested: [[1, 2], []int{}, [3]]
		objs:   {
			'o': LayoutInner{6, [7]}
		}
		strs:   ['a', 'b']
		f:      1.5
	}
}

fn test_legacy_layout() {
	assert json2.encode(layout_values(), prettify: true, legacy_layout: true) == '{\n\t"name":\t"x\\ty",\n\t"inner":\t{\n\t\t"a":\t1,\n\t\t"b":\t[1, 2]\n\t},\n\t"list":\t[{\n\t\t\t"a":\t2,\n\t\t\t"b":\t[]\n\t\t}, {\n\t\t\t"a":\t3,\n\t\t\t"b":\t[4]\n\t\t}],\n\t"empty":\t{\n\t},\n\t"m":\t{\n\t\t"k":\t1\n\t},\n\t"em":\t{\n\t},\n\t"nested":\t[[1, 2], [], [3]],\n\t"objs":\t{\n\t\t"o":\t{\n\t\t\t"a":\t6,\n\t\t\t"b":\t[7]\n\t\t}\n\t},\n\t"strs":\t["a", "b"],\n\t"f":\t1.5\n}'
	assert json2.encode(LayoutEmpty{}, prettify: true, legacy_layout: true) == '{\n}'
	assert json2.encode([[LayoutInner{}]], prettify: true, legacy_layout: true) == '[[{\n\t\t\t"a":\t0,\n\t\t\t"b":\t[]\n\t\t}]]'
	assert json2.encode([1, 2]!, prettify: true, legacy_layout: true) == '[1, 2]'
}

fn test_legacy_layout_needs_prettify() {
	assert json2.encode(layout_values(), legacy_layout: true) == json2.encode(layout_values())
}
