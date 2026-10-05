@[generated]
module main

import os
import zbrgen

// https://github.com/vlang/v/issues/29413
const _zbr_c_Base = 40
const zbrLimit = 2

struct _zbr_ty_Point {
mut:
	xPos int
	_y   int
}

fn _zbr_ty_Point.new(xPos int) _zbr_ty_Point {
	return _zbr_ty_Point{
		xPos: xPos
		_y:   1
	}
}

fn (p _zbr_ty_Point) sumAll() int {
	return p.xPos + p._y
}

fn (mut p _zbr_ty_Point) moveBy(dx int) {
	p.xPos += dx
}

struct _zbr_ty_Box[T] {
	value T
}

struct _zbr_ty_Line {
	_zbr_ty_Point
	endPoint _zbr_ty_Point
}

enum _zbr_ty_Color {
	red
	greenLight
}

type _zbr_ty_Meters = int

type _zbr_ty_Shape = _zbr_ty_Point | _zbr_ty_Color | string

interface _zbr_ty_Summer {
	sumAll() int
}

struct _zbr_ty_Holder {
	inner struct {
		xPos int
	}
}

fn camelCase(n int) int {
	return n + 1
}

fn _zbr_fn_helper(myArg int, _other int) int {
	return myArg + _other
}

fn _zbr_fn_kind(s _zbr_ty_Shape) string {
	return match s {
		_zbr_ty_Point { 'point' }
		_zbr_ty_Color { 'color' }
		string { 'string' }
	}
}

fn _zbr_fn_maybe(ok bool) ?_zbr_ty_Point {
	if ok {
		return _zbr_ty_Point{
			xPos: 9
		}
	}
	return none
}

fn test_functions_variables_and_consts() {
	_x := 3
	myValue := camelCase(_x)
	assert myValue == 4
	assert _zbr_fn_helper(myValue, _x) == 7
	assert _zbr_c_Base + zbrLimit == 42
}

fn test_struct_literals_and_methods() {
	p := _zbr_ty_Point{
		xPos: 3
	}
	assert p.xPos == 3
	assert p._y == 0
	mut q := _zbr_ty_Point.new(4)
	q.moveBy(2)
	assert q.sumAll() == 7
	r := &_zbr_ty_Point{
		xPos: 5
	}
	assert r.xPos == 5
	points := [_zbr_ty_Point{
		xPos: 1
	}, _zbr_ty_Point.new(2)]
	assert points.len == 2
	assert points[1].sumAll() == 3
	empty := []_zbr_ty_Point{len: 2}
	assert empty[1].xPos == 0
	mut by_name := map[string]_zbr_ty_Point{}
	by_name['p'] = p
	assert by_name['p'].xPos == 3
	box := _zbr_ty_Box[int]{
		value: 42
	}
	assert box.value == 42
	line := _zbr_ty_Line{
		_zbr_ty_Point: _zbr_ty_Point{
			xPos: 1
		}
		endPoint:      _zbr_ty_Point{
			xPos: 8
		}
	}
	assert line.xPos == 1
	assert line.endPoint.xPos == 8
	if found := _zbr_fn_maybe(true) {
		assert found.xPos == 9
	} else {
		assert false
	}
	assert sizeof(_zbr_ty_Point) == 2 * sizeof(int)
}

fn test_enums_aliases_sum_types_and_interfaces() {
	c := _zbr_ty_Color.greenLight
	assert c == .greenLight
	assert '${c}' == 'greenLight'
	m := _zbr_ty_Meters(12)
	assert int(m) == 12
	s := _zbr_ty_Shape(_zbr_ty_Point.new(1))
	assert s is _zbr_ty_Point
	assert _zbr_fn_kind(s) == 'point'
	assert _zbr_fn_kind(_zbr_ty_Color.red) == 'color'
	assert _zbr_fn_kind('x') == 'string'
	p := s as _zbr_ty_Point
	assert p.xPos == 1
	summer := _zbr_ty_Summer(_zbr_ty_Point.new(5))
	assert summer.sumAll() == 6
}

fn test_types_of_an_imported_generated_module() {
	v := zbrgen._zbr_ty_Vec{
		xPos:  1
		_yPos: 2
	}
	assert v.scaledSum() == 30
	assert zbrgen._zbr_ty_Vec.make(3, 4).scaledSum() == 70
	assert zbrgen._zbr_fn_origin().xPos == 0
	mode := zbrgen._zbr_ty_Mode.fastMode
	assert mode == .fastMode
	value := zbrgen._zbr_ty_Value(v)
	assert value is zbrgen._zbr_ty_Vec
	assert zbrgen._zbr_fn_describe(value) == 'vec 1'
	assert zbrgen._zbr_fn_describe(zbrgen._zbr_ty_Value(mode)) == 'mode fastMode'
	assert zbrgen._zbr_c_Scale == 10
}

fn test_values_of_imported_modules() {
	// `mod.value.member` is not an enum value, even though types can be lowercase here.
	assert os.args.len > 0
	assert zbrgen.zbrNames.len == 2
	assert zbrgen._zbr_c_Origin.xPos == 2
	assert zbrgen._zbr_c_Origin.scaledSum() == 50
}

fn test_anonymous_structs() {
	h := _zbr_ty_Holder{}
	assert h.inner.xPos == 0
}
