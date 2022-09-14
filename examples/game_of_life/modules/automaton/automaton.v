module automaton

// ///////////////////////////////////////////////////////////
pub struct A2D {
pub mut:
	maxx int
	maxy int
	data &int
}

fn new_a2d(maxx int, maxy int) &A2D {
	size := int(sizeof(int)) * (maxx * maxy)
	return &A2D{
		maxx: maxx
		maxy: maxy
		data: unsafe { &int(vcalloc(size)) }
	}
}

[inline]
pub fn (a &A2D) set(x int, y int, newval int) {
	unsafe {
		mut e := &int(0)
		e = a.data + y * a.maxx + x
		*e = newval
	}
}

[inline]
pub fn (a &A2D) get(x int, y int) int {
	unsafe {
		mut e := &int(0)
		e = a.data + y * a.maxx + x
		_ = e
		return *e
	}
}

[inline]
pub fn (a &A2D) clear() {
	for y := 0; y < a.maxy; y++ {
		for x := 0; x < a.maxx; x++ {
			a.set(x, y, 0)
		}
	}
}

// ///////////////////////////////////////////////////////////
pub struct Automaton {
pub mut:
	field     &A2D = unsafe { nil }
	new_field &A2D = unsafe { nil }
}

fn new_automaton(ftext string) Automaton {
	f := ftext.split('\n').map(it.trim_space()).filter(it.len > 0)
	maxy := f.len
	mut maxx := 0
	for y := 0; y < f.len; y++ {
		if maxx < f[y].len {
			maxx = f[y].len
		}
	}
	field := new_a2d(maxx, maxy)
	new_field := new_a2d(maxx, maxy)
	for y in 0 .. field.maxy {
		for x in 0 .. field.maxx {
			val := if x < f[y].len && f[y][x] == `#` { 1 } else { 0 }
			field.set(x, y, val)
		}
	}
	return Automaton{
		field: field
		new_field: new_field
	}
}

pub fn (mut aa Automaton) update() {
	aa.new_field.clear()
	for y := 1; y < aa.field.maxy; y++ {
		for x := 1; x < aa.field.maxx; x++ {
			moore_sum := (0 + aa.field.get(x - 1, y - 1) + aa.field.get(x, y - 1) + aa.field.get(x +
				1, y - 1) + aa.field.get(x - 1, y) + 0 + aa.field.get(x + 1, y) +
				aa.field.get(x - 1, y + 1) + aa.field.get(x, y + 1) + aa.field.get(x + 1, y + 1))
			cell := aa.field.get(x, y)
			v := if cell == 1 { moore_sum in [2, 3] } else { moore_sum == 3 }
			aa.new_field.set(x, y, if v { 1 } else { 0 })
		}
	}
	tmp := aa.field
	aa.field = aa.new_field
	aa.new_field = tmp
}

pub fn gun() Automaton {
	field := '
*******************************************
*                                         *
*  A shooting gun:                        *
*                          #              *
*                        # #              *
*              ##      ##            ##   *
*             #   #    ##            ##   *
*  ##        #     #   ##                 *
*  ##        #   # ##    # #              *
*            #     #       #              *
*             #   #                       *
*              ##                         *
*                                         *
*  Tetris Life:                           *
*                                         *
*  ##       ####                          *
*  ##                                     *
*                                         *
*                                         *
*                                         *
*  #         ##                           *
*  ###      ##                            *
*                                         *
*                                         *
*                                         *
*        #                                *
*       ###                               *
*                                         *
*                                         *
*                                         *
*                                         *
*******************************************
'
	return new_automaton(field)
}
