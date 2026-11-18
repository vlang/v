module main

@[sendable]
struct Point {
	x int
	y int
}

@[sendable]
struct Inner {
	v int
}

@[sendable]
struct Outer {
	inner Inner
	xs    []int
	m     map[string]int
	opt   ?int
}

fn add_points(a Point, b Point) int {
	return a.x + a.y + b.x + b.y
}

fn test_sendable_spawn() {
	h := spawn add_points(Point{ x: 1, y: 2 }, Point{ x: 3, y: 4 })
	assert h.wait() == 10
}

fn test_sendable_channel() {
	ch := chan Point{cap: 1}
	ch <- Point{ x: 5, y: 6 }
	p := <-ch
	assert p.x + p.y == 11
}

fn test_sendable_nested_collections() {
	o := Outer{
		inner: Inner{ v: 7 }
		xs:    [1, 2, 3]
		m:     {
			'a': 1
			'b': 2
		}
		opt:   9
	}
	h := spawn fn (o Outer) int {
		return o.inner.v + o.xs.len + o.m.len
	}(o)
	// inner.v=7, xs.len=3, m.len=2 => 12 (opt is not used here).
	assert h.wait() == 12
	ch := chan Outer{cap: 1}
	ch <- o
	got := <-ch
	assert got.inner.v == 7
	assert got.xs == [1, 2, 3]
	assert got.m['a'] == 1
	assert got.opt or { 0 } == 9
}

fn test_sendable_closure_capture() {
	p := Point{ x: 4, y: 5 }
	h := spawn fn [p] () int {
		return p.x * 2 + p.y
	}()
	assert h.wait() == 13
}
