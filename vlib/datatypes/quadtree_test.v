module datatypes

fn test_create() {
	mut qt := Quadtree{}
	test := qt.create(0, 0, 1340, 640, 8, 4, 0)
	test_clone := qt.create(0, 0, 1340, 640, 8, 4, 0)
	assert test == test_clone
}

fn test_insert() {
	mut qt := Quadtree{}
	mut test := qt.create(0, 0, 1340, 640, 8, 4, 0)
	mut pt := AABB{
		x: 100
		y: 50
		width: 60
		height: 100
	}
	assert test.particles == []
	test.insert(pt)
	assert test.particles[0] == pt
}

fn test_retrieve() {
	mut qt := Quadtree{}
	mut test := qt.create(0, 0, 1340, 640, 8, 4, 0)
	mut pt := AABB{
		x: 100
		y: 50
		width: 60
		height: 100
	}
	test.insert(pt)
	t := test.retrieve(pt)
	assert t[0] == pt
}

fn test_clear() {
	mut qt := Quadtree{}
	mut test := qt.create(0, 0, 1340, 640, 8, 4, 0)
	mut test_clone := qt.create(0, 0, 1340, 640, 8, 4, 0)
	mut pt := AABB{
		x: 100
		y: 50
		width: 60
		height: 100
	}
	test.split()
	test.insert(pt)
	assert test != test_clone
	test.clear()
	assert test == test_clone
}

fn test_get_nodes() {
	mut qt := Quadtree{}
	mut test := qt.create(0, 0, 1340, 640, 8, 4, 0)
	test.split()
	t := test.get_nodes()
	assert t.len == 4
}

fn test_split() {
	mut qt := Quadtree{}
	mut test := qt.create(0, 0, 1340, 640, 8, 4, 0)
	test.split()
	t := test.get_nodes()
	assert t.len == 4
}

fn test_get_index() {
	mut qt := Quadtree{}
	mut test := qt.create(0, 0, 1340, 640, 8, 4, 0)
	mut pt := AABB{
		x: 100
		y: 50
		width: 60
		height: 100
	}
	test.particles << pt
	t := test.get_index(pt)
	assert t == [1]
}
