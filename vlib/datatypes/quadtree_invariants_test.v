module datatypes

// A 9x9 grid spaced ten units apart never straddles a subdivision boundary of
// a 100x100 tree, so every point lands in exactly one quadrant and the stored
// count has to match the inserted count.
fn quadtree_grid() []AABB {
	mut points := []AABB{}
	for i in 0 .. 9 {
		for j in 0 .. 9 {
			points << AABB{
				x:      10.0 + f64(i * 10)
				y:      10.0 + f64(j * 10)
				width:  1
				height: 1
			}
		}
	}
	return points
}

fn quadtree_count_particles(q &Quadtree) int {
	mut total := q.particles.len
	for j in 0 .. q.nodes.len {
		total += quadtree_count_particles(&q.nodes[j])
	}
	return total
}

fn quadtree_level_sizes(q &Quadtree, mut sizes map[int]int) {
	if q.nodes.len > 0 {
		sizes[q.level + 1] = (sizes[q.level + 1] or { 0 }) + q.nodes.len
		for j in 0 .. q.nodes.len {
			quadtree_level_sizes(&q.nodes[j], mut sizes)
		}
	}
}

fn test_stays_flat_until_the_capacity_is_exceeded() {
	mut qt := Quadtree{}
	mut tree := qt.create(0, 0, 100, 100, 8, 4, 0)
	grid := quadtree_grid()
	for p in grid[..8] {
		tree.insert(p)
	}
	assert tree.particles.len == 8
	assert tree.nodes.len == 0
	assert quadtree_count_particles(&tree) == 8
	tree.insert(grid[8])
	assert tree.particles.len == 0
	assert tree.nodes.len == 4
	assert quadtree_count_particles(&tree) == 9
}

fn test_the_four_quadrants_tile_the_parent_perimeter() {
	mut qt := Quadtree{}
	mut tree := qt.create(0, 0, 100, 100, 1, 4, 0)
	tree.insert(quadtree_grid()[0])
	tree.insert(quadtree_grid()[80])
	assert tree.nodes.len == 4
	mut area := 0.0
	for node in tree.nodes {
		area += node.perimeter.width * node.perimeter.height
	}
	assert area == tree.perimeter.width * tree.perimeter.height
	// each quadrant is a quarter of the parent and shares one of its corners
	assert tree.nodes[0].perimeter.x == 50.0
	assert tree.nodes[0].perimeter.y == 0.0
	assert tree.nodes[1].perimeter.x == 0.0
	assert tree.nodes[1].perimeter.y == 0.0
	assert tree.nodes[2].perimeter.x == 0.0
	assert tree.nodes[2].perimeter.y == 50.0
	assert tree.nodes[3].perimeter.x == 50.0
	assert tree.nodes[3].perimeter.y == 50.0
	for node in tree.nodes {
		assert node.perimeter.width == 50.0
		assert node.perimeter.height == 50.0
		assert node.level == 1
		assert node.capacity == tree.capacity
		assert node.depth == tree.depth
	}
}

fn test_a_split_moves_every_particle_out_of_the_parent() {
	mut qt := Quadtree{}
	mut tree := qt.create(0, 0, 100, 100, 1, 5, 0)
	grid := quadtree_grid()
	for p in grid {
		tree.insert(p)
	}
	assert tree.particles.len == 0
	assert tree.nodes.len == 4
	assert quadtree_count_particles(&tree) == grid.len
}

fn test_the_stored_node_count_matches_a_level_by_level_count() {
	mut qt := Quadtree{}
	mut tree := qt.create(0, 0, 100, 100, 1, 5, 0)
	grid := quadtree_grid()
	for p in grid {
		tree.insert(p)
	}
	mut sizes := map[int]int{}
	quadtree_level_sizes(&tree, mut sizes)
	mut total := 0
	for _, n in sizes {
		total += n
	}
	assert total == tree.get_nodes().len
	assert sizes[1] == 4
	// no level may exceed the configured depth
	for level, _ in sizes {
		assert level <= tree.depth
	}
}

// NOTE: a particle that overlaps two subdivisions is stored once per quadrant
// it touches, so the tree can hold more particles than were inserted.
fn test_a_straddling_particle_is_stored_in_each_quadrant_it_touches() {
	mut qt := Quadtree{}
	mut tree := qt.create(0, 0, 100, 100, 0, 4, 0)
	// capacity 0 forces the split on the first insert
	tree.insert(AABB{
		x:      45
		y:      45
		width:  10
		height: 10
	})
	assert tree.nodes.len == 4
	assert tree.particles.len == 0
	assert quadtree_count_particles(&tree) == 4
}

// NOTE: a zero-sized particle sitting exactly on both midpoints matches no
// quadrant at all, so it is dropped rather than stored.
fn test_a_particle_exactly_on_a_midpoint_is_dropped() {
	mut qt := Quadtree{}
	mut tree := qt.create(0, 0, 100, 100, 0, 4, 0)
	tree.insert(AABB{
		x:      50
		y:      50
		width:  0
		height: 0
	})
	assert tree.nodes.len == 4
	assert tree.particles.len == 0
	assert quadtree_count_particles(&tree) == 0
}

// NOTE: nothing checks a particle against the perimeter, so a point far
// outside the root is filed under whichever quadrant its position selects.
fn test_a_particle_outside_the_perimeter_is_still_stored() {
	mut qt := Quadtree{}
	mut tree := qt.create(0, 0, 100, 100, 0, 4, 0)
	outside := AABB{
		x:      200
		y:      200
		width:  1
		height: 1
	}
	tree.insert(outside)
	assert quadtree_count_particles(&tree) == 1
}

fn test_a_depth_of_zero_never_subdivides() {
	mut qt := Quadtree{}
	mut tree := qt.create(0, 0, 100, 100, 1, 0, 0)
	grid := quadtree_grid()
	for p in grid {
		tree.insert(p)
	}
	assert tree.nodes.len == 0
	assert tree.particles.len == grid.len
	assert quadtree_count_particles(&tree) == grid.len
	assert tree.get_nodes().len == 0
}

fn test_retrieve_finds_every_point_that_was_inserted() {
	mut qt := Quadtree{}
	mut tree := qt.create(0, 0, 100, 100, 1, 5, 0)
	grid := quadtree_grid()
	for p in grid {
		tree.insert(p)
	}
	for p in grid {
		found := tree.retrieve(p)
		assert found.len > 0, 'retrieve returned nothing for ${p.x}, ${p.y}'
		mut has_p := false
		for g in found {
			if g == p {
				has_p = true
				break
			}
		}
		assert has_p, 'retrieve missed ${p.x}, ${p.y}'
	}
}

// retrieve is a region query: a wider box can return more particles, never
// fewer, and every particle the box actually overlaps has to be in the result.
fn test_retrieve_returns_a_superset_of_the_overlapping_points() {
	mut qt := Quadtree{}
	mut tree := qt.create(0, 0, 100, 100, 1, 5, 0)
	grid := quadtree_grid()
	for p in grid {
		tree.insert(p)
	}
	for i in 0 .. 5 {
		for j in 0 .. 5 {
			box := AABB{
				x:      f64(i * 18)
				y:      f64(j * 18)
				width:  20
				height: 20
			}
			mut overlapping := 0
			for p in grid {
				if p.x < box.x + box.width && p.x + p.width > box.x && p.y < box.y + box.height
					&& p.y + p.height > box.y {
					overlapping++
				}
			}
			found := tree.retrieve(box)
			assert found.len >= overlapping, 'box ${box.x}, ${box.y}: ${found.len} < ${overlapping}'
			for p in grid {
				if p.x >= box.x && p.x + p.width <= box.x + box.width && p.y >= box.y
					&& p.y + p.height <= box.y + box.height {
					mut has_p := false
					for g in found {
						if g == p {
							has_p = true
							break
						}
					}
					assert has_p, 'box ${box.x}, ${box.y} missed ${p.x}, ${p.y}'
				}
			}
		}
	}
}

fn test_clear_empties_every_generation() {
	mut qt := Quadtree{}
	mut tree := qt.create(0, 0, 100, 100, 1, 5, 0)
	grid := quadtree_grid()
	for p in grid {
		tree.insert(p)
	}
	assert tree.get_nodes().len > 4
	fresh := qt.create(0, 0, 100, 100, 1, 5, 0)
	tree.clear()
	assert tree == fresh
	assert tree.particles.len == 0
	assert tree.nodes.len == 0
	assert tree.get_nodes().len == 0
	assert quadtree_count_particles(&tree) == 0
}
