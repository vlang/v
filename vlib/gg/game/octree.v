module game

// Octree is a simple spatial index for axis-aligned boxes. Items are stored
// by integer id; a box that straddles child boundaries stays in the parent.
@[heap]
pub struct Octree {
pub mut:
	bounds   AABB
	capacity int = 8
	depth    int
	items    []int
	boxes    []AABB
	children []&Octree
}

// new_octree builds an empty tree covering bounds.
pub fn new_octree(bounds AABB, capacity int) &Octree {
	return &Octree{
		bounds:   bounds
		capacity: if capacity < 1 { 1 } else { capacity }
	}
}

fn child_bounds(b AABB, index int) AABB {
	half := (b.max - b.min).scale(0.5)
	center := (b.min + b.max).scale(0.5)
	mut lo := center
	mut hi := center
	if index & 1 != 0 {
		lo.x += half.x
		hi.x += half.x
	} else {
		hi.x -= half.x
	}
	if index & 2 != 0 {
		lo.y += half.y
		hi.y += half.y
	} else {
		hi.y -= half.y
	}
	if index & 4 != 0 {
		lo.z += half.z
		hi.z += half.z
	} else {
		hi.z -= half.z
	}
	return AABB{ min: lo, max: hi }
}

fn contains_box(outer AABB, inner AABB) bool {
	return inner.min.x >= outer.min.x && inner.max.x <= outer.max.x
		&& inner.min.y >= outer.min.y && inner.max.y <= outer.max.y
		&& inner.min.z >= outer.min.z && inner.max.z <= outer.max.z
}

fn split_octree(mut o Octree) {
	if o.children.len > 0 || o.depth >= 6 {
		return
	}
	o.children = []&Octree{cap: 8}
	for i in 0 .. 8 {
		o.children << new_octree(child_bounds(o.bounds, i), o.capacity)
	}
	o.children[0].depth = o.depth + 1
}

// insert adds an id with its bounding box. It returns false when the box
// lies completely outside the tree.
pub fn (mut o Octree) insert(id int, box AABB) bool {
	if !aabb_overlap(o.bounds, box) {
		return false
	}
	if o.children.len > 0 {
		for i in 0 .. 8 {
			if contains_box(o.children[i].bounds, box) {
				return o.children[i].insert(id, box)
			}
		}
	}
	o.items << id
	o.boxes << box
	if o.items.len > o.capacity && o.depth < 6 {
		if o.children.len == 0 {
			split_octree(mut o)
		}
		mut kept := []int{}
		mut kept_boxes := []AABB{}
		for i, item in o.items {
			mut placed := false
			for c in o.children {
				if contains_box(c.bounds, o.boxes[i]) {
					c.insert(item, o.boxes[i])
					placed = true
					break
				}
			}
			if !placed {
				kept << item
				kept_boxes << o.boxes[i]
			}
		}
		o.items = kept
		o.boxes = kept_boxes
	}
	return true
}

// query appends the ids whose boxes overlap the given box.
pub fn (o &Octree) query(box AABB, mut out []int) {
	if !aabb_overlap(o.bounds, box) {
		return
	}
	for i, item_box in o.boxes {
		if aabb_overlap(item_box, box) {
			out << o.items[i]
		}
	}
	for child in o.children {
		child.query(box, mut out)
	}
}

// count returns the number of ids stored in this node only.
pub fn (o &Octree) count() int {
	return o.items.len
}
