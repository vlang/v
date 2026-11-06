module canvas

// Draw ordering of Skia Graphite's Device/DrawList: painter's depth,
// compressed painter's order from a bounds manager, the disjoint stencil
// index and the sort keys that decide the execution and batching order.

// kUnassigned DisjointStencilIndex and the reserved first values.
const stencil_unassigned = u16(0xffff)
const no_intersection = u16(0)

struct DrawOrder {
mut:
	paint_order   u16 = no_intersection
	stencil_index u16 = stencil_unassigned
	depth         u16
}

fn (mut o DrawOrder) depends_on_paint_order(previous u16) {
	next := previous + 1
	if o.paint_order < next {
		o.paint_order = next
	}
}

fn (mut o DrawOrder) reverse_depth_as_stencil() {
	o.stencil_index = stencil_unassigned - o.depth
}

// rects_intersect mirrors Rect::intersects(ComplementRect): strict on all edges.
fn rects_intersect(a Rect, b Rect) bool {
	return a.vals[0] < -b.vals[2] && a.vals[1] < -b.vals[3] && a.vals[2] < -b.vals[0]
		&& a.vals[3] < -b.vals[1]
}

// BoundsManager mirrors Graphite's HybridBoundsManager with the Device's
// tuning: brute force for 64 draws, then a grid of 16px cells (at most 32x32).
struct BoundsManager {
	device_width  int
	device_height int
mut:
	rects  []Rect
	orders []u16
	// The grid, once more than 64 draws were recorded.
	grid        []u16
	grid_width  int
	grid_height int
	scale_x     f32
	scale_y     f32
}

const grid_cell_size = 16
const max_brute_force_n = 64
const max_grid_size = 32

fn new_bounds_manager(width int, height int) BoundsManager {
	return BoundsManager{
		device_width:  width
		device_height: height
	}
}

fn (b &BoundsManager) most_recent_draw(bounds Rect) u16 {
	if b.grid.len > 0 {
		l, t, r, bottom := b.grid_coords(bounds)
		mut max := u16(0)
		for y in t .. bottom + 1 {
			for x in l .. r + 1 {
				v := b.grid[y * b.grid_width + x]
				if v > max {
					max = v
				}
			}
		}
		return max
	}
	mut max := u16(0)
	for i, r in b.rects {
		if rects_intersect(r, bounds) && max < b.orders[i] {
			max = b.orders[i]
		}
	}
	return max
}

fn (mut b BoundsManager) record_draw(bounds Rect, order u16) {
	if b.grid.len == 0 && b.rects.len >= max_brute_force_n {
		b.make_grid()
		for i, r in b.rects {
			b.record_grid(r, b.orders[i])
		}
		b.rects.clear()
		b.orders.clear()
	}
	if b.grid.len > 0 {
		b.record_grid(bounds, order)
		return
	}
	b.rects << bounds
	b.orders << order
}

// make_grid mirrors GridBoundsManager::MakeRes(deviceSize, 16, 32).
fn (mut b BoundsManager) make_grid() {
	mut width := b.device_width
	mut height := b.device_height
	mut grid_width := int(C.ceilf(f32(width) / f32(grid_cell_size)))
	if grid_width > max_grid_size {
		grid_width = max_grid_size
	} else {
		width = grid_width * grid_cell_size
	}
	mut grid_height := int(C.ceilf(f32(height) / f32(grid_cell_size)))
	if grid_height > max_grid_size {
		grid_height = max_grid_size
	} else {
		height = grid_height * grid_cell_size
	}
	b.grid_width = grid_width
	b.grid_height = grid_height
	b.scale_x = f32(grid_width) / f32(width)
	b.scale_y = f32(grid_height) / f32(height)
	b.grid = []u16{len: grid_width * grid_height}
}

fn (b &BoundsManager) grid_coords(bounds Rect) (int, int, int, int) {
	// pin(cast<int32_t>(ltrb * scale), 0, gridDim - 1)
	l := clamp_int(int(bounds.left() * b.scale_x), 0, b.grid_width - 1)
	t := clamp_int(int(bounds.top() * b.scale_y), 0, b.grid_height - 1)
	r := clamp_int(int(bounds.right() * b.scale_x), 0, b.grid_width - 1)
	bottom := clamp_int(int(bounds.bottom() * b.scale_y), 0, b.grid_height - 1)
	return l, t, r, bottom
}

fn (mut b BoundsManager) record_grid(bounds Rect, order u16) {
	l, t, r, bottom := b.grid_coords(bounds)
	for y in t .. bottom + 1 {
		for x in l .. r + 1 {
			if order > b.grid[y * b.grid_width + x] {
				b.grid[y * b.grid_width + x] = order
			}
		}
	}
}

@[inline]
fn clamp_int(v int, lo int, hi int) int {
	return if v < lo {
		lo
	} else if v > hi {
		hi
	} else {
		v
	}
}

fn C.ceilf(x f32) f32
