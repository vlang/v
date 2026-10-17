// A copy of an immutable struct shares its map fields, but once such a field is
// replaced with a clone, writing its entries no longer reaches the original.
enum RangeKind {
	length
	number
}

struct AcceptedRange {
	min f64
	max f64
}

struct RangeContext {
mut:
	ranges map[RangeKind]AcceptedRange
}

fn widened_range_count(ctx RangeContext, widen bool) int {
	if widen {
		mut copy := ctx
		copy.ranges = ctx.ranges.clone()
		copy.ranges[.length] = AcceptedRange{
			min: 0
			max: 10
		}
		return copy.ranges.len
	}
	return ctx.ranges.len
}

fn widened_in_loop(ctx RangeContext) int {
	mut total := 0
	mut copy := ctx
	for i in 0 .. 2 {
		copy.ranges = ctx.ranges.clone()
		copy.ranges[.length] = AcceptedRange{
			max: f64(i)
		}
		total += copy.ranges.len
		copy = ctx
	}
	return total
}

fn widened_from_own_field(ctx RangeContext) int {
	mut copy := ctx
	copy.ranges = copy.ranges.clone()
	copy.ranges[.length] = AcceptedRange{
		max: 3
	}
	return copy.ranges.len
}

fn test_entry_write_to_cloned_map_field_of_copy() {
	mut ctx := RangeContext{}
	ctx.ranges[.number] = AcceptedRange{
		max: 1
	}
	assert widened_range_count(ctx, true) == 2
	assert widened_range_count(ctx, false) == 1
	assert widened_in_loop(ctx) == 4
	assert widened_from_own_field(ctx) == 2
	assert ctx.ranges.len == 1
	assert RangeKind.length !in ctx.ranges
}
