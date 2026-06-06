const fixed_array_expr_hi = [u8(1), 2, 3, 4]!
const fixed_array_expr_lo = [u8(5), 6, 7, 8]!

struct FixedArrayExprMesh {
mut:
	highlight [4]u8
}

fn fixed_array_expr_set_if(mut mesh FixedArrayExprMesh, highlighted bool) {
	mesh.highlight = if highlighted { fixed_array_expr_hi } else { fixed_array_expr_lo }
}

fn fixed_array_expr_set_match(mut mesh FixedArrayExprMesh, highlighted bool) {
	mesh.highlight = match highlighted {
		true { fixed_array_expr_hi }
		false { fixed_array_expr_lo }
	}
}

fn test_fixed_array_field_assign_from_if_expr() {
	mut mesh := FixedArrayExprMesh{}

	fixed_array_expr_set_if(mut mesh, true)
	assert mesh.highlight == fixed_array_expr_hi

	fixed_array_expr_set_if(mut mesh, false)
	assert mesh.highlight == fixed_array_expr_lo
}

fn test_fixed_array_field_assign_from_match_expr() {
	mut mesh := FixedArrayExprMesh{}

	fixed_array_expr_set_match(mut mesh, true)
	assert mesh.highlight == fixed_array_expr_hi

	fixed_array_expr_set_match(mut mesh, false)
	assert mesh.highlight == fixed_array_expr_lo
}
