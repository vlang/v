struct FixedCallbackVertex {
	value f32
	// A one-dimensional array parameter decays to a pointer in C, so the
	// callback may refer to its own still-incomplete element struct.
	self_sum fn ([2]FixedCallbackVertex) f32 = unsafe { nil }
}

type FixedVertexSink = fn (voidptr, [4]FixedCallbackVertex)

type NestedVertexSink = fn ([2][3]FixedCallbackVertex) f32

// This holder has no inline vertex storage to provide the struct dependency.
struct AlphaFixedCallbackHolder {
	call NestedVertexSink = unsafe { nil }
}

struct FixedCallbackRenderer {
mut:
	sink     FixedVertexSink  = unsafe { nil }
	nested   NestedVertexSink = unsafe { nil }
	vertices [4]FixedCallbackVertex
}

@[heap]
struct FixedCallbackResult {
mut:
	total f32
}

fn fixed_callback_sum(context voidptr, vertices [4]FixedCallbackVertex) {
	mut result := unsafe { &FixedCallbackResult(context) }
	mut total := f32(0)
	for vertex in vertices { total += vertex.value }
	result.total = total
}

fn nested_callback_sum(vertices [2][3]FixedCallbackVertex) f32 {
	return vertices[0][0].value + vertices[1][2].value
}

fn test_fn_type_fixed_array_struct_parameter() {
	mut renderer := FixedCallbackRenderer{
		sink:     fixed_callback_sum
		nested:   nested_callback_sum
		vertices: [FixedCallbackVertex{ value: 1 }, FixedCallbackVertex{ value: 2 },
			FixedCallbackVertex{ value: 3 }, FixedCallbackVertex{ value: 4 }]!
	}
	mut result := &FixedCallbackResult{}
	renderer.sink(result, renderer.vertices)
	assert result.total == 10
	grid := [
		[FixedCallbackVertex{ value: 5 }, FixedCallbackVertex{}, FixedCallbackVertex{}]!,
		[FixedCallbackVertex{}, FixedCallbackVertex{}, FixedCallbackVertex{ value: 7 }]!,
	]!
	assert renderer.nested(grid) == 12
	holder := AlphaFixedCallbackHolder{ call: nested_callback_sum }
	assert holder.call(grid) == 12
	vertex := FixedCallbackVertex{
		self_sum: fn (values [2]FixedCallbackVertex) f32 {
			return values[0].value + values[1].value
		}
	}
	assert vertex.self_sum([FixedCallbackVertex{ value: 11 }, FixedCallbackVertex{ value: 13 }]!) == 24
}
