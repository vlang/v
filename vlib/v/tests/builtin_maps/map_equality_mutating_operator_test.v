// vtest vflags: -gc none -enable-globals

struct MapEqualityMutation {
	value int
}

__global (
	equality_left         map[string][]MapEqualityMutation
	equality_right        map[string][]MapEqualityMutation
	equality_key          string
	equality_delete_right bool
	equality_buffers      [][]u8
)

fn (left MapEqualityMutation) == (right MapEqualityMutation) bool {
	if equality_delete_right {
		equality_right.delete(equality_key)
	} else {
		equality_left.delete(equality_key)
	}
	// Reuse the deleted key's allocation before the outer equality loop hashes it again.
	for _ in 0 .. 128 {
		equality_buffers << []u8{len: equality_key.len + 1, init: u8(120)}
	}
	return left.value == right.value
}

fn test_map_equality_keeps_keys_alive_across_mutating_value_operators() {
	for delete_right in [false, true] {
		equality_key = 'map-equality-dynamic-key-${17}'
		equality_delete_right = delete_right
		equality_buffers = [][]u8{}
		equality_left = {
			equality_key: [MapEqualityMutation{17}]
		}
		equality_right = {
			equality_key: [MapEqualityMutation{17}]
		}
		assert (equality_left == equality_right) == !delete_right
		assert equality_buffers.len == 128
		assert equality_left.len == if delete_right { 1 } else { 0 }
		assert equality_right.len == if delete_right { 0 } else { 1 }
	}
}
