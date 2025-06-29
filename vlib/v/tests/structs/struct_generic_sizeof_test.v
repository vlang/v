const cache_line_size = 32

struct PaddedSlot[T] {
mut:
	data T
	pad  [cache_line_size - sizeof(T)]u8
}

fn test_main() {
	x := PaddedSlot[int]{}
	assert '${x}' == 'PaddedSlot[int]{
    data: 0
    pad: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
}'
	x2 := PaddedSlot[u8]{}
	assert '${x2}' == 'PaddedSlot[u8]{
    data: 0
    pad: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
}'
}
