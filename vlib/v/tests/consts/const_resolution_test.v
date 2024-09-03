import gx

const left = gx.align_left

fn test_main() {
	align := left
	assert dump(align.str()) == 'left'
	assert dump(gx.align_left) == gx.align_left
	assert gx.align_left.str() == 'left'
}
