import gg

const left = gg.HorizontalAlign.left

fn test_main() {
	align := left
	assert dump(align.str()) == 'left'
	assert dump(gg.HorizontalAlign.left) == gg.HorizontalAlign.left
	assert gg.HorizontalAlign.left.str() == 'left'
}
