// vtest build: !musl? // gx now transitively imports sokol, and that needs GL/gl.h, which is not installed on the musl CIs
import gg

const left = gg.HorizontalAlign.left

fn test_main() {
	align := left
	assert dump(align.str()) == 'left'
	assert dump(gg.HorizontalAlign.left) == gg.HorizontalAlign.left
	assert gg.HorizontalAlign.left.str() == 'left'
}
