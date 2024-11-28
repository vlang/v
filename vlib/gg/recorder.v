module gg

@[heap]
pub struct SSRecorderSettings {
pub mut:
	stop_at_frame     i64 = -1
	screenshot_frames []u64
	screenshot_folder string
	screenshot_prefix string
}
