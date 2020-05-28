module gx

pub const (
	align_left  = 1
	align_right = 4
)

pub struct TextCfg {
pub:
	color     Color
	size      int
	align     int
	max_width int
	family    string
	bold      bool
	mono      bool
}
