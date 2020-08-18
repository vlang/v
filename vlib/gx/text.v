module gx

pub enum HorizontalAlign {
	left = C.FONS_ALIGN_LEFT
	center = C.FONS_ALIGN_CENTER
	right = C.FONS_ALIGN_RIGHT
}

pub enum VerticalAlign {
	top = C.FONS_ALIGN_TOP
	middle = C.FONS_ALIGN_MIDDLE
	bottom = C.FONS_ALIGN_BOTTOM
	baseline = C.FONS_ALIGN_BASELINE
}

pub struct TextCfg {
pub:
	color          Color = black
	size           int = 16
	align          HorizontalAlign = .left
	vertical_align VerticalAlign = .top
	max_width      int
	family         string
	bold           bool
	mono           bool
	italic         bool

}
