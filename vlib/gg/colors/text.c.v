module colors

import fontstash

const used_import = fontstash.used_import

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
