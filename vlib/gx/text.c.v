module gx

import fontstash as _

@[deprecated: 'use gg.HorizontalAlign instead']
@[deprecated_after: '2026-01-24']
pub enum HorizontalAlign {
	left   = C.FONS_ALIGN_LEFT
	center = C.FONS_ALIGN_CENTER
	right  = C.FONS_ALIGN_RIGHT
}

@[deprecated: 'use gg.VerticalAlign instead']
@[deprecated_after: '2026-01-24']
pub enum VerticalAlign {
	top      = C.FONS_ALIGN_TOP
	middle   = C.FONS_ALIGN_MIDDLE
	bottom   = C.FONS_ALIGN_BOTTOM
	baseline = C.FONS_ALIGN_BASELINE
}
