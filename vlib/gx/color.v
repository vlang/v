@[deprecated: 'module gx is deprecated, use `import gg` instead']
@[deprecated_after: '2026-01-24']
module gx

import gg

// Color represents a 32 bit color value in sRGB format

pub type Color = gg.Color

@[deprecated: 'use gg.black instead']
@[deprecated_after: '2026-01-24']
pub const black = gg.black

@[deprecated: 'use gg.gray instead']
@[deprecated_after: '2026-01-24']
pub const gray = gg.gray

@[deprecated: 'use gg.white instead']
@[deprecated_after: '2026-01-24']
pub const white = gg.white

@[deprecated: 'use gg.red instead']
@[deprecated_after: '2026-01-24']
pub const red = gg.red

@[deprecated: 'use gg.green instead']
@[deprecated_after: '2026-01-24']
pub const green = gg.green

@[deprecated: 'use gg.blue instead']
@[deprecated_after: '2026-01-24']
pub const blue = gg.blue

@[deprecated: 'use gg.yellow instead']
@[deprecated_after: '2026-01-24']
pub const yellow = gg.yellow

@[deprecated: 'use gg.magenta instead']
@[deprecated_after: '2026-01-24']
pub const magenta = gg.magenta

@[deprecated: 'use gg.cyan instead']
@[deprecated_after: '2026-01-24']
pub const cyan = gg.cyan

@[deprecated: 'use gg.orange instead']
@[deprecated_after: '2026-01-24']
pub const orange = gg.orange

@[deprecated: 'use gg.purple instead']
@[deprecated_after: '2026-01-24']
pub const purple = gg.purple

@[deprecated: 'use gg.indigo instead']
@[deprecated_after: '2026-01-24']
pub const indigo = gg.indigo

@[deprecated: 'use gg.pink instead']
@[deprecated_after: '2026-01-24']
pub const pink = gg.pink

@[deprecated: 'use gg.violet instead']
@[deprecated_after: '2026-01-24']
pub const violet = gg.violet

@[deprecated: 'use gg.dark_blue instead']
@[deprecated_after: '2026-01-24']
pub const dark_blue = gg.dark_blue

@[deprecated: 'use gg.dark_gray instead']
@[deprecated_after: '2026-01-24']
pub const dark_gray = gg.dark_gray

@[deprecated: 'use gg.dark_green instead']
@[deprecated_after: '2026-01-24']
pub const dark_green = gg.dark_green

@[deprecated: 'use gg.dark_red instead']
@[deprecated_after: '2026-01-24']
pub const dark_red = gg.dark_red

@[deprecated: 'use gg.light_blue instead']
@[deprecated_after: '2026-01-24']
pub const light_blue = gg.light_blue

@[deprecated: 'use gg.light_gray instead']
@[deprecated_after: '2026-01-24']
pub const light_gray = gg.light_gray

@[deprecated: 'use gg.light_green instead']
@[deprecated_after: '2026-01-24']
pub const light_green = gg.light_green

@[deprecated: 'use gg.light_red instead']
@[deprecated_after: '2026-01-24']
pub const light_red = gg.light_red

// hex takes in a 32 bit integer and splits it into 4 byte values
@[deprecated: 'use gg.hex instead']
@[deprecated_after: '2026-01-24']
pub fn hex(color i32) Color {
	return gg.hex(color)
}

// rgb builds a Color instance from given r, g, b values
@[deprecated: 'use gg.rgb instead']
@[deprecated_after: '2026-01-24']
pub fn rgb(r u8, g u8, b u8) Color {
	return gg.rgb(r, g, b)
}

// rgba builds a Color instance from given r, g, b, a values
@[deprecated: 'use gg.rgba instead']
@[deprecated_after: '2026-01-24']
pub fn rgba(r u8, g u8, b u8, a u8) Color {
	return gg.rgba(r, g, b, a)
}

// color_from_string returns a Color, corresponding to the given string
// or black Color if string is not found in lookup table, or a hex color if starting with #
@[deprecated: 'use gg.color_from_string instead']
@[deprecated_after: '2026-01-24']
pub fn color_from_string(s string) Color {
	return gg.color_from_string(s)
}
