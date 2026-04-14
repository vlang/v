@[deprecated(msg: 'module gx is deprecated, use `import gg` instead', after: '2026-01-24')]
module gx

import gg

// Color represents a 32 bit color value in sRGB format

pub type Color = gg.Color

@[deprecated(msg: 'use gg.black instead', after: '2026-01-24')]
pub const black = gg.black

@[deprecated(msg: 'use gg.gray instead', after: '2026-01-24')]
pub const gray = gg.gray

@[deprecated(msg: 'use gg.white instead', after: '2026-01-24')]
pub const white = gg.white

@[deprecated(msg: 'use gg.red instead', after: '2026-01-24')]
pub const red = gg.red

@[deprecated(msg: 'use gg.green instead', after: '2026-01-24')]
pub const green = gg.green

@[deprecated(msg: 'use gg.blue instead', after: '2026-01-24')]
pub const blue = gg.blue

@[deprecated(msg: 'use gg.yellow instead', after: '2026-01-24')]
pub const yellow = gg.yellow

@[deprecated(msg: 'use gg.magenta instead', after: '2026-01-24')]
pub const magenta = gg.magenta

@[deprecated(msg: 'use gg.cyan instead', after: '2026-01-24')]
pub const cyan = gg.cyan

@[deprecated(msg: 'use gg.orange instead', after: '2026-01-24')]
pub const orange = gg.orange

@[deprecated(msg: 'use gg.purple instead', after: '2026-01-24')]
pub const purple = gg.purple

@[deprecated(msg: 'use gg.indigo instead', after: '2026-01-24')]
pub const indigo = gg.indigo

@[deprecated(msg: 'use gg.pink instead', after: '2026-01-24')]
pub const pink = gg.pink

@[deprecated(msg: 'use gg.violet instead', after: '2026-01-24')]
pub const violet = gg.violet

@[deprecated(msg: 'use gg.dark_blue instead', after: '2026-01-24')]
pub const dark_blue = gg.dark_blue

@[deprecated(msg: 'use gg.dark_gray instead', after: '2026-01-24')]
pub const dark_gray = gg.dark_gray

@[deprecated(msg: 'use gg.dark_green instead', after: '2026-01-24')]
pub const dark_green = gg.dark_green

@[deprecated(msg: 'use gg.dark_red instead', after: '2026-01-24')]
pub const dark_red = gg.dark_red

@[deprecated(msg: 'use gg.light_blue instead', after: '2026-01-24')]
pub const light_blue = gg.light_blue

@[deprecated(msg: 'use gg.light_gray instead', after: '2026-01-24')]
pub const light_gray = gg.light_gray

@[deprecated(msg: 'use gg.light_green instead', after: '2026-01-24')]
pub const light_green = gg.light_green

@[deprecated(msg: 'use gg.light_red instead', after: '2026-01-24')]
pub const light_red = gg.light_red

// hex takes in a 32 bit integer and splits it into 4 byte values
@[deprecated(msg: 'use gg.hex instead', after: '2026-01-24')]
pub fn hex(color i32) Color {
	return gg.hex(color)
}

// rgb builds a Color instance from given r, g, b values
@[deprecated(msg: 'use gg.rgb instead', after: '2026-01-24')]
pub fn rgb(r u8, g u8, b u8) Color {
	return gg.rgb(r, g, b)
}

// rgba builds a Color instance from given r, g, b, a values
@[deprecated(msg: 'use gg.rgba instead', after: '2026-01-24')]
pub fn rgba(r u8, g u8, b u8, a u8) Color {
	return gg.rgba(r, g, b, a)
}

// color_from_string returns a Color, corresponding to the given string
// or black Color if string is not found in lookup table, or a hex color if starting with #
@[deprecated(msg: 'use gg.color_from_string instead', after: '2026-01-24')]
pub fn color_from_string(s string) Color {
	return gg.color_from_string(s)
}
