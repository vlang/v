module progress

import math
import strings

// Style draws the bar graphic itself (not the percent, rate or ETA).
//
// `draw` must write `start`, then exactly `cells` terminal cells, then `end`.
// Implement it to make your own look; `fraction` is in 0.0..=1.0.
pub interface Style {
	draw(mut sb strings.Builder, fraction f64, cells int)
}

// ClassicStyle renders ` [=====>    ]`.
pub struct ClassicStyle {
pub mut:
	start string = ' ['
	end   string = ']'
	fill  rune   = `=`
	head  rune   = `>`
	empty rune   = ` `
}

// draw implements Style.
pub fn (s ClassicStyle) draw(mut sb strings.Builder, fraction f64, cells int) {
	sb.write_string(s.start)
	if cells > 0 {
		n := math.min(int(math.clamp(fraction, 0.0, 1.0) * f64(cells)), cells)
		sb.write_repeated_rune(s.fill, n)
		if n < cells {
			sb.write_rune(if s.head == 0 { s.empty } else { s.head })
			sb.write_repeated_rune(s.empty, cells - n - 1)
		}
	}
	sb.write_string(s.end)
}

// Partial block glyphs for 1/8 .. 7/8 of a cell (U+258F .. U+2589).
const block_partials = [`▏`, `▎`, `▍`, `▌`, `▋`, `▊`, `▉`]!

// BlockStyle renders `▕█████▌    ▏` with 1/8-cell resolution, so even a short
// bar moves smoothly. `fill` should be a full-block glyph.
pub struct BlockStyle {
pub mut:
	start string = '▕'
	end   string = '▏'
	fill  rune   = `█` // U+2588
	empty rune   = ` `
}

// draw implements Style.
pub fn (s BlockStyle) draw(mut sb strings.Builder, fraction f64, cells int) {
	sb.write_string(s.start)
	if cells > 0 {
		eighths := math.min(int(math.clamp(fraction, 0.0, 1.0) * f64(cells * 8) + 1e-9), cells * 8)
		full := eighths / 8
		rem := eighths % 8
		sb.write_repeated_rune(s.fill, full)
		if full < cells {
			sb.write_rune(if rem == 0 { s.empty } else { block_partials[rem - 1] })
			sb.write_repeated_rune(s.empty, cells - full - 1)
		}
	}
	sb.write_string(s.end)
}
