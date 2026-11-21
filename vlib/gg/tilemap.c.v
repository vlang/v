module gg

// draw_tilemap draws every nonzero tile as a filled square. The tile value
// indexes colors starting at 1; values without a matching color use white.
pub fn (m &TileMap) draw(ctx &Context, colors []Color) {
	for row in 0 .. m.rows {
		for col in 0 .. m.cols {
			tile := m.at(col, row)
			if tile <= 0 {
				continue
			}
			c := if tile - 1 < colors.len {
				colors[tile - 1]
			} else {
				white
			}
			ctx.draw_rect_filled(f32(col) * m.tile_size, f32(row) * m.tile_size, m.tile_size,
				m.tile_size, c)
		}
	}
}
