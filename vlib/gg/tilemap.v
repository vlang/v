// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// TileMap stores a grid of integer tiles. Tile 0 is treated as empty; any
// other value is solid and selects a color in draw_tilemap.
pub struct TileMap {
pub mut:
	tiles     []int
	cols      int
	rows      int
	tile_size f32 = 32
}

// new_tilemap builds a map from a row-major tile array.
pub fn new_tilemap(tiles []int, cols int, rows int, tile_size f32) TileMap {
	return TileMap{ tiles: tiles, cols: cols, rows: rows, tile_size: tile_size }
}

// at returns the tile at a cell, or -1 when the cell is outside the map.
pub fn (m &TileMap) at(col int, row int) int {
	if col < 0 || row < 0 || col >= m.cols || row >= m.rows {
		return -1
	}
	idx := row * m.cols + col
	if idx < 0 || idx >= m.tiles.len {
		return -1
	}
	return m.tiles[idx]
}

// set changes one cell if it is inside the map.
pub fn (mut m TileMap) set(col int, row int, value int) {
	if col < 0 || row < 0 || col >= m.cols || row >= m.rows {
		return
	}
	idx := row * m.cols + col
	if idx >= 0 && idx < m.tiles.len {
		m.tiles[idx] = value
	}
}

// is_solid reports whether a cell contains a nonzero tile.
pub fn (m &TileMap) is_solid(col int, row int) bool {
	return m.at(col, row) > 0
}

// collides_rect reports whether any solid tile intersects r.
pub fn (m &TileMap) collides_rect(r Rect) bool {
	if m.tile_size <= 0 {
		return false
	}
	c0 := int(math.floor(f64(r.x) / f64(m.tile_size)))
	c1 := int(math.floor(f64(r.x + r.width) / f64(m.tile_size)))
	r0 := int(math.floor(f64(r.y) / f64(m.tile_size)))
	r1 := int(math.floor(f64(r.y + r.height) / f64(m.tile_size)))
	for row in r0 .. r1 + 1 {
		for col in c0 .. c1 + 1 {
			if m.is_solid(col, row) {
				return true
			}
		}
	}
	return false
}

// collides_circle reports whether a circle intersects any solid tile. It
// checks the tiles overlapped by the circle's bounding box.
pub fn (m &TileMap) collides_circle(c Circle) bool {
	return m.collides_rect(Rect{
		x:      c.x - c.radius
		y:      c.y - c.radius
		width:  c.radius * 2
		height: c.radius * 2
	})
}
