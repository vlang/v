// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module fx

import gg
import gg.game

// UITheme holds the colors and spacing used by the immediate-mode UI helpers.
pub struct UITheme {
pub mut:
	background  gg.Color = gg.Color{24, 26, 34, 255}
	panel       gg.Color = gg.Color{44, 48, 62, 255}
	panel_hover gg.Color = gg.Color{62, 70, 92, 255}
	text        gg.Color = gg.Color{230, 234, 244, 255}
	accent      gg.Color = gg.Color{80, 170, 220, 255}
	border      gg.Color = gg.Color{90, 98, 120, 255}
	padding     f32      = 8
}

// default_ui_theme returns a dark high-contrast starter theme.
pub fn default_ui_theme() UITheme {
	return UITheme{}
}

// ui_hit reports whether a mouse point falls inside a rectangle.
pub fn ui_hit(r gg.Rect, mouse gg.Vec2) bool {
	return game.point_in_rect(mouse, r)
}

// ui_slider_value converts a mouse x position into a value between min and max
// for a horizontal slider rectangle. The result is clamped to the range.
pub fn ui_slider_value(r gg.Rect, mouse_x f32, min f32, max f32) f32 {
	if r.width <= 0 || max <= min {
		return min
	}
	t := math_clip_unit((mouse_x - r.x) / r.width)
	return min + (max - min) * t
}

fn math_clip_unit(v f32) f32 {
	if v < 0 {
		return 0
	}
	if v > 1 {
		return 1
	}
	return v
}

// ui_grid_cells lays out count cells of the given size in a row-major grid
// inside bounds and returns their rectangles.
pub fn ui_grid_cells(bounds gg.Rect, cell_width f32, cell_height f32, count int) []gg.Rect {
	mut out := []gg.Rect{}
	if cell_width <= 0 || cell_height <= 0 || count <= 0 {
		return out
	}
	cols := int(bounds.width / cell_width)
	if cols <= 0 {
		return out
	}
	for i in 0 .. count {
		col := i % cols
		row := i / cols
		out << gg.Rect{
			x:      bounds.x + f32(col) * cell_width
			y:      bounds.y + f32(row) * cell_height
			width:  cell_width
			height: cell_height
		}
	}
	return out
}
