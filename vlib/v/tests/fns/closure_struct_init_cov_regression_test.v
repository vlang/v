// vtest vflags: -cov .vtmp_cov_issue_24842
module main

import term.ui as tui

enum ThemeSlot {
	primary
}

struct CoverageClosureCtx {
	active_fg_color tui.Color
	on_draw_cb      fn (int, int, string, tui.Color, ?tui.Color)
	on_draw_rect_cb fn (int, int, int, int)
}

fn test_cov_struct_init_with_closures_and_index_or() {
	palette := map[ThemeSlot]tui.Color{}
	key := ThemeSlot.primary
	mut drawn := []int{}
	mut drawn_ref := &drawn
	_ = CoverageClosureCtx{
		active_fg_color: palette[key] or {
			tui.Color{
				r: 1
				g: 2
				b: 3
			}
		}
		on_draw_cb:      fn [mut drawn_ref] (x int, y int, text string, fg tui.Color, bg ?tui.Color) {
			drawn_ref << x + y + text.len + fg.r
			_ = bg
		}
		on_draw_rect_cb: fn [mut drawn_ref] (x int, y int, width int, height int) {
			drawn_ref << x + y + width + height
		}
	}
	assert true
}
