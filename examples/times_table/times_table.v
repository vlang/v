import ui
import gx

const (
	WIN_SIZE     = 540
	MIN          = 1
	MAX          = 9
	FONT_SIZE    = 30
	N            = MAX - MIN + 1
	CELL_SIZE    = WIN_SIZE / N
	TEXT_CFG     = gx.TextCfg { color: gx.BLACK, size: FONT_SIZE }
	HEADER_COLOR = gx.rgb(240, 240, 240)
)

fn main() {
	cfg := ui.WinCfg {
		width: WIN_SIZE
		height: WIN_SIZE
		title: 'Times Table'
		draw_fn: draw
	}
	wnd := ui.new_window(cfg)
	for {
		ui.wait_events()
	}
}

// ui.Window uses native drawing API (Core Graphics, GDI+)
fn draw() {
	// Horizontal header
	gx.draw_rect(0, 0, WIN_SIZE, CELL_SIZE, HEADER_COLOR)
	// Vertical header
	gx.draw_rect(0, 0, CELL_SIZE, WIN_SIZE, HEADER_COLOR)
	for i := MIN; i <= MAX; i++ {
		y := CELL_SIZE * (i - MIN)
		for j := MIN; j <= MAX; j++ {
			x := CELL_SIZE * (j - MIN)
			// Draw the result
			if ! (i == MIN && j == MIN) {
				res := i * j
				mut text_padding_x := (CELL_SIZE - FONT_SIZE) / 2 - 1
				text_padding_y := text_padding_x - 3
				if res < 10 {
					text_padding_x += 9
				}
				gx.draw_text(x + text_padding_x, y + text_padding_y, res.str(), TEXT_CFG)
			}
		}
		// Horizontal lines
		gx.draw_line(0, y, WIN_SIZE, y)
		// Vertical lines
		gx.draw_line(y, 0, y, WIN_SIZE)
	}
}

