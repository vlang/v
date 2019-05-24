import ui
import gx

const (
	WinSize     = 540
	Min         = 1
	Max         = 9
	FontSize    = 30
	N           = Max - Min + 1
	CellSize    = WinSize / N
	TextCfg     = gx.TextCfg { color: gx.BLACK, size: FontSize }
	HeaderColor = gx.rgb(240, 240, 240)
)

fn main() {
	ui.new_window(ui.WinCfg {
		width: WinSize
		height: WinSize
		title: 'Times Table'
		draw_fn: draw
	})
	for {
		ui.wait_events()
	}
}

// ui.Window uses native drawing API (Core Graphics, GDI+)
fn draw() {
	ui.draw_rect(0, 0, WinSize, CellSize, HeaderColor)// Horizontal header
	ui.draw_rect(0, 0, CellSize, WinSize, HeaderColor)// Vertical header
	for i := Min; i <= Max; i++ {
		y := CellSize * (i - Min)
		for j := Min; j <= Max; j++ {
			// Skip top left corner
			if i == Min && j == Min {
				continue
			}
			// Draw the result
			x := CellSize * (j - Min)
			res := i * j
			mut text_padding_x := (CellSize - FontSize) / 2 - 1
			text_padding_y := text_padding_x - 3
			if res < 10 {
				text_padding_x += 9
			}
			ui.draw_text(x + text_padding_x, y + text_padding_y, res.str(), TextCfg)
		}
		// Horizontal lines
		ui.draw_line(0, y, WinSize, y)
		// Vertical lines
		ui.draw_line(y, 0, y, WinSize)
	}
}

