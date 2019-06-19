import ui
import gx

const (
	NrCols     = 3
	CellHeight = 25
	CellWidth  = 100
	TableWidth = CellWidth * NrCols
)

struct User {
mut:
	first_name string
	last_name  string
	age        int
}

struct Context {
mut:
	first_name ui.TextBox
	last_name  ui.TextBox
	age        ui.TextBox
	users      []User
	window     *ui.Window
	txt_pos    int
}

fn main() {
	mut ctx := &Context {
		txt_pos: 10
		window: 0// TODO
	}
	ctx.window = ui.new_window(ui.WinCfg {
		width: 500
		height: 300
		title: 'Users'
		draw_fn: draw
		ptr: ctx
	})
	ctx.first_name = ctx.add_textbox('First name')
	ctx.last_name = ctx.add_textbox('Last name')
	ctx.age = ctx.add_textbox('Age')
	mut btn := ui.new_button('Add user', ctx.window, btn_click)
	btn.widget.set_pos(TableWidth + 50, ctx.txt_pos)
	for {
		ui.wait_events()
	}
}

// TODO replace with `fn (ctx mut Context) btn_click() {`
fn btn_click(btn voidptr, ctx mut Context) {
	ctx.users << User {
		first_name: ctx.first_name.text()
		last_name: ctx.last_name.text()
		age: ctx.age.text().to_i()
	}
	ctx.window.refresh()
}

// TODO replace with `fn (ctx mut Context) draw() {`
fn draw(ctx *Context) {
	for i, user in ctx.users {
		x := 10
		y := 10 + i * CellHeight
		// Outer border
		ui.draw_empty_rect(x, y, TableWidth, CellHeight, gx.GRAY)
		// Vertical separators
		ui.draw_line(x + CellWidth, y, x + CellWidth, y + CellHeight)
		ui.draw_line(x + CellWidth * 2, y, x + CellWidth * 2, y + CellHeight)
		// Text values
		ui.draw_text_def(x + 5, y + 5, user.first_name)
		ui.draw_text_def(x + 5 + CellWidth, y + 5, user.last_name)
		ui.draw_text_def(x + 5 + CellWidth * 2, y + 5, user.age.str())
	}
}

fn (ctx mut Context) add_textbox(placeholder string) ui.TextBox {
	mut txt_box := ui.new_textbox(ctx.window, false)
	txt_box.set_placeholder(placeholder)
	txt_box.widget.set_pos(TableWidth + 50, ctx.txt_pos)
	ctx.txt_pos += 30
	return txt_box
}

