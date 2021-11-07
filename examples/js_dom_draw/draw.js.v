import jsdom

fn get_2dcontext(canvas jsdom.IElement) ?jsdom.CanvasRenderingContext2D {
	if canvas is jsdom.HTMLCanvasElement {
		c := canvas.get_context('2d')
		match c {
			jsdom.CanvasRenderingContext2D {
				return c
			}
			else {
				return error('cannot fetch 2d context')
			}
		}
	} else {
		return error('canvas is not an HTMLCanvasElement')
	}
}

fn draw_line(context jsdom.CanvasRenderingContext2D, x1 int, y1 int, x2 int, y2 int) {
	context.begin_path()
	context.set_stroke_style('black')
	context.set_line_width(1)
	context.move_to(x1, y1)
	context.line_to(x2, y2)
	context.stroke()
	context.close_path()
}

struct DrawState {
mut:
	drawing bool
	x       int
	y       int
}

fn main() {
	document := jsdom.document

	elem := document.get_element_by_id('myButton') ?
	elemc := document.get_element_by_id('myCanvas') or { panic('no canvas') }
	canv := jsdom.get_html_canvas_element(elemc) or { panic('expected canvas') }

	context := canv.get_context_2d()
	mut state := DrawState{}
	canv.add_event_listener('mousedown', fn [mut state] (_ jsdom.IEventTarget, event jsdom.IEvent) {
		state.drawing = true
		if event is jsdom.MouseEvent {
			state.x = event.offset_x()
			state.y = event.offset_y()
		}
	})

	canv.add_event_listener('mousemove', fn [context, mut state] (_ jsdom.IEventTarget, event jsdom.IEvent) {
		if state.drawing {
			if event is jsdom.MouseEvent {
				draw_line(context, state.x, state.y, event.offset_x(), event.offset_y())
				state.x = event.offset_x()
				state.y = event.offset_y()
			}
		}
	})

	jsdom.window.add_event_listener('mouseup', fn [context, mut state] (_ jsdom.IEventTarget, event jsdom.IEvent) {
		if state.drawing {
			if event is jsdom.MouseEvent {
				draw_line(context, state.x, state.y, event.offset_x(), event.offset_y())
			}
			state.x = 0
			state.y = 0
			state.drawing = false
		}
	})
	elem.add_event_listener('click', fn [context, canv] (_ jsdom.IEventTarget, _ jsdom.IEvent) {
		context.clear_rect(0, 0, canv.width(), canv.height())
	})
}
