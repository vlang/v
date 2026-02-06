import js.dom

fn get_canvas(elem JS.HTMLElement) JS.HTMLCanvasElement {
	match elem {
		JS.HTMLCanvasElement {
			return elem
		}
		else {
			panic('Not a canvas')
		}
	}
}

fn draw_line(mut context JS.CanvasRenderingContext2D, x1 int, y1 int, x2 int, y2 int) {
	context.beginPath()
	context.strokeStyle = 'black'.str
	context.lineWidth = JS.Number(1)
	context.moveTo(x1, y1)
	context.lineTo(x2, y2)
	context.stroke()
	context.closePath()
}

struct DrawState {
mut:
	context JS.CanvasRenderingContext2D
	drawing bool
	x       int
	y       int
}

fn main() {
	window := dom.window()
	document := dom.document
	clear_btn := document.getElementById('clearButton'.str)?
	canvas_elem := document.getElementById('canvas'.str)?
	canvas := get_canvas(canvas_elem)
	ctx := canvas.getContext('2d'.str, js_undefined())?
	context := match ctx {
		JS.CanvasRenderingContext2D {
			ctx
		}
		else {
			panic('can not get 2d context')
		}
	}
	mut state := DrawState{context, false, 0, 0}

	canvas.addEventListener('mousedown'.str, fn [mut state] (event JS.Event) {
		state.drawing = true
		match event {
			JS.MouseEvent {
				state.x = int(event.offsetX)
				state.y = int(event.offsetY)
			}
			else {}
		}
	}, JS.EventListenerOptions{})
	canvas.addEventListener('mousemove'.str, fn [mut state] (event JS.Event) {
		if state.drawing {
			match event {
				JS.MouseEvent {
					draw_line(mut state.context, state.x, state.y, int(event.offsetX),
						int(event.offsetY))
					state.x = int(event.offsetX)
					state.y = int(event.offsetY)
				}
				else {}
			}
		}
	}, JS.EventListenerOptions{})

	window.addEventListener('mouseup'.str, fn [mut state] (event JS.Event) {
		if state.drawing {
			match event {
				JS.MouseEvent {
					draw_line(mut state.context, state.x, state.y, int(event.offsetX),
						int(event.offsetY))
				}
				else {}
			}
			state.x = 0
			state.y = 0
			state.drawing = false
		}
	}, JS.EventListenerOptions{})
	clear_btn.addEventListener('click'.str, fn [mut state, canvas] (_ JS.Event) {
		state.context.clearRect(0, 0, canvas.width, canvas.height)
	}, JS.EventListenerOptions{})
}
