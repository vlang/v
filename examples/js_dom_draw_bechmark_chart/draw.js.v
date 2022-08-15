module main

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
	context.moveTo(0, 0)
	context.lineTo(100, 100)
	context.stroke()
	context.closePath()
}

struct DrawState {
mut:
	context JS.CanvasRenderingContext2D
	drawing bool
	x       f64
	y       f64
}

fn (mut state DrawState) draw_bench_chart(color string, time_array []int, max_time int) ? {
	println(time_array.len)
	max_height := f64(480)
	max_width := f64(720)

	state.drawing = true
	state.x = f64(0)
	state.y = f64(max_height)
	state.context.strokeStyle = color.str
	state.context.lineWidth = JS.Number(1)

	for i := 0; i <= time_array.len; i++ {
		state.context.beginPath()
		state.context.moveTo(state.x, state.y)
		state.x = max_width / f64(time_array.len) * i + 1.0
		state.y = max_height - (max_height / f64(max_time) * f64(time_array[i]))
		state.context.lineTo(state.x, state.y)
		state.context.stroke()
		state.context.closePath()
	}

	state.drawing = false
}

fn main() {
	document := dom.document

	clear_btn := document.getElementById('clearButton'.str)?

	canvas_elem := document.getElementById('canvas_insert_id'.str)?

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

	attribute_names := ['sqlite_memory_insert_times', 'sqlite_file_insert_times' /*
	,
		'postgres_insert_times', 'mysql_insert_times'
	*/]
	chart_colors := ['gray', 'black', 'red', 'orange', 'purple']

	for idx, name in attribute_names {
		// get values in JS.String values
		mut attribute_js_values := canvas_elem.getAttribute(name.str) or {
			println('Não pegou o attributo')
			continue // if attribute not exist, jump.
		}
		if attribute_js_values.length < JS.Number(1) {
			continue // if attribute value is empty, jump.
		}

		// convert []JS.String in v []string
		mut attribute_string_values := tos(attribute_js_values).replace('[', '').replace(']',
			'').split(',')

		// convert []string in []int
		mut attribute_int_values := []int{}
		for variable in attribute_string_values {
			attribute_int_values << variable.int()
		}
		// draw chart
		state.draw_bench_chart(chart_colors[idx], attribute_int_values, 11204530) or {
			println(err)
		}
	}

	clear_btn.addEventListener('click'.str, fn [mut state, canvas] (_ JS.Event) {
		state.context.clearRect(0, 0, canvas.width, canvas.height)
	}, JS.EventListenerOptions{})
}
