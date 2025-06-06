module main

import js.dom

fn get_canvas(elem JS.HTMLElement) JS.HTMLCanvasElement {
	//  error: `JS.HTMLElement` doesn't implement method `getContext` of interface `JS.HTMLCanvasElement`
	match elem {
		JS.HTMLCanvasElement {
			return elem
		}
		else {
			JS.console.log('Not canvas')
			return JS.HTMLCanvasElement{}
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

struct FrameworkPlatform {
mut:
	v_sqlite_memory []int
	// v_sqlite_file            []int
	typescript_sqlite_memory []int
}

fn (mut state DrawState) draw_bench_chart(color string, time_array []int, max_time int) ? {
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

	mut canvas_elem := map[string]JS.HTMLElement{}
	mut canvas := map[string]JS.HTMLCanvasElement{}

	canvas_elem['insert'] = document.getElementById('canvas_insert_id'.str)?
	JS.console.log('canvas_insert_id')

	canvas_elem['select'] = document.getElementById('canvas_select_id'.str)?
	JS.console.log('canvas_select_id')

	canvas_elem['update'] = document.getElementById('canvas_update_id'.str)?
	JS.console.log('canvas_update_id')

	// for orm_stmt_kind in ["insert", "select", "update"]{
	for orm_stmt_kind in ['insert', 'select', 'update'] {
		// type HTMLElement

		canvas[orm_stmt_kind] = get_canvas(canvas_elem[orm_stmt_kind])

		ctx := canvas[orm_stmt_kind].getContext('2d'.str, js_undefined())?

		context := match ctx {
			JS.CanvasRenderingContext2D {
				ctx
			}
			else {
				panic('can not get 2d context')
			}
		}

		mut state := DrawState{context, false, 0, 0}

		mut inserts_from_framework := canvas_elem[orm_stmt_kind].getAttribute('inserts_from_framework'.str)?

		mut max_benchmark := canvas_elem[orm_stmt_kind].getAttribute('max_benchmark'.str)?

		// -----------------------------------------------------------------------------------------------------------------------------------------------------------------
		mut obj := FrameworkPlatform{}
		obj = JS.JSON.parse(tos(inserts_from_framework))

		// Waiting for v implement for loop getting key and value of object in v.js
		mut attribute_int_values := []int{}

		//* v framework
		for variable in obj.v_sqlite_memory {
			attribute_int_values << variable
		}

		state.draw_bench_chart('gray', attribute_int_values, tos(max_benchmark).int())?
		attribute_int_values = []

		//* typescript framework

		for variable in obj.typescript_sqlite_memory {
			attribute_int_values << variable
		}

		state.draw_bench_chart('red', attribute_int_values, tos(max_benchmark).int())?

		attribute_int_values = []
	}
}
