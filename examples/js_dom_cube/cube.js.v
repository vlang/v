import js.dom
import math

const (
	vert_code = 'attribute vec3 position;uniform mat4 Pmatrix;uniform mat4 Vmatrix;uniform mat4 Mmatrix;attribute vec3 color;varying vec3 vColor;void main(void) {gl_Position = Pmatrix * Vmatrix * Mmatrix * vec4(position,1.);vColor = color;}
	'

	frag_code = 'precision mediump float;varying vec3 vColor;void main(void) {gl_FragColor = vec4(vColor, 1.);}
	'

	vertices     = [
		f32(-1),
		-1,
		-1,
		1,
		-1,
		-1,
		1,
		1,
		-1,
		-1,
		1,
		-1,
		-1,
		-1,
		1,
		1,
		-1,
		1,
		1,
		1,
		1,
		-1,
		1,
		1,
		-1,
		-1,
		-1,
		-1,
		1,
		-1,
		-1,
		1,
		1,
		-1,
		-1,
		1,
		1,
		-1,
		-1,
		1,
		1,
		-1,
		1,
		1,
		1,
		1,
		-1,
		1,
		-1,
		-1,
		-1,
		-1,
		-1,
		1,
		1,
		-1,
		1,
		1,
		-1,
		-1,
		-1,
		1,
		-1,
		-1,
		1,
		1,
		1,
		1,
		1,
		1,
		1,
		-1,
	]
	colors       = [
		f32(5),
		3,
		7,
		5,
		3,
		7,
		5,
		3,
		7,
		5,
		3,
		7,
		1,
		1,
		3,
		1,
		1,
		3,
		1,
		1,
		3,
		1,
		1,
		3,
		0,
		0,
		1,
		0,
		0,
		1,
		0,
		0,
		1,
		0,
		0,
		1,
		1,
		0,
		0,
		1,
		0,
		0,
		1,
		0,
		0,
		1,
		0,
		0,
		1,
		1,
		0,
		1,
		1,
		0,
		1,
		1,
		0,
		1,
		1,
		0,
		0,
		1,
		0,
		0,
		1,
		0,
		0,
		1,
		0,
		0,
		1,
		0,
	]
	indices      = [
		u16(0),
		1,
		2,
		0,
		2,
		3,
		4,
		5,
		6,
		4,
		6,
		7,
		8,
		9,
		10,
		8,
		10,
		11,
		12,
		13,
		14,
		12,
		14,
		15,
		16,
		17,
		18,
		16,
		18,
		19,
		20,
		21,
		22,
		20,
		22,
		23,
	]
	amortization = 0.95
)

fn get_webgl() (JS.HTMLCanvasElement, JS.WebGLRenderingContext) {
	JS.console.log(dom.document)
	elem := dom.document.getElementById('myCanvas'.str) or { panic('cannot get canvas') }
	match elem {
		JS.HTMLCanvasElement {
			webgl := elem.getContext('experimental-webgl'.str, js_undefined()) or {
				panic('context not found')
			}
			match webgl {
				JS.WebGLRenderingContext {
					return elem, webgl
				}
				else {
					panic('cannot get webgl')
				}
			}
		}
		else {
			panic('not an canvas')
		}
	}
}

fn get_projection(angle f64, a f64, z_min f64, z_max f64) []f64 {
	ang := math.tan((angle * 0.5) * math.pi / 180)
	return [
		0.5 / ang,
		0,
		0,
		0,
		0,
		0.5 * a / ang,
		0,
		0,
		0,
		0,
		-(z_max + z_min) / (z_max - z_min),
		-1,
		0,
		0,
		(-2 * z_max * z_min) / (z_max - z_min),
		0,
	]
}

fn JS.Math.cos(JS.Number) JS.Number
fn JS.Math.sin(JS.Number) JS.Number
fn rotate_x(mut m []f64, angle f64) {
	c := math.cos(angle)
	s := math.sin(angle)
	mv1 := m[1]
	mv5 := m[5]
	mv9 := m[9]
	m[1] = m[1] * c - m[2] * s
	m[5] = m[5] * c - m[6] * s
	m[9] = m[9] * c - m[10] * s

	m[2] = m[2] * c + mv1 * s
	m[6] = m[6] * c + mv5 * s
	m[10] = m[10] * c + mv9 * s
}

fn rotate_y(mut m []f64, angle f64) {
	c := math.cos(angle)
	s := math.sin(angle)

	mv0 := m[0]
	mv4 := m[4]
	mv8 := m[8]
	m[0] = c * m[0] + s * m[2]
	m[4] = c * m[4] + s * m[6]
	m[8] = c * m[8] + s * m[10]

	m[2] = c * m[2] - s * mv0
	m[6] = c * m[6] - s * mv4
	m[10] = c * m[10] - s * mv8
}

struct State {
mut:
	drag         bool
	gl           JS.WebGLRenderingContext
	canvas       JS.HTMLCanvasElement
	old_x        f64
	old_y        f64
	dx           f64
	dy           f64
	theta        f64
	phi          f64
	time_old     f64
	mo_matrix    []f64
	view_matrix  []f64
	proj_matrix  []f64
	pmatrix      JS.WebGLUniformLocation
	vmatrix      JS.WebGLUniformLocation
	mmatrix      JS.WebGLUniformLocation
	index_buffer JS.WebGLBuffer
}

fn animate(mut state State, time f64) {
	if !state.drag {
		state.dx = state.dx * amortization
		state.dy = state.dy * amortization
		state.theta += state.dx
		state.phi += state.dy
	}

	state.mo_matrix[0] = 1
	state.mo_matrix[1] = 0
	state.mo_matrix[2] = 0
	state.mo_matrix[3] = 0

	state.mo_matrix[4] = 0
	state.mo_matrix[5] = 1
	state.mo_matrix[6] = 0
	state.mo_matrix[7] = 0

	state.mo_matrix[8] = 0
	state.mo_matrix[9] = 0
	state.mo_matrix[10] = 1
	state.mo_matrix[11] = 0

	state.mo_matrix[12] = 0
	state.mo_matrix[13] = 0
	state.mo_matrix[14] = 0
	state.mo_matrix[15] = 1
	// println('${state.theta} ${state.phi}')
	rotate_x(mut state.mo_matrix, state.phi)
	rotate_y(mut state.mo_matrix, state.theta)
	state.time_old = time
	state.gl.enable(dom.gl_depth_test())
	state.gl.clearColor(0.5, 0.5, 0.5, 0.9)
	state.gl.clearDepth(1.0)
	state.gl.viewport(0.0, 0.0, state.canvas.width, state.canvas.height)
	state.gl.clear(JS.Number(int(dom.gl_color_buffer_bit()) | int(dom.gl_depth_buffer_bit())))

	state.gl.uniformMatrix4fv(state.pmatrix, JS.Boolean(false), state.proj_matrix.to_number_array())
	state.gl.uniformMatrix4fv(state.vmatrix, JS.Boolean(false), state.view_matrix.to_number_array())
	state.gl.uniformMatrix4fv(state.mmatrix, JS.Boolean(false), state.mo_matrix.to_number_array())

	state.gl.bindBuffer(dom.gl_element_array_buffer(), state.index_buffer)
	state.gl.drawElements(dom.gl_triangles(), indices.len, dom.gl_unsigned_short(), 0)

	dom.window().requestAnimationFrame(fn [mut state] (time JS.Number) {
		animate(mut state, f64(time))
	})
}

fn main() {
	canvas, gl := get_webgl()

	vertex_buffer := gl.createBuffer()?
	gl.bindBuffer(dom.gl_array_buffer(), vertex_buffer)
	gl.bufferData(dom.gl_array_buffer(), float32_array(vertices), dom.gl_static_draw())

	color_buffer := gl.createBuffer()?
	gl.bindBuffer(dom.gl_array_buffer(), color_buffer)
	gl.bufferData(dom.gl_array_buffer(), float32_array(colors), dom.gl_static_draw())

	index_buffer := gl.createBuffer()?
	gl.bindBuffer(dom.gl_element_array_buffer(), index_buffer)
	gl.bufferData(dom.gl_element_array_buffer(), uint16_array(indices), dom.gl_static_draw())

	vert_shader := gl.createShader(dom.gl_vertex_shader())?
	gl.shaderSource(vert_shader, vert_code.str)
	gl.compileShader(vert_shader)

	if !bool(JS.Boolean(gl.getShaderParameter(vert_shader, dom.gl_compile_status()))) {
		panic('An error occurred when compiling vertex shader: ${string(gl.getShaderInfoLog(vert_shader))}')
	}

	frag_shader := gl.createShader(dom.gl_fragment_shader())?
	gl.shaderSource(frag_shader, frag_code.str)
	gl.compileShader(frag_shader)
	if !bool(JS.Boolean(gl.getShaderParameter(frag_shader, dom.gl_compile_status()))) {
		panic('An error occurred when compiling fragment shader: ${string(gl.getShaderInfoLog(frag_shader))}')
	}

	shader_program := gl.createProgram()?
	gl.attachShader(shader_program, vert_shader)
	gl.attachShader(shader_program, frag_shader)
	gl.linkProgram(shader_program)

	if !bool(JS.Boolean(gl.getProgramParameter(shader_program, dom.gl_link_status()))) {
		panic('unable to initialize the shader program: ${string(gl.getProgramInfoLog(shader_program))}')
	}

	pmatrix := gl.getUniformLocation(shader_program, 'Pmatrix'.str)?
	vmatrix := gl.getUniformLocation(shader_program, 'Vmatrix'.str)?
	mmatrix := gl.getUniformLocation(shader_program, 'Mmatrix'.str)?

	gl.bindBuffer(dom.gl_array_buffer(), vertex_buffer)
	position := gl.getAttribLocation(shader_program, 'position'.str)
	gl.vertexAttribPointer(position, JS.Number(3), dom.gl_float(), JS.Boolean(false),
		JS.Number(0), JS.Number(0))
	gl.enableVertexAttribArray(position)

	gl.bindBuffer(dom.gl_array_buffer(), color_buffer)
	color := gl.getAttribLocation(shader_program, 'color'.str)
	gl.vertexAttribPointer(color, JS.Number(3), dom.gl_float(), JS.Boolean(false), JS.Number(0),
		JS.Number(0))
	gl.enableVertexAttribArray(color)
	gl.useProgram(shader_program)

	mut proj_matrix := get_projection(40.0, f64(canvas.width) / f64(canvas.height), 1.0,
		100.0)
	mut mo_matrix := [f64(1), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]
	mut view_matrix := [f64(1), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]

	view_matrix[14] = view_matrix[14] - 6

	mut state := State{false, gl, canvas, 0, 0, 0, 0, 0, 0, 0, mo_matrix, view_matrix, proj_matrix, pmatrix, vmatrix, mmatrix, index_buffer}

	canvas.addEventListener('mousedown'.str, fn [mut state] (e JS.Event) {
		state.drag = true
		match e {
			JS.MouseEvent {
				state.old_x = f64(e.pageX)
				state.old_y = f64(e.pageY)
				e.preventDefault()
			}
			else {}
		}
	}, JS.EventListenerOptions{})

	canvas.addEventListener('mouseup'.str, fn [mut state] (e JS.Event) {
		state.drag = false
	}, JS.EventListenerOptions{})
	canvas.addEventListener('mouseout'.str, fn [mut state] (e JS.Event) {
		state.drag = false
	}, JS.EventListenerOptions{})
	canvas.addEventListener('mousemove'.str, fn [mut state] (e JS.Event) {
		if !state.drag {
			return
		}
		match e {
			JS.MouseEvent {
				state.dx = (f64(e.pageX) - state.old_x) * 2.0 * math.pi / f64(state.canvas.width)
				state.dy = (f64(e.pageY) - state.old_y) * 2.0 * math.pi / f64(state.canvas.height)
				state.theta += state.dx
				state.phi += state.dy
				state.old_x = f64(e.pageX)
				state.old_y = f64(e.pageY)
				e.preventDefault()
			}
			else {
				panic('not a mouse event??')
			}
		}
	}, JS.EventListenerOptions{})

	animate(mut state, 0)
}
