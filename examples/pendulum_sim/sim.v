// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// sim.v * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// created by: jordan bonecutter * * * * * * * * * * * * * * * * * * *
// jpbonecutter@gmail.com  * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// I wrote the pendulum simulator to learn V, I think it could be a
// good addition to the examples directory.
// Essentially, the pendulum sim runs a simulation of a pendulum with
// a metallic tip swinging over three magnets.
// I run this simulation with the initial position at each pixel in an
// image and color the pixel according to the magnet over which it
// finally rests.
// I used some fun features in V like coroutines, channels,
// struct embedding, mutability, methods, and the like.
import math
import os
import term
import runtime

// customisable through setting VJOBS
const parallel_workers = runtime.nr_jobs()

const width = 800

const height = 600

struct Vec3D {
	x f64
	y f64
	z f64
}

fn (v Vec3D) add(v2 Vec3D) Vec3D {
	return Vec3D{
		x: v.x + v2.x
		y: v.y + v2.y
		z: v.z + v2.z
	}
}

fn (v Vec3D) dot(v2 Vec3D) f64 {
	return (v.x * v2.x) + (v.y * v2.y) + (v.z * v2.z)
}

fn (v Vec3D) scale(scalar f64) Vec3D {
	return Vec3D{
		x: v.x * scalar
		y: v.y * scalar
		z: v.z * scalar
	}
}

fn (v Vec3D) norm_squared() f64 {
	return v.dot(v)
}

fn (v Vec3D) norm() f64 {
	return math.sqrt(v.norm_squared())
}

struct SimState {
mut:
	position Vec3D
	velocity Vec3D
	accel    Vec3D
}

// magnets lie at [
//	 math.cos(index * 2 * math.pi / 3) * magnet_spacing
//	 math.sin(index * 2 * math.pi / 3) * magnet_spacing
//	 -magnet_height
// ]
struct SimParams {
	rope_length     f64
	bearing_mass    f64
	magnet_spacing  f64
	magnet_height   f64
	magnet_strength f64
	gravity         f64
}

fn (params SimParams) get_rope_vector(state SimState) Vec3D {
	rope_origin := Vec3D{
		x: 0
		y: 0
		z: params.rope_length
	}

	return state.position.add(rope_origin.scale(-1))
}

fn (mut state SimState) satisfy_rope_constraint(params SimParams) {
	mut rope_vector := params.get_rope_vector(state)
	rope_vector = rope_vector.scale(params.rope_length / rope_vector.norm())
	state.position = Vec3D{
		x: 0
		y: 0
		z: params.rope_length
	}.add(rope_vector)
}

fn (params SimParams) get_grav_force(state SimState) Vec3D {
	return Vec3D{
		x: 0
		y: 0
		z: -params.bearing_mass * params.gravity
	}
}

fn (params SimParams) get_magnet_position(theta f64) Vec3D {
	return Vec3D{
		x: math.cos(theta) * params.magnet_spacing
		y: math.sin(theta) * params.magnet_spacing
		z: -params.magnet_height
	}
}

fn (params SimParams) get_magnet_force(theta f64, state SimState) Vec3D {
	magnet_position := params.get_magnet_position(theta)
	mut diff := magnet_position.add(state.position.scale(-1))
	distance_squared := diff.norm_squared()
	diff = diff.scale(1.0 / math.sqrt(distance_squared))
	return diff.scale(params.magnet_strength / distance_squared)
}

fn (params SimParams) get_magnet_dist(theta f64, state SimState) f64 {
	return params.get_magnet_position(theta).add(state.position.scale(-1)).norm()
}

fn (params SimParams) get_magnet1_force(state SimState) Vec3D {
	return params.get_magnet_force(0.0 * math.pi / 3.0, state)
}

fn (params SimParams) get_magnet2_force(state SimState) Vec3D {
	return params.get_magnet_force(2.0 * math.pi / 3.0, state)
}

fn (params SimParams) get_magnet3_force(state SimState) Vec3D {
	return params.get_magnet_force(4.0 * math.pi / 3.0, state)
}

fn (params SimParams) get_tension_force(state SimState, f_passive Vec3D) Vec3D {
	rope_vector := params.get_rope_vector(state)
	rope_vector_norm := rope_vector.scale(1.0 / rope_vector.norm())
	return rope_vector_norm.scale(-1.0 * rope_vector_norm.dot(f_passive))
}

fn (mut state SimState) increment(delta_t f64, params SimParams) {
	// basically just add up all forces =>
	// get an accelleration =>
	// add to velocity =>
	// ensure rope constraint is satisfied

	// force due to gravity
	f_gravity := params.get_grav_force(state)

	// force due to each magnet
	f_magnet1 := params.get_magnet1_force(state)

	// force due to each magnet
	f_magnet2 := params.get_magnet2_force(state)

	// force due to each magnet
	f_magnet3 := params.get_magnet3_force(state)

	// passive forces
	f_passive := f_gravity.add(f_magnet1.add(f_magnet2.add(f_magnet3)))

	// force due to tension of the rope
	f_tension := params.get_tension_force(state, f_passive)

	// sum up all the fores
	f_sum := f_tension.add(f_passive)

	// get the acceleration
	accel := f_sum.scale(1.0 / params.bearing_mass)
	state.accel = accel

	// update the velocity
	state.velocity = state.velocity.add(accel.scale(delta_t))

	// update the position
	state.position = state.position.add(state.velocity.scale(delta_t))

	// ensure the position satisfies rope constraint
	state.satisfy_rope_constraint(params)
}

fn (state SimState) done() bool {
	return state.velocity.norm() < 0.05 && state.accel.norm() < 0.01
}

struct PPMWriter {
mut:
	file os.File
}

struct ImageSettings {
	width  int
	height int
}

struct Pixel {
	r byte
	g byte
	b byte
}

fn (mut writer PPMWriter) start_for_file(fname string, settings ImageSettings) {
	writer.file = os.create(fname) or { panic("can't create file $fname") }
	writer.file.writeln('P6 $settings.width $settings.height 255') or {}
}

fn (mut writer PPMWriter) next_pixel(p Pixel) {
	writer.file.write([p.r, p.g, p.b]) or {}
}

fn (mut writer PPMWriter) finish() {
	writer.file.close()
}

fn sim_runner(mut state SimState, params SimParams) Pixel {
	// do the simulation!
	for _ in 0 .. 1000 {
		state.increment(0.0005, params)
		if state.done() {
			println('done!')
			break
		}
	}

	// find the closest magnet
	m1_dist := params.get_magnet_dist(0, state)
	m2_dist := params.get_magnet_dist(2.0 * math.pi / 3.0, state)
	m3_dist := params.get_magnet_dist(4.0 * math.pi / 3.0, state)

	if m1_dist < m2_dist && m1_dist < m3_dist {
		return Pixel{
			r: 255
			g: 0
			b: 0
		}
	} else if m2_dist < m1_dist && m2_dist < m3_dist {
		return Pixel{
			r: 0
			g: 255
			b: 0
		}
	} else {
		return Pixel{
			r: 0
			g: 0
			b: 255
		}
	}
}

struct SimResult {
	id u64
	p  Pixel
}

struct SimRequest {
	id     u64
	params SimParams
mut:
	initial SimState
}

fn sim_worker(request_chan chan SimRequest, result_chan chan SimResult) {
	// serve sim requests as they come in
	for {
		mut request := <-request_chan or { break }

		result_chan <- SimResult{
			id: request.id
			p: sim_runner(mut request.initial, request.params)
		}
	}
}

struct ValidPixel {
	Pixel
mut:
	valid bool
}

fn image_worker(mut writer PPMWriter, result_chan chan SimResult, total_pixels u64) {
	// as new pixels come in, write them to the image file
	mut current_index := u64(0)
	mut pixel_buf := []ValidPixel{len: int(total_pixels), init: ValidPixel{
		valid: false
	}}
	for {
		result := <-result_chan or { break }
		pixel_buf[result.id].Pixel = result.p
		pixel_buf[result.id].valid = true

		for current_index < total_pixels && pixel_buf[current_index].valid {
			writer.next_pixel(pixel_buf[current_index].Pixel)
			current_index++
		}

		if current_index >= total_pixels {
			break
		}
	}
}

fn main() {
	params := SimParams{
		rope_length: 0.25
		bearing_mass: 0.03
		magnet_spacing: 0.05
		magnet_height: 0.03
		magnet_strength: 10.0
		gravity: 4.9
	}

	mut writer := PPMWriter{}
	writer.start_for_file('test.ppm', ImageSettings{
		width: width
		height: height
	})
	defer {
		writer.finish()
	}

	result_chan := chan SimResult{}
	request_chan := chan SimRequest{}

	// start a worker on each core
	for _ in 0 .. parallel_workers {
		go sim_worker(request_chan, result_chan)
	}

	go fn (request_chan chan SimRequest, params SimParams) {
		mut index := u64(0)
		println('')
		for y in 0 .. height {
			term.clear_previous_line()
			println('Line: $y')
			for x in 0 .. width {
				// setup initial conditions
				mut state := SimState{}
				state.position = Vec3D{
					x: 0.1 * ((f64(x) - 0.5 * f64(width - 1)) / f64(width - 1))
					y: 0.1 * ((f64(y) - 0.5 * f64(height - 1)) / f64(height - 1))
					z: 0.0
				}
				state.velocity = Vec3D{}
				state.satisfy_rope_constraint(params)
				request_chan <- SimRequest{
					id: index
					initial: state
					params: params
				}
				index++
			}
		}
		request_chan.close()
	}(request_chan, params)

	image_worker(mut writer, result_chan, width * height)
}
