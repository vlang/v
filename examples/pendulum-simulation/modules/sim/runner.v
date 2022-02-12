module sim

import benchmark
import term

pub type SimRequestHandler = fn (request &SimRequest) ?

pub type SimStartHandler = fn () ?

pub type SimFinishHandler = fn () ?

pub const (
	default_width  = 600
	default_height = 600
)

[params]
pub struct GridSettings {
pub:
	width  int = sim.default_width
	height int = sim.default_height
}

pub fn new_grid_settings(settings GridSettings) GridSettings {
	return GridSettings{
		...settings
	}
}

[params]
pub struct RunnerSettings {
pub:
	grid       GridSettings
	on_request SimRequestHandler
	on_start   SimStartHandler
	on_finish  SimFinishHandler
}

pub fn run(params SimParams, settings RunnerSettings) {
	height := settings.grid.height
	width := settings.grid.width

	if !isnil(settings.on_start) {
		settings.on_start() or {
			log(@MOD + '.' + @FN + ': Simulation start handler failed. Error $err')
		}
	}

	mut index := 0
	log('')

	mut bmark := benchmark.new_benchmark()
	for y in 0 .. height {
		$if verbose ? {
			term.clear_previous_line()
		}
		log(@MOD + '.' + @FN + ': y: ${y + 1}')
		for x in 0 .. width {
			bmark.step()
			// setup state conditions
			position := vector(
				x: 0.1 * ((f64(x) - 0.5 * f64(width - 1)) / f64(width - 1))
				y: 0.1 * ((f64(y) - 0.5 * f64(height - 1)) / f64(height - 1))
				z: 0.0
			)
			velocity := vector(x: 0, y: 0, z: 0)

			mut state := new_state(
				position: position
				velocity: velocity
			)

			state.satisfy_rope_constraint(params)
			request := &SimRequest{
				id: index
				state: state
				params: params
			}
			settings.on_request(request) or {
				log(@MOD + '.' + @FN + ': request handler failed. Error $err')
				bmark.fail()
				break
			}
			index++
			bmark.ok()
		}
	}
	bmark.stop()
	println(bmark.total_message(@FN))

	if !isnil(settings.on_finish) {
		settings.on_finish() or {
			log(@MOD + '.' + @FN + ': Simulation stop handler failed. Error $err')
		}
	}
}
