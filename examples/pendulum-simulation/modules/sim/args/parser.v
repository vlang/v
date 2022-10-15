module args

import flag
import os
import runtime
import sim
import math

// customisable through setting VJOBS
const max_parallel_workers = runtime.nr_jobs()

[params]
pub struct ParserSettings {
	sequential    bool
	img           bool
	extra_workers int
}

pub struct SequentialArgs {
pub:
	params   sim.SimParams
	grid     sim.GridSettings
	filename string
}

pub struct ParallelArgs {
	SequentialArgs
pub:
	workers int = args.max_parallel_workers
}

pub type SimArgs = ParallelArgs | SequentialArgs

pub fn parse_args(config ParserSettings) !SimArgs {
	if config.sequential {
		args := parse_sequential_args()!
		return SimArgs(args)
	} else {
		args := parse_parallel_args(config.extra_workers)!
		return SimArgs(args)
	}
}

fn parse_sequential_args() !SequentialArgs {
	mut fp := flag.new_flag_parser(os.args)
	fp.application('vps')
	fp.version('v0.1.0')
	fp.limit_free_args(0, 0)!
	fp.description('This is a pendulum simulation written in pure V')
	fp.skip_executable()

	// output parameters
	width := fp.int('width', `w`, sim.default_width, 'width of the image output. Defaults to $sim.default_width')
	height := fp.int('height', `h`, sim.default_height, 'height of the image output. Defaults to $sim.default_height')
	filename := fp.string('output', `o`, 'out.ppm', 'name of the image output. Defaults to out.ppm')

	// simulation parameters
	rope_length := fp.float('rope-length', 0, sim.default_rope_length, 'rope length to use on simulation. Defaults to $sim.default_rope_length')
	bearing_mass := fp.float('bearing-mass', 0, sim.default_bearing_mass, 'bearing mass to use on simulation. Defaults to $sim.default_bearing_mass')
	magnet_spacing := fp.float('magnet-spacing', 0, sim.default_magnet_spacing, 'magnet spacing to use on simulation. Defaults to $sim.default_magnet_spacing')
	magnet_height := fp.float('magnet-height', 0, sim.default_magnet_height, 'magnet height to use on simulation. Defaults to $sim.default_magnet_height')
	magnet_strength := fp.float('magnet-strength', 0, sim.default_magnet_strength, 'magnet strength to use on simulation. Defaults to $sim.default_magnet_strength')
	gravity := fp.float('gravity', 0, sim.default_gravity, 'gravity to use on simulation. Defaults to $sim.default_gravity')

	fp.finalize() or {
		println(fp.usage())
		return error('none')
	}

	params := sim.sim_params(
		rope_length: rope_length
		bearing_mass: bearing_mass
		magnet_spacing: magnet_spacing
		magnet_height: magnet_height
		magnet_strength: magnet_strength
		gravity: gravity
	)

	grid := sim.new_grid_settings(
		width: width
		height: height
	)

	args := SequentialArgs{
		params: params
		filename: filename
		grid: grid
	}

	sim.log('$args')

	return args
}

fn parse_parallel_args(extra_workers int) !ParallelArgs {
	mut fp := flag.new_flag_parser(os.args)
	fp.application('vps')
	fp.version('v0.1.0')
	fp.limit_free_args(0, 0)!
	fp.description('This is a pendulum simulation written in pure V')
	fp.skip_executable()

	workers := fp.int('workers', 0, args.max_parallel_workers, 'amount of workers to use on simulation. Defaults to $args.max_parallel_workers')

	// output parameters
	width := fp.int('width', `w`, sim.default_width, 'width of the image output. Defaults to $sim.default_width')
	height := fp.int('height', `h`, sim.default_height, 'height of the image output. Defaults to $sim.default_height')
	filename := fp.string('output', `o`, 'out.ppm', 'name of the image output. Defaults to out.ppm')

	// simulation parameters
	rope_length := fp.float('rope-length', 0, sim.default_rope_length, 'rope length to use on simulation. Defaults to $sim.default_rope_length')
	bearing_mass := fp.float('bearing-mass', 0, sim.default_bearing_mass, 'bearing mass to use on simulation. Defaults to $sim.default_bearing_mass')
	magnet_spacing := fp.float('magnet-spacing', 0, sim.default_magnet_spacing, 'magnet spacing to use on simulation. Defaults to $sim.default_magnet_spacing')
	magnet_height := fp.float('magnet-height', 0, sim.default_magnet_height, 'magnet height to use on simulation. Defaults to $sim.default_magnet_height')
	magnet_strength := fp.float('magnet-strength', 0, sim.default_magnet_strength, 'magnet strength to use on simulation. Defaults to $sim.default_magnet_strength')
	gravity := fp.float('gravity', 0, sim.default_gravity, 'gravity to use on simulation. Defaults to $sim.default_gravity')

	fp.finalize() or {
		println(fp.usage())
		return error('none')
	}

	params := sim.sim_params(
		rope_length: rope_length
		bearing_mass: bearing_mass
		magnet_spacing: magnet_spacing
		magnet_height: magnet_height
		magnet_strength: magnet_strength
		gravity: gravity
	)

	grid := sim.new_grid_settings(
		width: width
		height: height
	)

	args := ParallelArgs{
		params: params
		filename: filename
		grid: grid
		workers: get_workers(workers, extra_workers)
	}

	sim.log('$args')

	return args
}

[inline]
fn get_workers(workers int, extra_workers int) int {
	result := if workers + extra_workers <= args.max_parallel_workers {
		workers
	} else {
		args.max_parallel_workers - extra_workers
	}

	return math.max(1, result)
}
