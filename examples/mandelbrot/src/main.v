module main

import os
import time
import complex as cplx

const image_width = 4400
const image_height = 3520
const max_iteration = 255
const x_width = 4.0
const y_with = 3.0
const x_start = -2.2
const y_start = -1.5
const x_step = x_width / image_width
const y_step = y_with / image_height

fn main() {
	mut index := 0
	mut x := 0.0
	mut y := y_start
	mut output := []u8{len: int(image_width * image_height), cap: int(image_width * image_height), init: 0}

	println('Generating ${image_width}x${image_height} Mandelbrot\'s set. This may take some times...')
	mut stop_watch := time.new_stopwatch()

	for py := 0; py < image_height; py++ {
		x = x_start

		for px := 0; px < image_width; px++ {
			mut c := cplx.Complex{x, y}
			mut pt := cplx.Complex{0, 0}
			mut v := 0

			for pt.mod() < 4.0 && v < max_iteration {
				pt = pt * pt
				pt = pt + c
				v++
			}

			output[index] = u8(max_iteration - v)
			index++
			x += x_step
		}
		y += y_step
	}

	stop_watch.stop()
	println('Image generated in ${stop_watch.elapsed().milliseconds()} milliseconds.')

	save_image('Mandelbrot.pgm', output, image_width, image_height) or {
		println(err)
		return
	}

	println('Done.')
}

/**
 * Save our computed Mandelbrot's set.
 * This method returns and Error or nothing.
 */
fn save_image(filename string, data []u8, width int, height int) ! {
	println('Saving output to file: ${filename}')

	mut file := os.open_file(filename, 'w', os.s_irgrp | os.s_irusr | os.s_iwusr) or {
		return error('Error creating file : ${err}')
	}

	defer {
		file.close()
	}

	header := 'P5 ${image_width} ${image_height} 255 \n'
	_ := file.write_string(header) or { return error('Error writing string : ${err}') }

	_ := file.write(data) or { return error('Error writing data : ${err}') }
}
