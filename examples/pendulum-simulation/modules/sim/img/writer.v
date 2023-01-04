module img

import gx
import sim

pub struct ValidColor {
	gx.Color
pub mut:
	valid bool
}

pub struct ImageWritter {
	settings ImageSettings
pub mut:
	writer        PPMWriter
	current_index int
	buffer        []ValidColor
}

pub fn new_image_writer(mut writer PPMWriter, settings ImageSettings) &ImageWritter {
	total_pixels := settings.width * settings.height
	mut buffer := []ValidColor{len: total_pixels, init: ValidColor{
		valid: false
	}}
	return &ImageWritter{
		writer: writer
		settings: settings
		buffer: buffer
	}
}

pub fn (mut iw ImageWritter) handle(result sim.SimResult) !int {
	total_pixels := iw.settings.width * iw.settings.height

	// find the closest magnet
	iw.buffer[result.id].Color = compute_pixel(result)
	iw.buffer[result.id].valid = true

	for iw.current_index < total_pixels && iw.buffer[iw.current_index].valid {
		iw.writer.handle_pixel(iw.buffer[iw.current_index].Color) or {
			sim.log(@MOD + '.' + @FN + ': pixel handler failed. Error ${err}')
			break
		}
		iw.current_index++
	}

	if iw.current_index == total_pixels {
		iw.writer.write() or { panic('Could not write image') }
		return error('none')
	}

	return iw.current_index
}

pub fn compute_pixel(result sim.SimResult) gx.Color {
	closest_to_m1 := result.magnet1_distance < result.magnet2_distance
		&& result.magnet1_distance < result.magnet3_distance
	closest_to_m2 := result.magnet2_distance < result.magnet1_distance
		&& result.magnet2_distance < result.magnet3_distance

	if closest_to_m1 {
		return gx.red
	} else if closest_to_m2 {
		return gx.green
	} else {
		return gx.blue
	}
}
