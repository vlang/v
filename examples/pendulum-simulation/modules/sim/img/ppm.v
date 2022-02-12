module img

import gx
import os
import sim

[params]
pub struct ImageSettings {
pub:
	width      int = sim.default_width
	height     int = sim.default_height
	cache_size int = 200
}

pub fn new_image_settings(settings ImageSettings) ImageSettings {
	return ImageSettings{
		...settings
	}
}

pub fn image_settings_from_grid(grid sim.GridSettings) ImageSettings {
	return ImageSettings{
		width: grid.width
		height: grid.height
	}
}

pub fn (s ImageSettings) to_grid_settings() sim.GridSettings {
	return sim.GridSettings{
		width: s.width
		height: s.height
	}
}

pub struct PPMWriter {
mut:
	file       os.File
	cache      []byte
	cache_size int
}

pub fn ppm_writer_for_fname(fname string, settings ImageSettings) ?&PPMWriter {
	mut writer := &PPMWriter{
		cache_size: settings.cache_size
		cache: []byte{cap: settings.cache_size}
	}
	writer.start_for_file(fname, settings) ?
	return writer
}

pub fn (mut writer PPMWriter) start_for_file(fname string, settings ImageSettings) ? {
	writer.file = os.create(fname) ?
	writer.file.writeln('P6 $settings.width $settings.height 255') ?
}

pub fn (mut writer PPMWriter) handle_pixel(p gx.Color) ? {
	if writer.cache.len >= writer.cache_size {
		writer.write() ?
		writer.flush() ?
	}
	writer.cache << [p.r, p.g, p.b]
}

pub fn (mut writer PPMWriter) flush() ? {
	writer.cache.clear()
}

pub fn (mut writer PPMWriter) write() ? {
	writer.file.write(writer.cache) ?
}

pub fn (mut writer PPMWriter) close() {
	writer.file.close()
}
