import os
import stbi

const vroot = @VEXEROOT
const tfolder = os.join_path(os.vtmp_dir(), 'stbi')
const logo_path = os.join_path(vroot, 'examples/assets/logo.png')
const background_path = os.join_path(vroot, 'examples/flappylearning/assets/img/background.png')

fn testsuite_begin() {
	os.mkdir_all(tfolder) or {}
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

fn test_stbi_read_write() {
	println('Source path: ${logo_path}')
	d_s := stbi.load(logo_path) or { panic(err) }
	println('Image source data:\n ${d_s}')

	out_path := os.join_path(tfolder, 'test.png')
	println('Out path: ${out_path}')
	stbi.stbi_write_png(out_path, d_s.width, d_s.height, 4, d_s.data, d_s.width * 4) or {
		panic(err)
	}

	d_d := stbi.load(out_path) or { panic(err) }
	println('Image dest data:\n ${d_d}')

	assert d_s.width == d_d.width
	assert d_s.height == d_d.height
	assert d_s.nr_channels == d_d.nr_channels
	assert d_s.original_nr_channels == 4

	mut v_s := unsafe { &u32(d_s.data) }
	mut v_d := unsafe { &u32(d_d.data) }
	mut delta := i64(0)
	for index in 0 .. (d_d.width * d_d.width) {
		unsafe {
			delta += v_s[index] - v_d[index]
		}
	}
	assert 0 == delta
	os.rm(out_path) or {}
}

fn test_stbi_resize() {
	println('Source path: ${logo_path}')
	d_s := stbi.load(logo_path) or { panic(err) }
	println('Image source data:\n ${d_s}')

	new_width, new_height := 100, 100

	d_r := stbi.resize_uint8(d_s, new_width, new_height) or { panic(err) }
	assert d_r.original_nr_channels == 4
	println('Resized Image source data:\n ${d_s}')

	out_path := os.join_path(tfolder, 'test.png')
	println('Out path: ${out_path}')
	stbi.stbi_write_png(out_path, d_r.width, d_r.height, 4, d_r.data, d_r.width * 4) or {
		panic(err)
	}

	d_d := stbi.load(out_path) or { panic(err) }
	println('Image dest data:\n ${d_d}')

	assert d_d.width == new_width
	assert d_d.height == new_height
	assert d_d.nr_channels == d_r.nr_channels
	assert d_d.original_nr_channels == 4
	os.rm(out_path) or {}
}

fn test_load_image_with_channels_different_than_4() {
	img := stbi.load(background_path)!
	assert img.nr_channels == 4, 'by default, stbi.load should convert images to 4 channels'
	assert img.original_nr_channels == 3, 'the default, should not affect the img.original_nr_channels field; it should be based solely on the original image data on disk'

	img_resized := stbi.resize_uint8(img, 128, 128)!
	assert img_resized.nr_channels == 4
	assert img_resized.original_nr_channels == 3

	img3 := stbi.load(background_path, desired_channels: 0)!
	assert img3.nr_channels == 3, 'stbi.load, with desired_channels: 0, should return an image, without any conversion. the nr_channels should be determined by the image data'
	assert img.original_nr_channels == 3, 'the default, should not affect the img.original_nr_channels field; it should be based solely on the original image data on disk'

	img3_resized := stbi.resize_uint8(img3, 128, 128)!
	assert img3_resized.nr_channels == 3
	assert img3_resized.original_nr_channels == 3
}
