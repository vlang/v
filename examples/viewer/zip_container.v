/**********************************************************************
*
* Zip container manager
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* TODO:
**********************************************************************/
import sokol.gfx
import szip

fn (mut il Item_list) scan_zip(path string, in_index int) ? {
	println('Scanning ZIP [$path]')
	mut zp := szip.open(path, szip.CompressionLevel.no_compression, szip.OpenMode.read_only)?
	n_entries := zp.total()?
	// println(n_entries)
	for index in 0 .. n_entries {
		zp.open_entry_by_index(index)?
		is_dir := zp.is_dir()?
		name := zp.name()
		size := zp.size()
		// println("$index ${name} ${size:10} $is_dir")

		if !is_dir {
			ext := get_extension(name)
			if is_image(ext) == true {
				il.n_item += 1
				mut item := Item{
					need_extract: true
					path: path
					name: name.clone()
					container_index: in_index
					container_item_index: index
					i_type: ext
					n_item: il.n_item
					drawable: true
					size: size
				}
				il.lst << item
			}
		}
		// IMPORTANT NOTE: don't close the zip entry before we have used all the items!!
		zp.close_entry()
	}
	zp.close()
}

fn (mut app App) load_texture_from_zip() ?(gfx.Image, int, int) {
	item := app.item_list.lst[app.item_list.item_index]
	// println("Load from zip [${item.path}]")

	// open the zip
	if app.zip_index != item.container_index {
		if app.zip_index >= 0 {
			app.zip.close()
		}
		app.zip_index = item.container_index
		// println("Opening the zip [${item.path}]")
		app.zip = szip.open(item.path, szip.CompressionLevel.no_compression, szip.OpenMode.read_only)?
	}
	// println("Now get the image")
	app.zip.open_entry_by_index(item.container_item_index)?
	zip_entry_size := int(item.size)

	app.resize_buf_if_needed(zip_entry_size)

	app.zip.read_entry_buf(app.mem_buf, app.mem_buf_size)?
	app.zip.close_entry()
	return app.load_texture_from_buffer(app.mem_buf, zip_entry_size)
}
