/**********************************************************************
*
* File scanner
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* TODO:
**********************************************************************/
import os

// STBI supported format
//        STBI_NO_JPEG *
//        STBI_NO_PNG  *
//        STBI_NO_BMP  *
//        STBI_NO_PSD
//        STBI_NO_TGA  *
//        STBI_NO_GIF  *
//        STBI_NO_HDR  *
//        STBI_NO_PIC  *
//        STBI_NO_PNM  *

/******************************************************************************
*
* Struct and Enums
*
******************************************************************************/
enum Item_type {
	file = 0
	folder
	// archive format
	zip = 16
	archive_file
	// graphic format, MUST stay after the other types!!
	bmp = 32
	jpg
	png
	gif
	tga
	ppm
	pgm
	pic
	hdr
}

pub struct Item {
pub mut:
	path                 string
	name                 string
	size                 u64
	i_type               Item_type = .file
	container_index      int  // used if the item is in a container (.zip, .rar, etc)
	container_item_index int  // index in the container if the item is contained
	need_extract         bool // if true need to extraction from the container
	drawable             bool // if true the image can be showed
	n_item               int  //
	rotation             int  // number of rotation of PI/2
}

struct Item_list {
pub mut:
	lst        []Item
	path_sep   string
	item_index int = -1 // image currently shown
	n_item     int  // number of images scanned
	loaded     bool // flag that indicate that the list is ready to  be used
}

/******************************************************************************
*
* Utility functions
*
******************************************************************************/
[inline]
fn modulo(x int, n int) int {
	return (x % n + n) % n
}

[inline]
fn get_extension(x string) Item_type {
	// 4 char extension check
	if x.len > 4 {
		ext4 := x[x.len - 4..].to_lower()
		match ext4 {
			// containers
			'.zip' { return .zip }
			// graphic formats
			'.jpg' { return .jpg }
			'.png' { return .png }
			'.bmp' { return .bmp }
			'.gif' { return .gif }
			'.tga' { return .tga }
			'.ppm' { return .ppm }
			'.pgm' { return .pgm }
			'.pic' { return .pic }
			'.hdr' { return .hdr }
			else {}
		}
	}
	// 5 char extension check
	if x.len > 5 {
		ext5 := x[x.len - 5..].to_lower()
		if ext5 == '.jpeg' {
			{
				return .jpg
			}
		}
	}
	return .file
}

[inline]
fn is_image(x Item_type) bool {
	if int(x) >= int(Item_type.bmp) {
		return true
	}
	return false
}

[inline]
fn is_container(x Item_type) bool {
	if x in [.zip, .folder] {
		return true
	}
	return false
}

[inline]
fn (item_list Item_list) is_inside_a_container() bool {
	if item_list.lst.len <= 0 || item_list.n_item <= 0 {
		return false
	}
	return item_list.lst[item_list.item_index].need_extract
}

[inline]
fn (item_list Item_list) get_file_path() string {
	if item_list.lst.len <= 0 || item_list.n_item <= 0 {
		return ''
	}
	if item_list.lst[item_list.item_index].path.len > 0 {
		return '${item_list.lst[item_list.item_index].path}$item_list.path_sep${item_list.lst[item_list.item_index].name}'
	}
	return item_list.lst[item_list.item_index].name
}

/******************************************************************************
*
* Scan functions
*
******************************************************************************/
fn (mut item_list Item_list) scan_folder(path string, in_index int) ! {
	println('Scanning [$path]')
	mut folder_list := []string{}
	lst := os.ls(path)!

	// manage the single files
	for c, x in lst {
		pt := '$path$item_list.path_sep$x'
		mut item := Item{
			path: path
			name: x
			container_index: in_index
			container_item_index: c
		}
		if os.is_dir(pt) {
			folder_list << x
		} else {
			ext := get_extension(x)
			if ext == .zip {
				item.i_type = .zip
				item_list.lst << item
				item_list.scan_zip(pt, item_list.lst.len - 1)!
				continue
			}
			if is_image(ext) == true {
				item_list.n_item += 1
				item.n_item = item_list.n_item
				item.i_type = ext
				item.drawable = true
				item_list.lst << item
				continue
			}
		}
	}

	// manage the folders
	for x in folder_list {
		pt := '$path$item_list.path_sep$x'
		item := Item{
			path: path
			name: x
			i_type: .folder
		}
		item_list.lst << item
		item_list.scan_folder(pt, item_list.lst.len - 1)!
	}
	// println(item_list.lst.len)
	// println("==================================")
}

fn (item_list Item_list) print_list() {
	println('================================')
	for x in item_list.lst {
		if x.i_type == .folder {
			print('[]')
		}
		if x.i_type == .zip {
			print('[ZIP]')
		}
		println('$x.path => $x.container_index $x.container_item_index $x.name ne:$x.need_extract')
	}
	println('n_item: $item_list.n_item index: $item_list.item_index')
	println('================================')
}

fn (mut item_list Item_list) get_items_list(args []string) {
	item_list.loaded = false
	println('Args: $args')

	item_list.path_sep = $if windows { '\\' } $else { '/' }
	for x in args {
		// scan folder
		if os.is_dir(x) {
			mut item := Item{
				path: x
				name: x
				container_index: item_list.lst.len
				i_type: .folder
			}
			item_list.lst << item
			item_list.scan_folder(x, item_list.lst.len - 1) or {
				eprintln('ERROR: scanning folder [$x]!')
				continue
			}
		} else {
			mut item := Item{
				path: ''
				name: x
				container_index: -1
			}
			ext := get_extension(x)
			// scan .zip
			if ext == .zip {
				item.i_type = .zip
				item_list.lst << item
				item_list.scan_zip(x, item_list.lst.len - 1) or {
					eprintln('ERROR: scanning zip [$x]!')
					continue
				}
				continue
			}
			// single images
			if is_image(ext) == true {
				item_list.n_item += 1
				item.n_item = item_list.n_item
				item.i_type = ext
				item.drawable = true
				item_list.lst << item
				continue
			}
		}
	}
	// debug call for list all the loaded items
	// item_list.print_list()

	println('Items: $item_list.n_item')
	println('Scanning done.')

	item_list.get_next_item(1)
	item_list.loaded = true
}

/******************************************************************************
*
* Navigation functions
*
******************************************************************************/
fn (mut item_list Item_list) get_next_item(in_inc int) {
	// if empty exit
	if item_list.lst.len <= 0 || item_list.n_item <= 0 {
		return
	}

	inc := if in_inc > 0 { 1 } else { -1 }
	mut i := item_list.item_index + in_inc
	i = modulo(i, item_list.lst.len)
	start := i
	for {
		if item_list.lst[i].drawable == true {
			item_list.item_index = i
			break
		}
		i = i + inc
		i = modulo(i, item_list.lst.len)
		// if we are in a loop break it
		if i == start {
			break
		}
	}
	// println("Found: ${item_list.item_index}")
}

fn (mut item_list Item_list) go_to_next_container(in_inc int) {
	// if empty exit
	if item_list.lst.len <= 0 || item_list.n_item <= 0 {
		return
	}
	inc := if in_inc > 0 { 1 } else { -1 }
	mut i := item_list.item_index + in_inc
	i = modulo(i, item_list.lst.len)
	start := i
	for {
		// check if we found a folder
		if is_container(item_list.lst[i].i_type) == true
			&& i != item_list.lst[item_list.item_index].container_index {
			item_list.item_index = i
			item_list.get_next_item(1)
			break
		}
		// continue to search
		i = i + inc
		i = modulo(i, item_list.lst.len)
		// if we are in a loop break it
		if i == start {
			break
		}
	}
}

/******************************************************************************
*
* Other functions
*
******************************************************************************/
[inline]
fn (mut item_list Item_list) rotate(in_inc int) {
	item_list.lst[item_list.item_index].rotation += in_inc
	if item_list.lst[item_list.item_index].rotation >= 4 {
		item_list.lst[item_list.item_index].rotation = 0
	}
}
