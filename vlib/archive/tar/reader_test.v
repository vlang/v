module tar

import os

fn testsuite_begin() {
	os.chdir(@VMODROOT) or {}
}

const testdata = 'vlib/archive/tar/testdata'

// files copied from golang: https://github.com/golang/go/blob/master/src/archive/tar/testdata/file-and-dir.tar	
fn test_golang_testdata() {
	// [ ] dir       | 0 bytes | folder
	// [ ] small.txt | 5 bytes | file
	r1 := new_test_reader('file-and-dir.tar', false)!
	assert r1.dirs[0] == 'dir/'
	assert r1.files['small.txt'] == 5
	assert r1.data['small.txt'] == 'Kilts'.bytes()
	assert r1.other[0] == 'block:4 special:blank_1 continue'
	assert r1.other[1] == 'block:5 special:blank_2 end_archive'

	// [ ] small.txt  |  5 bytes | file
	// [ ] small2.txt | 11 bytes | file
	r2 := new_test_reader('gnu.tar', false)!
	assert r2.dirs.len == 0
	assert r2.files['small.txt'] == 5
	assert r2.files['small2.txt'] == 11
	assert r2.data['small.txt'] == 'Kilts'.bytes()
	assert r2.data['small2.txt'] == 'Google.com\n'.bytes()

	// [ ] h1<?><?><?><?>bye | 0 bytes
	r3 := new_test_reader('gnu-not-utf8.tar', false)!
	r3_filename := [u8(`h`), `i`, 0x80, 0x81, 0x82, 0x83, `b`, `y`, `e`].bytestr()
	r3_file_len := r3.files[r3_filename] or { assert false, 'file not found: ${r3_filename}' }
	assert r3_file_len == 0
	assert r3.other.len == 2

	// [ ] 0123456789 | 0 bytes
	r4 := new_test_reader('gnu-long-nul.tar', false)!
	assert r4.dirs.len == 0
	r4_filename := '0123456789'
	r4_file_len := r4.files[r4_filename] or {
		assert false, 'file ${r4_filename} not found in ${r4.files.keys()}'
	}
	assert r4_file_len == 0
	assert r4.other[0] == 'block:1 special:long_name size:161'
	assert r4.other[1] == 'block:2 special:long_name data_part:161'

	// [ ] ☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹ | 0 bytes
	r5 := new_test_reader('gnu-utf8.tar', false)!
	r5_filename := '☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹☺☻☹'
	r5_file_len := r5.files[r5_filename] or { assert false, 'file not found: ${r5_filename}' }
	assert r5_file_len == 0
	assert r5.other[0] == 'block:1 special:long_name size:163'
	assert r5.other[1] == 'block:2 special:long_name data_part:163'
}

fn test_long_long_short() {
	// test long path (human) substitute another long path (chimp) then a normal path (cat)
	r1 := new_test_reader_gz('life.tar.gz', false)!

	mammal := 'life/Animalia/Chordata/Mammalia'
	human := '${mammal}/Primates_Haplorhini_Simiiformes/Hominidae_Homininae_Hominini/Homo/Homo sapiens.txt'
	chimp := '${mammal}/Primates_Haplorhini_Simiiformes/Hominidae_Homininae_Hominini/Pan/Pan troglodytes.txt'
	cat := '${mammal}/Carnivora_Feliformia/Felidae_Felinae/Felis/Felis catus.txt'
	assert human.len > 100
	assert chimp.len > 100
	assert cat.len <= 100
	assert r1.files[human] == 35
	assert r1.files[chimp] == 40
	assert r1.files[cat] == 33
	assert r1.texts[human] == 'https://en.wikipedia.org/wiki/Human'
	assert r1.texts[chimp] == 'https://en.wikipedia.org/wiki/Chimpanzee'
	assert r1.texts[cat] == 'https://en.wikipedia.org/wiki/Cat'
}

struct TestReader {
	debug bool
mut:
	dirs  []string
	files map[string]u64
	data  map[string][]u8
	texts map[string]string
	other []string

	last_file string
	last_data []u8
}

// new_test_reader reads files *.tar
fn new_test_reader(tar_file string, debug bool) !&TestReader {
	mut reader := &TestReader{
		debug: debug
	}
	mut untar := Untar{
		reader: reader
	}
	all_blocks := os.read_bytes('${testdata}/${tar_file}')!
	untar.read_all_blocks(all_blocks)!
	return reader
}

// new_test_reader_gz reads files *.tar.gz
fn new_test_reader_gz(tar_gz_file string, debug bool) !&TestReader {
	mut reader := &TestReader{
		debug: debug
	}
	mut untar := Untar{
		reader: reader
	}
	mut decompressor := new_decompresor(untar)
	tar_gz := os.read_bytes('${testdata}/${tar_gz_file}')!
	decompressor.read_all(tar_gz)!

	return reader
}

fn (mut t TestReader) dir_block(mut read Read, size u64) {
	t.dirs << read.get_path()
	if t.debug {
		println('DIR   #${read.get_block_number()} ${read.get_path()}')
	}
}

fn (mut t TestReader) file_block(mut read Read, size u64) {
	t.last_file = read.get_path()
	t.files[t.last_file] = size
	if t.debug {
		println('FILE  #${read.get_block_number()} ${read.get_path()}')
	}
}

fn (mut t TestReader) data_block(mut read Read, data []u8, pending int) {
	path := read.get_path()
	if t.debug {
		println('DATA  #${read.get_block_number()} ${path}')
	}
	if t.last_file == path {
		t.last_data << data
		if pending == 0 {
			t.data[t.last_file] = t.last_data.clone()
			t.texts[path] = t.last_data.bytestr()
			if t.debug {
				println('TEXT  #${read.get_block_number()} ${t.last_data.bytestr()}')
			}
			t.last_file = ''
			t.last_data.clear()
		}
	}
}

fn (mut t TestReader) other_block(mut read Read, details string) {
	t.other << 'block:${read.block_number} special:${read.special} ${details}'
	if t.debug {
		println('OTHER #${read.get_block_number()} special:${read.special} ${details}')
	}
}
