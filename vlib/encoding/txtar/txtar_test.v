import os
import encoding.txtar

// txtar implements a trivial text-based file archive format,
// Ported from https://cs.opensource.google/go/x/tools/+/master:txtar/archive.go
// It has some convenience additions (the txtar.pack/1 and txtar.unpack/1 functions).
// See also the README.md file in this folder.

fn test_parse_nothing() {
	dump(@LOCATION)
	content := ''
	a := txtar.parse(content)
	assert a.str() == content
	assert a.comment == ''
	assert a.files.len == 0
}

fn test_parse_no_files() {
	dump(@LOCATION)
	content := 'some
comments
'
	a := txtar.parse(content)
	assert a.str() == content
	assert a.comment != ''
	assert a.files.len == 0
}

fn test_parse_no_comments() {
	dump(@LOCATION)
	content := '-- abc.xyz --
line1
-- another.txt --
z line1
'
	a := txtar.parse(content)
	assert a.str() == content
	assert a.comment == ''
	assert a.files.len == 2
	assert a.files[0].path == 'abc.xyz'
	assert a.files[0].content.split_into_lines() == ['line1']
	assert a.files[1].path == 'another.txt'
	assert a.files[1].content.split_into_lines() == ['z line1']
}

const simple_archive_content = 'some

comments on
several lines
-- abc.xyz --
line1
line2
-- empty --
-- folder2/another.txt --
z line1
z line2
z line3
-- folder3/final.txt --
'

fn test_parse() {
	dump(@LOCATION)
	a := txtar.parse(simple_archive_content)
	assert a.str() == simple_archive_content
	assert a.comment != ''
	assert a.comment.split_into_lines().len == 4
	assert a.comment.contains('\n\n')
	assert a.files.len == 4
	assert a.files[0].path == 'abc.xyz'
	assert a.files[0].content.split_into_lines() == ['line1', 'line2']
	assert a.files[1].path == 'empty'
	assert a.files[1].content == ''
	assert a.files[2].path == 'folder2/another.txt'
	assert a.files[2].content.split_into_lines() == ['z line1', 'z line2', 'z line3']
	assert a.files[3].path == 'folder3/final.txt'
	assert a.files[3].content == ''
}

fn test_parse_file() {
	dump(@LOCATION)
	fpath := os.join_path(os.vtmp_dir(), 'txtar.txt')
	defer {
		os.rm(fpath) or {}
	}
	os.write_file(fpath, simple_archive_content)!
	a := txtar.parse_file(fpath)!
	assert a.comment != ''
	assert a.files.len == 4
	assert a.str() == simple_archive_content
}

fn test_unpack_to_folder_then_pack_same_folder() {
	dump(@LOCATION)
	folder := os.join_path(os.vtmp_dir(), 'txtar_folder')
	a := txtar.parse(simple_archive_content)

	txtar.unpack(a, folder)!
	check_folder(folder)
	os.rmdir_all(folder) or {}

	a.unpack_to(folder)!
	check_folder(folder)

	b := txtar.pack(folder, 'abc')!
	os.rmdir_all(folder) or {}

	assert a.comment != b.comment
	assert b.comment == 'abc'
	assert b.files.len == a.files.len
	ofiles := a.files.sorted(|x, y| x.path < y.path)
	pfiles := b.files.sorted(|x, y| x.path < y.path)
	assert ofiles == pfiles
}

fn check_folder(folder string) {
	assert os.is_file(os.join_path(folder, 'empty'))
	assert os.is_file(os.join_path(folder, 'folder2/another.txt'))
	assert os.is_file(os.join_path(folder, 'folder3/final.txt'))
}
