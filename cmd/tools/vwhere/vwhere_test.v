module main

import os

const test_dir = os.join_path(os.dir(vexe), 'cmd', 'tools', 'vwhere', 'test')

fn test_create_finder() {
	mut fdr := Finder{}

	fdr.configure_from_arguments(['some'])
	assert fdr.symbol == .@fn
	assert fdr.name == 'some'
	assert fdr.visib == .all
	assert fdr.mutab == .any

	fdr.configure_from_arguments(['fn', 'some', '-vis', 'pub'])
	assert fdr.symbol == .@fn
	assert fdr.name == 'some'
	assert fdr.visib == .@pub

	fdr.configure_from_arguments(['method', 'Some.some', '-vis', 'pri'])
	assert fdr.symbol == .method
	assert fdr.receiver == 'Some'
	assert fdr.name == 'some'
	assert fdr.visib == .pri

	fdr.configure_from_arguments(['struct', 'Some', '-mod', 'foo'])
	assert fdr.symbol == .@struct
	assert fdr.name == 'Some'
	assert fdr.modul == 'foo'

	fdr.configure_from_arguments(['interface', 'Some', '-mod', 'foo', '-dir', 'bar'])
	assert fdr.symbol == .@interface
	assert fdr.name == 'Some'
	assert fdr.modul == 'foo'
	assert fdr.dirs == ['bar']

	fdr.configure_from_arguments(['enum', 'Some', '-dir', 'bar', '-dir', 'baz'])
	assert fdr.symbol == .@enum
	assert fdr.name == 'Some'
	assert fdr.dirs == ['bar', 'baz']

	fdr.configure_from_arguments(['const', 'some'])
	assert fdr.symbol == .@const
	assert fdr.name == 'some'

	fdr.configure_from_arguments(['var', 'some', '-mut', 'yes'])
	assert fdr.symbol == .var
	assert fdr.name == 'some'
	assert fdr.mutab == .yes

	fdr.configure_from_arguments(['var', 'some', '-mut', 'not'])
	assert fdr.symbol == .var
	assert fdr.name == 'some'
	assert fdr.mutab == .not

	fdr.configure_from_arguments(['regexp', '.*some.*'])
	assert fdr.symbol == .regexp
	assert fdr.name == '.*some.*'
}

fn test_find_mut_var() {
	args := ['var', 'p_2', '-mut', 'yes', '-dir', test_dir]
	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	assert fdr.matches == [
		Match{
			path: os.join_path(test_dir, 'file_one.v')
			line: 7
			text: "mut p_2 := Programmer{'Programmer', 'Mutable'}"
		},
	]
}

fn test_find_non_mut_var() {
	args := ['var', 'p_1', '-mut', 'not', '-dir', test_dir]
	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	assert fdr.matches == [
		Match{
			path: os.join_path(test_dir, 'file_one.v')
			line: 6
			text: "p_1 := Programmer{'Programmer', 'Inmutable'}"
		},
	]
}

fn test_find_method() {
	args := ['method', 'Programmer.drink', '-dir', test_dir]
	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	assert fdr.matches == [
		Match{
			path: os.join_path(test_dir, 'file_one.v')
			line: 15
			text: 'fn (p Programmer) drink(cups int) string'
		},
	]
}

fn test_find_pub_method() {
	args := ['method', 'Brogrammer.drink', '-vis', 'pub', '-dir', test_dir]
	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	assert fdr.matches == [
		Match{
			path: os.join_path(test_dir, 'file_one.v')
			line: 24
			text: 'pub fn (p Brogrammer) drink(glasses int) string'
		},
	]
}

fn test_find_pri_const() {
	args := ['const', 'y', '-vis', 'pri', '-dir', test_dir]
	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	assert fdr.matches == [
		Match{
			path: os.join_path(test_dir, 'file_two.v')
			line: 5
			text: 'y = 100'
		},
	]
}

fn test_find_pub_enum() {
	args := ['enum', 'Public', '-vis', 'pub', '-dir', test_dir]
	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	assert fdr.matches == [
		Match{
			path: os.join_path(test_dir, 'file_two.v')
			line: 9
			text: 'pub enum Public'
		},
	]
}

fn test_find_pri_enum() {
	args := ['enum', 'Private', '-vis', 'pri', '-dir', test_dir]
	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	assert fdr.matches == [
		Match{
			path: os.join_path(test_dir, 'file_two.v')
			line: 14
			text: 'enum Private'
		},
	]
}

fn test_find_fn() {
	args := ['fn', 'some_function_name', '-dir', test_dir]
	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	assert fdr.matches == [
		Match{
			path: os.join_path(test_dir, 'file_two.v')
			line: 27
			text: 'fn some_function_name(foo string, bar int) string'
		},
	]
}

fn test_find_pub_const_with_mod() {
	args := ['const', 'b', '-vis', 'pub', '-mod', test_dir]
	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	assert fdr.matches == [
		Match{
			path: os.join_path(test_dir, 'nested_mod', 'nested_file.v')
			line: 5
			text: 'b = 60'
		},
	]
}
