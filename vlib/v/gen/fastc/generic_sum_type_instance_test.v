module fastc

import os
import v.cmdexec

// A generic sum type (`type MessageData[T] = Small[T] | Stable[T] | Alloc[T]`) is copied per
// instance like a generic struct, with its methods; a method that repeats the receiver's
// parameter (`slice[T]()`) is specialized too.
fn test_fastc_generic_sum_type_gets_instances() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generic_sum_type_gets_instances_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'ds')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'ds', 'ds.v'), 'module ds

pub type MessageData[T] = MessageDataSmall[T]
	| MessageDataStable[T]
	| MessageDataAlloc[T]

pub struct MessageDataSmall[T] {
pub:
	data []T
}

pub struct MessageDataStable[T] {
pub:
	data []T
}

pub struct MessageDataAlloc[T] {
pub mut:
	data []T
}

pub enum Kind as u8 {
	small
	stable
	alloc
}

pub fn MessageData.init[T](small_size int, data []T) MessageData[T] {
	if data.len <= small_size {
		return MessageData[T](MessageDataSmall[T]{
			data: data.clone()
		})
	}
	return MessageData[T](MessageDataAlloc[T]{
		data: data.clone()
	})
}

pub fn MessageData.from_stable[T](data []T) MessageData[T] {
	return MessageData[T](MessageDataStable[T]{
		data: data
	})
}

pub fn (self MessageData[T]) kind[T]() Kind {
	return match self {
		MessageDataSmall[T] { .small }
		MessageDataStable[T] { .stable }
		MessageDataAlloc[T] { .alloc }
	}
}

pub fn (self MessageData[T]) slice[T]() []T {
	return match self {
		MessageDataSmall[T] { self.data }
		MessageDataStable[T] { self.data }
		MessageDataAlloc[T] { self.data }
	}
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import ds

struct WriteReq {
mut:
	payload ds.MessageData[u8]
}

fn (req &WriteReq) message() string {
	data := req.payload.slice()
	return match req.payload.kind() {
		.small { 'small \${data.bytestr()}' }
		.stable { 'stable \${data.bytestr()}' }
		.alloc { 'alloc \${data.bytestr()}' }
	}
}

fn main() {
	a := WriteReq{
		payload: ds.MessageData.from_stable[u8]('hello'.bytes())
	}
	b := WriteReq{
		payload: ds.MessageData.init[u8](2, 'hi'.bytes())
	}
	c := WriteReq{
		payload: ds.MessageData.init[u8](2, 'long'.bytes())
	}
	println(a.message())
	println(b.message())
	println(c.message())
	ints := ds.MessageData.init[int](1, [1, 2, 3])
	println(ints.slice().len)
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'stable hello\nsmall hi\nalloc long\n3', run.output
}
