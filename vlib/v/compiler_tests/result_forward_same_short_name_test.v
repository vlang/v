import os

const vexe = @VEXE

fn test_result_payloads_sharing_a_short_name_are_not_forwarded() {
	dir := os.join_path(os.vtmp_dir(), 'v3_result_forward_${os.getpid()}')
	os.rmdir_all(dir) or {}
	defer {
		os.rmdir_all(dir) or {}
	}
	os.mkdir_all(os.join_path(dir, 'link')) or { panic(err) }
	os.mkdir_all(os.join_path(dir, 'nether')) or { panic(err) }
	os.write_file(os.join_path(dir, 'v.mod'), "Module{ name: 'repro' }\n") or { panic(err) }
	os.write_file(os.join_path(dir, 'link', 'link.v'), 'module link

pub interface Listener {
	addr() string
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'nether', 'nether.v'), "module nether

import link

pub struct Listener {}

pub fn (l &Listener) addr() string {
	return 'nether'
}

pub fn listen() !&Listener {
	return &Listener{}
}

fn refuse() !&Listener {
	return error('no listener')
}

pub fn listener() !link.Listener {
	return listen()!
}

pub fn failing() !link.Listener {
	return refuse()!
}

pub fn forward() !&Listener {
	return listen()!
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'main.v'), 'module main

import nether

fn main() {
	l := nether.listener() or { panic(err) }
	println(l.addr())
	nether.failing() or { println(err) }
	f := nether.forward() or { panic(err) }
	println(f.addr())
}
') or {
		panic(err)
	}
	res := os.execute('${os.quoted_path(vexe)} -new-compiler run ${os.quoted_path(dir)}')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['nether', 'no listener', 'nether'], res.output
}
