module main

import json

pub enum PlatformType {
	unknown
	osx
	ubuntu
	alpine
}

pub enum CPUType {
	unknown
	intel
	arm
	intel32
	arm32
}

@[heap]
pub struct Node {
pub:
	name string = 'mymachine'
pub mut:
	platform    PlatformType
	cputype     CPUType
	done        map[string]string
	environment map[string]string
}

pub fn (mut node Node) save() ! {
	data := json.encode(node)
	dump(data)
}

fn test_encode_with_mut_struct() {
	mut n := Node{
		platform: .osx
		cputype:  .unknown
	}
	n.save() or { panic(err) }
}
