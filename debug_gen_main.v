import wit
import os

fn main() {
	src := 'package example:demo@0.1.0;

interface types {
	add: func(a: s32, b: s32) -> s32;
}
'
	doc := wit.parse(src) or { panic(err) }
	println('interfaces: ${doc.interfaces.len}')
	if doc.interfaces.len > 0 {
		println('items: ${doc.interfaces[0].items.len}')
		for it in doc.interfaces[0].items {
			println('item kind: ${it.kind}')
		}
	}
}
