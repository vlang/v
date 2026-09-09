module main

fn main() {
	backend := $if js_node { 'Node' } $else { 'Other' }
	println('Hello, ${backend} user!')
}
