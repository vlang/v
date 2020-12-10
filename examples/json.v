import json

struct User {
	name          string
	age           int
mut:
	is_registered bool
}

fn main() {
	s := '[{ "name":"Frodo", "age":25}, {"name":"Bobby", "age":10}]'
	mut users := json.decode([]