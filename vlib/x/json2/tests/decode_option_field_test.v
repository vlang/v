import x.json2
import time

struct Person {
mut:
	name     string
	age      ?int = 20
	birthday time.Time
	deathday ?time.Time
}

fn test_main() {
	resp := '{"name": "Bob", "age": 20, "birthday": "2025-10-12 10:14:52"}'
	person := decoder2.decode[Person](resp)!
	assert '${person}' == "Person{
    name: 'Bob'
    age: Option(20)
    birthday: 2025-10-12 10:14:52
    deathday: Option(none)
}"
}
