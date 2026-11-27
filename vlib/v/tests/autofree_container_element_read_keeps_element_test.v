// vtest vflags: -autofree -gc none
// Reading an element of a map or array, into a local or as a return value, must
// give the reader a copy: the container still holds (and frees) its element.

struct Table {
	name string
	cols []string
}

struct Storage {
mut:
	tables map[string]Table
	list   []Table
}

fn fresh() Storage {
	mut s := Storage{}
	s.tables['t'] = Table{'users'.repeat(2), ['id'.repeat(2)]}
	s.list << Table{'list'.repeat(2), ['c'.repeat(2)]}
	return s
}

fn (s Storage) stored() string {
	return s.tables['t'] or { Table{} }.name
}

fn (s Storage) return_or(id string) Table {
	return s.tables[id] or { Table{} }
}

fn (s Storage) local_or(id string) int {
	t := s.tables[id] or { Table{} }
	return t.name.len
}

fn (s Storage) local_plain(id string) int {
	t := s.tables[id]
	return t.name.len
}

fn return_or_param(tables map[string]Table, id string) Table {
	return tables[id] or { Table{} }
}

fn (s Storage) array_local(i int) string {
	t := s.list[i]
	return t.name
}

fn (s Storage) array_return(i int) Table {
	return s.list[i]
}

fn (s Storage) in_loop() int {
	mut total := 0
	for _ in 0 .. 3 {
		t := s.tables['t'] or { Table{} }
		total += t.name.len
	}
	return total
}

fn test_return_map_value_with_or() {
	s := fresh()
	assert s.return_or('t').name == 'usersusers'
	assert s.stored() == 'usersusers'
}

fn test_map_value_into_local() {
	s := fresh()
	assert s.local_or('t') == 10
	assert s.local_plain('t') == 10
	assert s.stored() == 'usersusers'
}

fn test_return_map_parameter_value_with_or() {
	s := fresh()
	assert return_or_param(s.tables, 't').cols == ['idid']
	assert s.stored() == 'usersusers'
}

fn test_array_element() {
	s := fresh()
	assert s.array_local(0) == 'listlist'
	assert s.array_return(0).cols == ['cc']
	assert s.list[0].name == 'listlist'
}

fn test_read_in_a_loop() {
	s := fresh()
	assert s.in_loop() == 30
	assert s.stored() == 'usersusers'
}
