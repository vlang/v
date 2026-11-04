import os

fn test_inherited_interface_result_returns_ignore_free_function_collisions() {
	root := os.join_path(os.vtmp_dir(), 'inherited_interface_result_return_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	programs := [
		"module main

type Value = int | string

interface Connection {
mut:
	select() ![][]Value
}

interface TransactionalConnection {
	Connection
mut:
	begin() !
}

struct Backend {}

fn (mut backend Backend) select() ![][]Value {
	return [[Value(42), Value('row')]]
}

fn (mut backend Backend) begin() ! {}

struct Inner {
mut:
	conn TransactionalConnection
}

struct Transaction {
mut:
	inner &Inner
}

fn (mut tx Transaction) select() ![][]Value {
	return tx.inner.conn.select()
}

fn select() int { return 99 }

fn main() {
	assert select() == 99
	mut tx := Transaction{
		inner: &Inner{conn: Backend{}}
	}
	rows := tx.select()!
	assert rows[0][0] == Value(42)
	assert rows[0][1] == Value('row')
}
",
		'module main

fn rows() bool { return false }

interface Base[T] {
mut:
    rows() ![][]T
}
interface Child[T] { Base }
interface Leaf[T] { Child }

struct Backend {}
fn (mut backend Backend) rows() ![][]int {
    return [[42, 43]]
}

struct Holder {
mut:
    conn Leaf[int]
}

fn (mut holder Holder) forward() ![][]int {
    return holder.conn.rows()
}
fn main() {
    mut holder := Holder{conn: Backend{}}
    values := holder.forward()!
    assert values == [[42, 43]]
    assert !rows()
}
',
	]
	for i, program in programs {
		source := os.join_path(root, 'case_${i}.v')
		os.write_file(source, program)!
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none', 'run',
			source])
		assert result.exit_code == 0, 'case=${i}: ${result.output}'
	}
}

fn test_inherited_interface_result_return_rejects_incompatible_payload() {
	root := os.join_path(os.vtmp_dir(), 'inherited_interface_result_mismatch_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'mismatch.v')
	os.write_file(source, "module main

type Value = int | string

fn rows() bool { return false }

interface Base {
mut:
    rows() ![][]Value
}
interface Child { Base }
interface Leaf { Child }

struct Backend {}
fn (mut backend Backend) rows() ![][]Value {
    return [[Value(42), Value('row')]]
}

struct Holder {
mut:
    conn Leaf
}

fn (mut holder Holder) forward() ![][]bool {
    return holder.conn.rows()
}
fn main() {
    mut holder := Holder{conn: Backend{}}
    _ := holder.forward()!
}
")!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-o',
		os.join_path(root, 'mismatch.c'), source])
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot use `![][]Value` as type `![][]bool` in return argument'), result.output
}
