import toml

// Nested structs used to be limited to a single level, see
// https://github.com/vlang/v/issues/18110. They now decode at any depth.

struct Leaf {
	z int
}

struct Branch {
	y    int
	leaf Leaf
}

struct Tree {
	x      int
	branch Branch
}

struct Forest {
	trees  []Tree
	by_key map[string]Tree
}

const toml_text = 'x = 1

[branch]
y = 2

[branch.leaf]
z = 3
'

fn test_decode_nested_struct() {
	t := toml.decode[Tree](toml_text) or { panic(err) }
	assert t.x == 1
	assert t.branch.y == 2
	assert t.branch.leaf.z == 3
}

fn test_encode_nested_struct() {
	t := Tree{1, Branch{2, Leaf{3}}}
	assert toml.decode[Tree](toml.encode[Tree](t))! == t
}

fn test_decode_nested_struct_in_array() {
	f := toml.decode[Forest]('trees = [{ x = 1, branch = { y = 2, leaf = { z = 3 } } }]') or {
		panic(err)
	}
	assert f.trees.len == 1
	assert f.trees[0].x == 1
	assert f.trees[0].branch.y == 2
	assert f.trees[0].branch.leaf.z == 3
}

fn test_decode_nested_struct_in_map() {
	f := toml.decode[Forest]('[by_key.main]\nx = 1\n[by_key.main.branch]\ny = 2\n[by_key.main.branch.leaf]\nz = 3\n') or {
		panic(err)
	}
	assert f.by_key['main'].x == 1
	assert f.by_key['main'].branch.y == 2
	assert f.by_key['main'].branch.leaf.z == 3
}
