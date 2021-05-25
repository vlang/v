module html

const (
	null_element = int(0x80000000)
)

struct Stack {
mut:
	elements []int
	size     int
}

[inline]
fn is_null(data int) bool {
	return data == html.null_element
}

[inline]
fn (stack Stack) is_empty() bool {
	return stack.size <= 0
}

fn (stack Stack) peek() int {
	return if !stack.is_empty() { stack.elements[stack.size - 1] } else { html.null_element }
}

fn (mut stack Stack) pop() int {
	mut to_return := html.null_element
	if !stack.is_empty() {
		to_return = stack.elements[stack.size - 1]
		stack.size--
	}
	return to_return
}

fn (mut stack Stack) push(item int) {
	if stack.elements.len > stack.size {
		stack.elements[stack.size] = item
	} else {
		stack.elements << item
	}
	stack.size++
}

struct BTree {
mut:
	all_tags     []Tag
	node_pointer int
	childrens    [][]int
	parents      []int
}

fn (mut btree BTree) add_children(tag Tag) int {
	btree.all_tags << tag
	if btree.all_tags.len > 1 {
		for btree.childrens.len <= btree.node_pointer {
			mut temp_array := btree.childrens.clone()
			temp_array << []int{}
			btree.childrens = temp_array
		}
		btree.childrens[btree.node_pointer] << btree.all_tags.len - 1
		for btree.parents.len < btree.all_tags.len {
			mut temp_array := btree.parents.clone()
			temp_array << 0
			btree.parents = temp_array
		}
		btree.parents[btree.all_tags.len - 1] = btree.node_pointer
	}
	return btree.all_tags.len - 1
}

[inline]
fn (btree BTree) get_children() []int {
	return btree.childrens[btree.node_pointer]
}

[inline]
fn (btree BTree) get_parent() int {
	return btree.parents[btree.node_pointer]
}

[inline]
fn (btree BTree) get_stored() Tag {
	return btree.all_tags[btree.node_pointer]
}

fn (mut btree BTree) move_pointer(to int) {
	if to < btree.all_tags.len {
		btree.node_pointer = to
	}
}
