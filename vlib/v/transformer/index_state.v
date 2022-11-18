module transformer

struct KeyVal {
	key   string
	value int
}

[if debug_bounds_checking ?]
fn debug_bounds_checking(str string) {
	println(str)
}

// IndexState is used to track the index analysis performed when parsing the code
// `IndexExpr` nodes are annotated with `is_direct`, indicating that the array index can be safely directly accessed.

// The c_gen code check will handle this annotation and perform this direct memory access. The following cases are considered valid for this optimisation:
// 1. the array size is known and has a `len` larger than the index requested
// 2. the array was previously accessed with a higher value which would have reported the issue already
// 3. the array was created from a range expression a := range[10..13] and the offset'ed indexes are safe

// Current limitations:
//  * any function using break/continue or goto/label stopped from being optimised as soon as the relevant AST nodes are found as the code can not be ensured to be sequential
//  * `enum` and `const` indexes are not optimised (they could probably be looked up)
//  * for loops with multiple var in their init and/or inc are not analysed
//  * mut array are not analysed as their size can be reduced, but self-assignment in a single line

pub struct IndexState {
mut:
	// max_index has the biggest array index accessed for then named array
	// so if a[2] was set or read, it will be 2
	// A new array with no .len will recorded as -1 (accessing a[0] would be invalid)
	// the value -2 is used to indicate that the array should not be analysed
	// this is used for a mut array
	max_index map[string]int
	// We need to snapshot when entering `if` and `for` blocks and restore on exit
	// as the statements may not be run. This is managed by indent() & unindent().
	saved_disabled []bool
	saved_key_vals [][]KeyVal
pub mut:
	// on encountering goto/break/continue statements we stop any analysis
	// for the current function (as the code is not linear anymore)
	disabled bool
	level    int
}

// we are remembering the last array accessed and checking if the value is safe
// the node is updated with this information which can then be used by the code generators
fn (mut i IndexState) safe_access(key string, new int) bool {
	$if no_bounds_checking {
		return false
	}
	if i.disabled {
		return false
	}
	old := i.max_index[key] or {
		debug_bounds_checking('${i.level} ${key}.len = ${new}')
		i.max_index[key] = new
		return false
	}
	if new > old {
		if old < -1 {
			debug_bounds_checking('${i.level} ${key}[${new}] unsafe (mut array)')
			return false
		}
		debug_bounds_checking('${i.level} ${key}[${new}] unsafe (index was ${old})')
		i.max_index[key] = new
		return false
	}
	debug_bounds_checking('${i.level} ${key}[${new}] safe (index is ${old})')
	return true
}

// safe_offset returns for a previvous array what was the highest
// offset we ever accessed for that identifier
fn (mut i IndexState) safe_offset(key string) int {
	$if no_bounds_checking {
		return -2
	}
	if i.disabled {
		return -2
	}
	return i.max_index[key] or { -1 }
}

// indent is used for when encountering new code blocks (if, for and functions)
// The code analysis needs to take into consideration blocks of code which
// may not run at runtime (if/for) and therefore even if a new maximum for an
// index access is found on an if branch it can not be used within the parent
// code. The same is true with for blocks. indent() snapshot the current state,
// to allow restoration with unindent()
// Also within a function, analysis must be `disabled` when goto or break are
// encountered as the code flow is then not lineear, and only restart when a
// new function analysis is started.
[if !no_bounds_checking]
fn (mut i IndexState) indent(is_function bool) {
	mut kvs := []KeyVal{cap: i.max_index.len}
	for k, v in i.max_index {
		kvs << KeyVal{k, v}
	}
	i.saved_disabled << i.disabled
	i.saved_key_vals << kvs
	if is_function {
		i.disabled = false
	}
	i.level += 1
}

// restoring the data as it was before the if/for/unsafe block
[if !no_bounds_checking]
fn (mut i IndexState) unindent() {
	i.level -= 1
	mut keys := []string{cap: i.max_index.len}
	for k, _ in i.max_index {
		keys << k
	}
	for k in keys {
		i.max_index.delete(k)
	}
	for saved in i.saved_key_vals.pop() {
		i.max_index[saved.key] = saved.value
	}
	i.disabled = i.saved_disabled.pop()
}
