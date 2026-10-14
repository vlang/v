module datatypes

// btree_min_degree is the B-tree minimum degree: every node except the root
// holds between btree_min_degree-1 and 2*btree_min_degree-1 keys.
const btree_min_degree = 16

// BTreeEntry is one key/value pair yielded by BTree iteration.
pub struct BTreeEntry[K, V] {
pub:
	key   K
	value V
}

@[heap]
struct BTreeNode[K, V] {
mut:
	keys     []K
	values   []V
	children []&BTreeNode[K, V]
	leaf     bool
}

// BTree is an ordered map with logarithmic insert, lookup and delete. Keys
// must have a total order consistent with `<` and `==`; floating-point NaN
// keys are unsupported. Integer, other float and string keys work. It is
// the V-idiomatic equivalent of Rust's `std::collections::BTreeMap`,
// complementing the string-only builtin `SortedMap` and the unordered builtin
// `map` and `datatypes.Set`.
@[heap]
pub struct BTree[K, V] {
mut:
	root &BTreeNode[K, V]
	len  int
}

// new_btree creates an empty BTree.
pub fn new_btree[K, V]() &BTree[K, V] {
	return &BTree[K, V]{
		root: &BTreeNode[K, V]{
			leaf: true
		}
	}
}

// len returns the number of entries.
pub fn (m &BTree[K, V]) len() int {
	return m.len
}

// is_empty reports whether the tree holds no entries.
pub fn (m &BTree[K, V]) is_empty() bool {
	return m.len == 0
}

// get returns the value for key, or none if absent.
pub fn (m &BTree[K, V]) get(key K) ?V {
	return m.root.search(key)
}

// exists reports whether key is present.
pub fn (m &BTree[K, V]) exists(key K) bool {
	_ := m.get(key) or { return false }
	return true
}

// set inserts or replaces key with value.
pub fn (mut m BTree[K, V]) set(key K, value V) {
	if m.get(key) == none {
		m.len++
	}
	r := m.root
	if r.keys.len == 2 * btree_min_degree - 1 {
		mut s := &BTreeNode[K, V]{}
		s.children << r
		m.root = s
		s.split_child(0)
		s.insert_nonfull(key, value)
		return
	}
	r.insert_nonfull(key, value)
}

// delete removes key, returning false if it was absent.
pub fn (mut m BTree[K, V]) delete(key K) bool {
	removed := m.root.remove(key)
	if m.root.keys.len == 0 && !m.root.leaf {
		m.root = m.root.children[0]
	}
	if removed {
		m.len--
	}
	return removed
}

// min_key returns the smallest key, or none if empty.
pub fn (m &BTree[K, V]) min_key() ?K {
	if m.root.keys.len == 0 {
		return none
	}
	mut n := m.root
	for !n.leaf {
		n = n.children[0]
	}
	return n.keys[0]
}

// max_key returns the largest key, or none if empty.
pub fn (m &BTree[K, V]) max_key() ?K {
	if m.root.keys.len == 0 {
		return none
	}
	mut n := m.root
	for !n.leaf {
		n = n.children[n.children.len - 1]
	}
	return n.keys[n.keys.len - 1]
}

// keys returns all keys in ascending order.
pub fn (m &BTree[K, V]) keys() []K {
	mut out := []BTreeEntry[K, V]{}
	m.root.collect_all(mut out)
	mut keys := []K{cap: out.len}
	for e in out {
		keys << e.key
	}
	return keys
}

// range_keys returns the keys k with lo <= k <= hi, in ascending order.
pub fn (m &BTree[K, V]) range_keys(lo K, hi K) []K {
	mut it := m.iterate(lo, hi)
	mut keys := []K{}
	for {
		e := it.next() or { break }
		keys << e.key
	}
	return keys
}

// iterate returns an entry iterator over lo <= k <= hi (snapshot semantics).
pub fn (m &BTree[K, V]) iterate(lo K, hi K) BTreeIter[BTreeEntry[K, V]] {
	mut out := []BTreeEntry[K, V]{}
	m.root.collect_range(lo, hi, mut out)
	return BTreeIter[BTreeEntry[K, V]]{
		entries: out
	}
}

// BTreeIter iterates over BTree entries in ascending order.
pub struct BTreeIter[E] {
mut:
	entries []E
	pos     int
}

// next returns the next entry, or none when exhausted.
pub fn (mut it BTreeIter[E]) next() ?E {
	if it.pos >= it.entries.len {
		return none
	}
	e := it.entries[it.pos]
	it.pos++
	return e
}

// BTreeSet is an ordered set built on BTree.
@[heap]
pub struct BTreeSet[V] {
mut:
	tree &BTree[V, bool]
}

// new_btree_set creates an empty BTreeSet.
pub fn new_btree_set[V]() &BTreeSet[V] {
	return &BTreeSet[V]{
		tree: new_btree[V, bool]()
	}
}

// add inserts value, returning false if it was already present.
pub fn (mut s BTreeSet[V]) add(value V) bool {
	if s.tree.exists(value) {
		return false
	}
	s.tree.set(value, true)
	return true
}

// contains reports whether value is present.
pub fn (s &BTreeSet[V]) contains(value V) bool {
	return s.tree.exists(value)
}

// remove deletes value, returning false if it was absent.
pub fn (mut s BTreeSet[V]) remove(value V) bool {
	return s.tree.delete(value)
}

// len returns the number of elements.
pub fn (s &BTreeSet[V]) len() int {
	return s.tree.len()
}

// is_empty reports whether the set holds no elements.
pub fn (s &BTreeSet[V]) is_empty() bool {
	return s.tree.is_empty()
}

// min returns the smallest element, or none if empty.
pub fn (s &BTreeSet[V]) min() ?V {
	return s.tree.min_key()
}

// max returns the largest element, or none if empty.
pub fn (s &BTreeSet[V]) max() ?V {
	return s.tree.max_key()
}

// range returns the elements v with lo <= v <= hi, in ascending order.
pub fn (s &BTreeSet[V]) range(lo V, hi V) []V {
	return s.tree.range_keys(lo, hi)
}

// --- internal B-tree machinery (CLRS chapter 18) ---

fn (n &BTreeNode[K, V]) find_pos(key K) int {
	mut i := 0
	for i < n.keys.len && n.keys[i] < key {
		i++
	}
	return i
}

fn (n &BTreeNode[K, V]) search(key K) ?V {
	i := n.find_pos(key)
	if i < n.keys.len && n.keys[i] == key {
		return n.values[i]
	}
	if n.leaf {
		return none
	}
	return n.children[i].search(key)
}

fn (mut p BTreeNode[K, V]) split_child(i int) {
	t := btree_min_degree
	mut y := p.children[i]
	mut z := &BTreeNode[K, V]{
		leaf: y.leaf
	}
	mid_key := y.keys[t - 1]
	mid_val := y.values[t - 1]
	for j in t .. 2 * t - 1 {
		z.keys << y.keys[j]
		z.values << y.values[j]
	}
	if !y.leaf {
		for j in t .. 2 * t {
			z.children << y.children[j]
		}
		y.children.trim(t)
	}
	y.keys.trim(t - 1)
	y.values.trim(t - 1)
	p.children.insert(i + 1, z)
	p.keys.insert(i, mid_key)
	p.values.insert(i, mid_val)
}

fn (mut n BTreeNode[K, V]) insert_nonfull(key K, value V) {
	i := n.find_pos(key)
	if i < n.keys.len && n.keys[i] == key {
		n.values[i] = value
		return
	}
	if n.leaf {
		n.keys.insert(i, key)
		n.values.insert(i, value)
		return
	}
	mut j := i
	if n.children[j].keys.len == 2 * btree_min_degree - 1 {
		n.split_child(j)
		if key == n.keys[j] {
			n.values[j] = value
			return
		}
		if key > n.keys[j] {
			j++
		}
	}
	n.children[j].insert_nonfull(key, value)
}

fn (mut n BTreeNode[K, V]) remove(key K) bool {
	t := btree_min_degree
	i := n.find_pos(key)
	if i < n.keys.len && n.keys[i] == key {
		if n.leaf {
			n.keys.delete(i)
			n.values.delete(i)
			return true
		}
		return n.remove_from_non_leaf(i)
	}
	if n.leaf {
		return false
	}
	last := i == n.keys.len
	if n.children[i].keys.len < t {
		n.fill(i)
	}
	if last && i > n.keys.len {
		return n.children[i - 1].remove(key)
	}
	return n.children[i].remove(key)
}

fn (mut n BTreeNode[K, V]) remove_from_non_leaf(i int) bool {
	t := btree_min_degree
	key := n.keys[i]
	if n.children[i].keys.len >= t {
		pred_key, pred_val := n.children[i].min_pair_last()
		n.keys[i] = pred_key
		n.values[i] = pred_val
		return n.children[i].remove(pred_key)
	}
	if n.children[i + 1].keys.len >= t {
		succ_key, succ_val := n.children[i + 1].min_pair_first()
		n.keys[i] = succ_key
		n.values[i] = succ_val
		return n.children[i + 1].remove(succ_key)
	}
	n.merge(i)
	return n.children[i].remove(key)
}

fn (n &BTreeNode[K, V]) min_pair_first() (K, V) {
	mut c := n
	for !c.leaf {
		c = c.children[0]
	}
	return c.keys[0], c.values[0]
}

fn (n &BTreeNode[K, V]) min_pair_last() (K, V) {
	mut c := n
	for !c.leaf {
		c = c.children[c.children.len - 1]
	}
	return c.keys[c.keys.len - 1], c.values[c.keys.len - 1]
}

fn (mut n BTreeNode[K, V]) fill(i int) {
	t := btree_min_degree
	if i > 0 && n.children[i - 1].keys.len >= t {
		n.borrow_from_prev(i)
	} else if i < n.keys.len && n.children[i + 1].keys.len >= t {
		n.borrow_from_next(i)
	} else if i < n.keys.len {
		n.merge(i)
	} else {
		n.merge(i - 1)
	}
}

fn (mut n BTreeNode[K, V]) borrow_from_prev(i int) {
	mut child := n.children[i]
	mut sibling := n.children[i - 1]
	child.keys.insert(0, n.keys[i - 1])
	child.values.insert(0, n.values[i - 1])
	if !child.leaf {
		child.children.insert(0, sibling.children[sibling.children.len - 1])
		sibling.children.delete_last()
	}
	n.keys[i - 1] = sibling.keys[sibling.keys.len - 1]
	n.values[i - 1] = sibling.values[sibling.values.len - 1]
	sibling.keys.delete_last()
	sibling.values.delete_last()
}

fn (mut n BTreeNode[K, V]) borrow_from_next(i int) {
	mut child := n.children[i]
	mut sibling := n.children[i + 1]
	child.keys << n.keys[i]
	child.values << n.values[i]
	if !child.leaf {
		child.children << sibling.children[0]
		sibling.children.delete(0)
	}
	n.keys[i] = sibling.keys[0]
	n.values[i] = sibling.values[0]
	sibling.keys.delete(0)
	sibling.values.delete(0)
}

fn (mut n BTreeNode[K, V]) merge(i int) {
	mut child := n.children[i]
	sibling := n.children[i + 1]
	child.keys << n.keys[i]
	child.values << n.values[i]
	child.keys << sibling.keys
	child.values << sibling.values
	if !child.leaf {
		child.children << sibling.children
	}
	n.keys.delete(i)
	n.values.delete(i)
	n.children.delete(i + 1)
}

fn (n &BTreeNode[K, V]) collect_all(mut out []BTreeEntry[K, V]) {
	for i, k in n.keys {
		if !n.leaf {
			n.children[i].collect_all(mut out)
		}
		out << BTreeEntry[K, V]{k, n.values[i]}
	}
	if !n.leaf {
		n.children[n.keys.len].collect_all(mut out)
	}
}

fn (n &BTreeNode[K, V]) collect_range(lo K, hi K, mut out []BTreeEntry[K, V]) {
	if n.keys.len == 0 {
		// Only the root can be empty; its single child is unbounded.
		if !n.leaf {
			n.children[0].collect_range(lo, hi, mut out)
		}
		return
	}
	// Each subtree is visited exactly once, as the left child of the key
	// following it; the trailing subtree is handled after the loop.
	for i, k in n.keys {
		if !n.leaf && k > lo && (i == 0 || n.keys[i - 1] < hi) {
			n.children[i].collect_range(lo, hi, mut out)
		}
		if k >= lo && k <= hi {
			out << BTreeEntry[K, V]{k, n.values[i]}
		}
	}
	if !n.leaf && n.keys[n.keys.len - 1] < hi {
		n.children[n.keys.len].collect_range(lo, hi, mut out)
	}
}
