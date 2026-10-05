// reserved_keywords_as_struct_field_test.v
// Copyright (c) 2021 Pasha Radchenko <ep4sh2k@gmail.com>. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

struct Empty {}

// LLNode is struct which holds data and links
struct LLNode {
	data int
	link &LinkedList
}

// LinkedList represent a linked list
type LinkedList = Empty | LLNode

// insert performs inserting of the value into the LinkedList
fn insert(ll LinkedList, val int) LinkedList {
	match ll {
		Empty {
			return LLNode{val, &LinkedList(ll)}
		}
		LLNode {
			return LLNode{
				...ll
				link: &LinkedList(insert(ll.link, val))
			}
		}
	}
}

// prepend performs inserting of the value on the top of the LinkedList
fn prepend(ll LinkedList, val int) LinkedList {
	match ll {
		Empty {
			return LLNode{val, &LinkedList(ll)}
		}
		LLNode {
			return LLNode{
				data: val
				link: &LinkedList(ll)
			}
		}
	}
}

fn test_reserved_keywords_as_struct_field() {
	mut ll := LinkedList(Empty{})
	ll = insert(ll, 997)
	ll = insert(ll, 998)
	ll = insert(ll, 999)
	empty := LinkedList(Empty{})
	last := LinkedList(LLNode{ data: 999, link: &empty })
	middle := LinkedList(LLNode{ data: 998, link: &last })
	desired_ll := LinkedList(LLNode{ data: 997, link: &middle })
	assert ll == desired_ll
}

fn (a LinkedList) == (b LinkedList) bool {
	if a is LLNode && b is LLNode {
		return a.data == b.data && *a.link == *b.link
	}
	return a is Empty && b is Empty
}
