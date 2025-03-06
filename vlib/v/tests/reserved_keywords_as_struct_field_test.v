// reserved_keywords_as_struct_field_test.v
// Copyright (c) 2021 Pasha Radchenko <ep4sh2k@gmail.com>. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

struct Empty {}

// LLNode is struct which holds data and links
struct LLNode {
	data int
	link LinkedList
}

// LinkedList represent a linked list
type LinkedList = Empty | LLNode

// insert performs inserting of the value into the LinkedList
fn insert(ll LinkedList, val int) LinkedList {
	match ll {
		Empty {
			return LLNode{val, Empty{}}
		}
		LLNode {
			return LLNode{
				...ll
				link: insert(ll.link, val)
			}
		}
	}
}

// prepend performs inserting of the value on the top of the LinkedList
fn prepend(ll LinkedList, val int) LinkedList {
	match ll {
		Empty {
			return LLNode{val, Empty{}}
		}
		LLNode {
			return LLNode{
				data: val
				link: ll
			}
		}
	}
}

fn test_reserved_keywords_as_struct_field() {
	mut ll := LinkedList(Empty{})
	ll = insert(ll, 997)
	ll = insert(ll, 998)
	ll = insert(ll, 999)
	mut desired_ll := LinkedList(LLNode{
		data: 997
		link: LinkedList(LLNode{
			data: 998
			link: LinkedList(LLNode{
				data: 999
				link: Empty{}
			})
		})
	})
	assert ll == desired_ll
}
