@[heap]
struct DumpHeapGenericLink {
	name string
	next &DumpHeapGenericLink = unsafe { nil }
}

type DumpHeapGenericNode = string | map[string]DumpHeapGenericNode

fn dump_heap_generic_decode_struct[T](mut t T, values map[string]DumpHeapGenericNode) {
	$for f in T.fields {
		val := values[f.name] or { return }
		$if f is string {
			if val is string {
				t.$(f.name) = val
			}
		} $else $if f is $pointer {
			if f.is_struct && val is map[string]DumpHeapGenericNode {
				mut elem := dump_heap_generic_deref_element(&t.$(f.name))
				dump_heap_generic_decode_struct(mut elem, val)
				t.$(f.name) = &elem
			}
		}
	}
}

fn dump_heap_generic_deref_element[T](a &T) T {
	return T{}
}

fn test_dump_does_not_corrupt_heap_generic_linked_list() {
	n3 := {
		'name': DumpHeapGenericNode('3')
	}
	n2 := {
		'name': DumpHeapGenericNode('2')
		'next': n3
	}
	n1 := {
		'name': DumpHeapGenericNode('1')
		'next': n2
	}
	mut link := DumpHeapGenericLink{}
	dump_heap_generic_decode_struct[DumpHeapGenericLink](mut link, n1)
	dump(link)
	assert link.name == '1'
	assert link.next.name == '2'
	assert link.next.next.name == '3'
	assert link.next.next.next == unsafe { nil }
}
