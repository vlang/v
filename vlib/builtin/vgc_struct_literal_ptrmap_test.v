// vtest vflags: -gc vgc
// vtest build: !race?
// A heap struct literal with direct pointer/string fields gets a compiler
// generated ptrmap with -gc vgc, so those fields must survive collection.

struct VGCChild {
	v int
}

struct VGCHolder {
	name  string
	child &VGCChild
	value int
}

fn test_vgc_struct_literal_ptrmap_keeps_fields() {
	holder := &VGCHolder{
		name:  'kept'.repeat(8)
		child: &VGCChild{ v: 7 }
		value: 3
	}
	gc_collect()
	assert holder.name.len == 32
	assert holder.child.v == 7
	assert holder.value == 3
}
