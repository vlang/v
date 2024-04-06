@[translated]
module translated

#include "@VMODROOT/sym.c"

@[c: 'ExternalSymbol']
pub fn external_symbol(&char) int

struct C.my_struct {
	active bool
}

@[c_extern]
__global my_instance C.my_struct

pub fn is_my_instance_active() bool {
	return my_instance.active
}
