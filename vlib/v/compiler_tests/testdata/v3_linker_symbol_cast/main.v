module main

import device

fn C.linker_symbol()
fn C.use_pointer(charptr)

fn kernel_entry() {}

fn main() {
	kernel_entry()
	name := 'vinix'
	C.use_pointer(name.str)
	address := u64(voidptr(C.linker_symbol))
	_ = address
	virtual_base := u64(1)
	physical_base := u64(2)
	text_phys := (address - virtual_base) + physical_base
	_ = text_phys
	device.initialise()
}
