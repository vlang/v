module main

fn get_key_refcount() u32 {
	key := &C._gpgme_key(unsafe { nil })
	return key._refs
}

fn main() {
	assert get_key_refcount() == 0
}
