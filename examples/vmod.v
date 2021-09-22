module main

import v.vmod

fn main() {
	mod := vmod.decode(@VMOD_FILE) or { panic('Error decoding v.mod') }
	println('$mod.name has version $mod.version')
	println('\nThe full mod struct: \n$mod')
}
