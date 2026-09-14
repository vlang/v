module shadowing_global_mod

pub struct Device {
pub mut:
	id int
}

// A global whose bare name another module also uses for a local.
@[has_globals]
__global (
	devices []&Device
)

pub fn register(d &Device) {
	devices << d
}

pub fn registered() int {
	return devices.len
}
