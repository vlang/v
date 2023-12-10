@[has_globals]
module profile

__global v__profile_enabled = true

pub fn state() bool {
	return v__profile_enabled
}

@[if profile]
pub fn on(state bool) {
	v__profile_enabled = state
}
