pub type FnGridObject = fn (mut object GridObject)

@[flag]
pub enum GridObjectAuto {
	drag
	drop
	scale_on_hover
	pickup
	sound
	off // auto "deactivation"
}

@[params]
pub struct GridObjectConfig {
	auto GridObjectAuto = ~GridObjectAuto.zero()
	on   ?FnGridObject
}

@[heap]
struct GridObject {
mut:
	config GridObjectConfig
	auto   GridObjectAuto = ~GridObjectAuto.zero()
}

pub fn on(config GridObjectConfig) &GridObject {
	mut ob := &GridObject{}
	ob.config = config
	ob.auto = config.auto
	if func := ob.config.on {
		func(mut ob)
	}
	return ob
}

fn test_main() {
	_ := on(
		on: fn (mut o GridObject) {
			o.auto = .off | .pickup | .sound
		}
	)
	assert true
}
