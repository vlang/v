interface Resource {}

fn main() {
	mut resource := &Resource(unsafe { nil })
	_ = resource
}
