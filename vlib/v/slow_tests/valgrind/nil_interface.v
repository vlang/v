interface Resource {}

@[unsafe]
fn main() {
	mut resource := &Resource(unsafe { nil })
	_ = resource
}
