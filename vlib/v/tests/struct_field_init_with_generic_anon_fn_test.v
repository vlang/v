pub struct Module {
}

pub struct Service {
	callback fn () = unsafe { nil }
}

pub fn (mut self Module) do_anything[T]() {
}

pub fn (mut self Module) register[T]() {
	_ := Service{
		callback: fn [mut self] [T]() {
			self.do_anything[T]()
		}
	}
}

struct Something {
}

struct SomethingDifferent {
}

fn test_struct_field_init_with_generic_anon_fn() {
	mut mod := Module{}
	mod.register[Something]()
	mod.register[SomethingDifferent]()
	assert true
}
