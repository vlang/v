module iface_mod

// Any has a namesake sum type in `sum_mod`. Resolving this name by its short
// form alone used to box `Holder.get` results into the sum type of that other
// module, which does not even compile.
pub interface Any {}

pub struct Holder {
pub:
	label string
}

pub fn (h &Holder) get() ?Any {
	return h.label
}
