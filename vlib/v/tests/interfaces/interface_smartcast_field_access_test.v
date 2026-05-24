// Tests for interface smart-cast cgen bugs in field access and variable
// declaration paths.
//
// 1. Interface→interface smart-cast: `parent.id` where parent is smart-cast
//    from one interface to another. Cgen used to emit
//    `(I_X_as_I_Y(parent)->id)` (`->` on a struct value) instead of
//    `(I_X_as_I_Y(parent).id)`.
//
// 2. Variable declared from a smart-cast selector should take the smart-cast
//    type, not the original interface. `mut dd := w.face` inside
//    `if mut w.face is Concrete` was being typed as the original interface,
//    so embedded-field access (e.g. `dd.context.x`) crashed in cgen.
//
// 3. Auto-deref of a `mut` method receiver passed as `voidptr` to a generic
//    method was emitting `*f` instead of `f` because the voidptr parameter
//    type carried the `.generic` flag during cgen, defeating the
//    `voidptr_type !in [...]` early-return.
import eventbus

// ── (1) interface→interface smart-cast field access ────────────────────────

interface Foo {
	id string
}

interface Bar {
	id string
}

struct ImplFB {
	id string
}

fn check_iface_to_iface(parent Foo) string {
	if parent is Bar {
		return parent.id
	}
	return ''
}

fn test_interface_to_interface_smartcast_field_access() {
	i := &ImplFB{
		id: 'iface_to_iface'
	}
	assert check_iface_to_iface(i) == 'iface_to_iface'
}

// ── (2) selector smart-cast var declaration ────────────────────────────────

struct Inner {
	value string
}

struct ConcreteWithEmbed {
	Inner
	tag string
}

interface IFace {}

struct WrapIF {
mut:
	face IFace
}

fn check_selector_smartcast_var(mut w WrapIF) string {
	if mut w.face is ConcreteWithEmbed {
		// `dd` should be ConcreteWithEmbed, not IFace, so accessing the
		// embedded `Inner.value` field works.
		dd := w.face
		return '${dd.tag}:${dd.value}'
	}
	return ''
}

fn test_selector_smartcast_var_declaration() {
	mut c := ConcreteWithEmbed{
		Inner: Inner{
			value: 'inner_v'
		}
		tag:   'tag_v'
	}
	mut w := WrapIF{
		face: &c
	}
	assert check_selector_smartcast_var(mut w) == 'tag_v:inner_v'
}

// ── (3) auto-deref of mut receiver passed as voidptr to a generic method ───

struct Counter {
mut:
	got_self int
	hits     int
}

fn counter_handler(receiver voidptr, args voidptr, sender voidptr) {
	mut c := unsafe { &Counter(receiver) }
	c.hits++
}

fn (mut c Counter) wire(mut bus eventbus.EventBus[string]) {
	// Bare `c` here. The receiver is a `voidptr` parameter on a generic
	// method, and the bug emitted `*c` (struct value) for the C call.
	bus.subscriber.subscribe_method('hit', counter_handler, c)
	c.got_self = unsafe { int(i64(voidptr(c)) & 0xffffffff) }
}

fn test_voidptr_mut_receiver_in_generic_method() {
	mut c := &Counter{}
	mut bus := eventbus.new[string]()
	c.wire(mut bus)
	bus.publish('hit', unsafe { nil }, unsafe { nil })
	bus.publish('hit', unsafe { nil }, unsafe { nil })
	assert c.hits == 2
}
