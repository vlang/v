import shadowing_global_mod

// `global_modules` maps a bare name to whichever module declares a global of
// that name. Uses of a local binding with the same name resolved to that global
// instead -- a module this file need not even import -- so the local was
// declared and then never read:
//
//   main__Node* devices = ...;      // the local
//   blk__devices->name = ...;       // every use
//
// The C compiler then rejected the field access on the global's own type.
struct Node {
pub mut:
	name  string
	count int
}

fn find() ?&Node {
	return &Node{
		name: 'dev'
	}
}

fn plain_local() string {
	mut devices := &Node{
		name: 'start'
	}
	devices.name = 'plain'
	return devices.name
}

fn local_from_or_block() string {
	mut devices := find() or { return 'unreachable' }
	devices.name = 'or-block'
	devices.count = 3
	return '${devices.name}:${devices.count}'
}

fn test_a_local_shadows_another_modules_global() {
	shadowing_global_mod.register(&shadowing_global_mod.Device{
		id: 1
	})
	assert plain_local() == 'plain'
	assert local_from_or_block() == 'or-block:3'
}

fn test_the_other_modules_global_is_untouched() {
	before := shadowing_global_mod.registered()
	assert plain_local() == 'plain'
	assert shadowing_global_mod.registered() == before
}
