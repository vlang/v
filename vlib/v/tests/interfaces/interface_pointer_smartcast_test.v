interface Actor {
	marker()
}

struct Entity {
	name string
}

fn (_ &Entity) marker() {}

struct Mob {
	id   u64
	name string
}

fn (_ &Mob) marker() {}

fn (mob &Mob) info() string {
	return '${mob.id}: ${mob.name}'
}

struct Entry {
	actor Actor
}

struct ValueEntity {
	name string
}

fn (_ ValueEntity) marker() {}

fn test_interface_value_smartcast_field_access() {
	actor := Actor(ValueEntity{
		name: 'value entity'
	})
	if actor is ValueEntity {
		assert actor.name == 'value entity'
	} else {
		assert false
	}
}

fn test_interface_pointer_smartcast_field_access() {
	entry := Entry{
		actor: &Entity{
			name: 'entity'
		}
	}
	if entry.actor is &Entity {
		assert entry.actor.name == 'entity'
	} else {
		assert false
	}
}

fn test_interface_pointer_smartcast_method_call() {
	mut actors := map[u64]Actor{}
	actors[1] = &Mob{
		id:   1
		name: 'cow'
	}
	entry := actors[1] or { panic('missing actor') }
	actor := entry
	if actor is &Mob {
		assert actor.info() == '1: cow'
	} else {
		assert false
	}
}

fn test_mut_interface_pointer_smartcast_method_call() {
	mut actor := Actor(&Mob{
		id:   2
		name: 'bull'
	})
	if mut actor is &Mob {
		assert actor.info() == '2: bull'
	} else {
		assert false
	}
}
