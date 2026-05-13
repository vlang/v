@[heap; minify]
struct Cfg {
	id   string
	rows []int
	meta map[string]string
}

struct State {
	value int
}

struct CrudState {
	value int
}

struct Caps {}

struct Store {
mut:
	data map[string]State
}

fn (store &Store) get(id string) ?State {
	if id in store.data {
		return store.data[id]
	}
	return none
}

@[heap]
struct Window {
mut:
	store Store
}

fn resolve_a(cfg Cfg, mut _ Window) (Cfg, State, bool, Caps) {
	return cfg, State{
		value: 1
	}, true, Caps{}
}

fn resolve_b(cfg Cfg, mut _ Window) (Cfg, CrudState) {
	return cfg, CrudState{
		value: 2
	}
}

fn (mut window Window) trigger(cfg Cfg) (Cfg, State, CrudState) {
	resolved_cfg0, source_state0, has_source, _ := resolve_a(cfg, mut window)
	mut resolved_cfg := resolved_cfg0
	mut source_state := source_state0
	mut crud_state := CrudState{}
	crud_enabled := true
	if crud_enabled {
		resolved_cfg, crud_state = resolve_b(resolved_cfg, mut window)
		if has_source {
			source_state = window.store.get(resolved_cfg.id) or { source_state }
		}
	}
	return resolved_cfg, source_state, crud_state
}

fn test_tuple_reassign_to_heap_struct_local() {
	mut window := &Window{
		store: Store{
			data: {
				'x': State{
					value: 99
				}
			}
		}
	}
	resolved_cfg, source_state, crud_state := window.trigger(Cfg{
		id:   'x'
		rows: [1, 2, 3]
		meta: {
			'a': 'b'
		}
	})
	assert resolved_cfg.id == 'x'
	assert source_state.value == 99
	assert crud_state.value == 2
}
