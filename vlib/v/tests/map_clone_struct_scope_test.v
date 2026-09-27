struct MapCloneScopeValue {
	text  string
	items []MapCloneScopeValue
mut:
	fields map[string]MapCloneScopeValue
}

type MapCloneScope = map[string]MapCloneScopeValue

fn check_map_clone_scope(scope map[string]MapCloneScopeValue, items []MapCloneScopeValue) {
	for index, item in items {
		mut item_scope := scope.clone()
		item_scope['item'] = item
		item_scope['index'] = MapCloneScopeValue{
			text: index.str()
		}
		item_scope['root'] = MapCloneScopeValue{
			text: 'replacement'
		}
		assert item_scope.len == 3
		assert item_scope['item'].text == item.text
		assert item_scope['index'].text == index.str()
		assert item_scope['root'].text == 'replacement'
		assert 'item' !in scope
		assert 'index' !in scope
	}
}

fn check_aliased_map_clone_scope(scope MapCloneScope) {
	mut copy := scope.clone()
	copy['root'] = MapCloneScopeValue{
		text: 'replacement'
	}
	copy['new'] = MapCloneScopeValue{
		text: 'inserted'
	}
	assert copy['root'].text == 'replacement'
	assert copy['new'].text == 'inserted'
	assert scope['root'].text == 'original'
	assert 'new' !in scope
}

fn test_cloned_recursive_map_scope_can_insert_and_replace_entries() {
	scope := {
		'root': MapCloneScopeValue{
			text:   'original'
			items:  [MapCloneScopeValue{
				text: 'nested item'
			}]
			fields: {
				'name': MapCloneScopeValue{
					text: 'nested field'
				}
			}
		}
	}
	items := [MapCloneScopeValue{
		text: 'first'
	}, MapCloneScopeValue{
		text: 'second'
	}]
	check_map_clone_scope(scope, items)
	assert scope.len == 1
	assert scope['root'].text == 'original'
	assert scope['root'].items[0].text == 'nested item'
	assert scope['root'].fields['name'].text == 'nested field'
	check_map_clone_scope(map[string]MapCloneScopeValue{}, items)
}

fn test_map_alias_clone_can_replace_struct_entries() {
	scope := MapCloneScope({
		'root': MapCloneScopeValue{
			text: 'original'
		}
	})
	check_aliased_map_clone_scope(scope)
	assert scope.len == 1
	assert scope['root'].text == 'original'
}
