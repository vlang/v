struct RecursiveStorageBranch {
	next     &RecursiveStorageNode
	optional ?&RecursiveStorageNode
	children []RecursiveStorageNode
	values   map[string]RecursiveStorageNode
}

type RecursiveStorageNode = RecursiveStorageBranch | int

struct RecursiveStorageHolder {
	value ?RecursiveStorageNode
}

fn test_sumtype_recursion_can_use_references_and_dynamic_collections() {
	leaf := RecursiveStorageNode(7)
	node := RecursiveStorageNode(RecursiveStorageBranch{
		next:     &leaf
		optional: &leaf
		children: [leaf]
		values:   {
			'leaf': leaf
		}
	})
	branch := node as RecursiveStorageBranch
	assert *branch.next == leaf
	optional := branch.optional or { panic('missing reference') }
	assert *optional == leaf
	assert branch.children[0] == leaf
	assert (branch.values['leaf'] or { panic('missing child') }) == leaf
	holder := RecursiveStorageHolder{ value: node }
	assert (holder.value or { panic('missing node') }) is RecursiveStorageBranch
}
