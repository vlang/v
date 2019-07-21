module main

struct ModDepGraphNode  {
	name string
	deps []string
}

struct ModDepGraph {
pub:
	mut:
	acyclic bool
	nodes   []ModDepGraphNode
}

struct DepSet {
	mut: 
	items []string
}

pub fn(mapset mut DepSet) add(item string) {
	mapset.items << item
}

pub fn(mapset &DepSet) diff(otherset DepSet) DepSet {
	mut diff := DepSet{}
	for item in mapset.items {
		if !item in otherset.items {
			diff.items << item
		}
	}
	return diff
}

pub fn(mapset &DepSet) size() int {
	return mapset.items.len
}

pub fn new_dep_graph() *ModDepGraph {
	return &ModDepGraph{
		acyclic: true
	}
}

pub fn(graph mut ModDepGraph) from_import_tables(import_tables []FileImportTable) {
	for fit in file_imports {
		mut deps := []string
		for _, m in fit.imports {
			deps << m
		}
		graph.add(fit.module_name, deps)
	}
}

pub fn(graph mut ModDepGraph) add(mod string, deps []string) {
	graph.nodes << ModDepGraphNode{
		name: mod,
		deps: deps
	}
}

pub fn(graph &ModDepGraph) resolve() *ModDepGraph {
	mut node_names := map[string]ModDepGraphNode{}
	mut node_deps := map[string]DepSet{}

	for _, node in graph.nodes {
		node_names[node.name] = node

		mut dep_set := DepSet{}
		for _, dep in node.deps {
			dep_set.add(dep)
		}
		node_deps[node.name] = dep_set
	}

	mut resolved := new_dep_graph()
	for node_deps.size != 0 {
		mut ready_set := DepSet{}
		for name, deps in node_deps {
			if deps.size() == 0 {
				ready_set.add(name)
			}
		}

		if ready_set.size() == 0 {
			mut g := new_dep_graph()
			g.acyclic = false
			for name, _ in node_deps {				
				g.nodes << node_names[name]
			}
			return g
		}

		ready_set.items.len > 0 {
			mut new_set := map[string]DepSet{}
			for name in ready_set.items {
				// node_deps.remove(name)
				resolved.nodes << node_names[name]
			}
			// remove once we have map.remove/delete 
			for k, d in node_deps {
				if !k in ready_set.items {
					new_set[k] = d
				}
			}
			node_deps = new_set
		}

		for name, deps in node_deps {
			node_deps[name] = deps.diff(ready_set)
		}
	}

	return resolved
}

pub fn(graph &ModDepGraph) imports() []string {
	mut mods := []string
	for node in graph.nodes {
		if node.name == 'main' {
			continue
		}
		mods << node.name
	}
	return mods
}

pub fn(graph &ModDepGraph) last_node() {
	return graph.nodes[graph.nodes.len-1]
}

pub fn(graph &ModDepGraph) display() {
	for _, node in graph.nodes {
		for _, dep in node.deps {
			println(' * $node.name -> $dep')
		}
	}
}
