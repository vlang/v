// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

struct ModDepGraphNode  {
mut:
	name string
	deps []string
	last_cycle string
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

pub fn(dset mut DepSet) add(item string) {
	dset.items << item
}

pub fn(dset &DepSet) diff(otherset DepSet) DepSet {
	mut diff := DepSet{}
	for item in dset.items {
		if !item in otherset.items {
			diff.items << item
		}
	}
	return diff
}

pub fn(dset &DepSet) size() int {
	return dset.items.len
}

pub fn new_mod_dep_graph() *ModDepGraph {
	return &ModDepGraph{
		acyclic: true
	}
}

pub fn(graph mut ModDepGraph) from_import_tables(import_tables []FileImportTable) {
	for fit in import_tables {
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

	mut resolved := new_mod_dep_graph()
	for node_deps.size != 0 {
		mut ready_set := DepSet{}
		for name, deps in node_deps {
			if deps.size() == 0 {
				ready_set.add(name)
			}
		}

		if ready_set.size() == 0 {
			mut g := new_mod_dep_graph()
			g.acyclic = false
			ndk := node_deps.keys()
			for name, _ in node_deps {				
				mut node := node_names[name]
				if name == ndk[node_deps.size-1] {
					node.last_cycle = node_deps[name].items[node_deps[name].items.len-1]
				}
				g.nodes << node
			}
			return g
		}

		ready_set.size() > 0 {
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

pub fn(graph &ModDepGraph) last_cycle() string {
	return graph.nodes[graph.nodes.len-1].last_cycle
}

pub fn(graph &ModDepGraph) display() {
	for i:=0; i<graph.nodes.len; i++ {
		node := graph.nodes[i]
		for dep in node.deps {
			mut out := ' * $node.name -> $dep'
			if !graph.acyclic && i == graph.nodes.len-1 && dep == node.last_cycle {
				out += ' <-- last cycle'
			}
			println(out)
		}
	}
}
