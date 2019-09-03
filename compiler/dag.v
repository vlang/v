// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Directed acyclic graph
// this implementation is specifically suited to ordering dependencies

module main

struct DepDagNode  {
mut:
	name string
	deps []string
	last_cycle string
}

struct DepDag {
pub:
	mut:
	acyclic bool
	nodes   []DepDagNode
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

pub fn new_dep_dag() &DepDag {
	return &DepDag{
		acyclic: true
	}
}


pub fn(graph mut DepDag) add(mod string, deps []string) {
	graph.nodes << DepDagNode{
		name: mod,
		deps: deps
	}
}

pub fn(graph &DepDag) resolve() &DepDag {
	mut node_names := map[string]DepDagNode
	mut node_deps := map[string]DepSet

	for _, node in graph.nodes {
		node_names[node.name] = node

		mut dep_set := DepSet{}
		for _, dep in node.deps {
			dep_set.add(dep)
		}
		node_deps[node.name] = dep_set
	}

	mut resolved := new_dep_dag()
	for node_deps.size != 0 {
		mut ready_set := DepSet{}
		for name, deps in node_deps {
			if deps.size() == 0 {
				ready_set.add(name)
			}
		}

		if ready_set.size() == 0 {
			mut g := new_dep_dag()
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

		for name in ready_set.items {
			node_deps.delete(name)
			resolved.nodes << node_names[name]
		}

		for name, deps in node_deps {
			node_deps[name] = deps.diff(ready_set)
		}
	}

	return resolved
}

pub fn(graph &DepDag) last_node() DepDagNode {
	return graph.nodes[graph.nodes.len-1]
}

pub fn(graph &DepDag) last_cycle() string {
	return graph.last_node().last_cycle
}

pub fn(graph &DepDag) display() {
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
