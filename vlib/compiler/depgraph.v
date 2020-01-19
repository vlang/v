// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Directed acyclic graph
// this implementation is specifically suited to ordering dependencies
module compiler

struct DepGraphNode {
mut:
	name string
	deps []string
}

struct DepGraph {
	// pub:
mut:
	acyclic bool
	nodes   []DepGraphNode
}

struct DepSet {
mut:
	items []string
}

pub fn (dset mut DepSet) add(item string) {
	dset.items << item
}

pub fn (dset &DepSet) diff(otherset DepSet) DepSet {
	mut diff := DepSet{
	}
	for item in dset.items {
		if !item in otherset.items {
			diff.items << item
		}
	}
	return diff
}

pub fn (dset &DepSet) size() int {
	return dset.items.len
}

pub fn new_dep_graph() &DepGraph {
	return &DepGraph{
		acyclic: true
	}
}

pub fn (graph mut DepGraph) add(mod string, deps []string) {
	graph.nodes << DepGraphNode{
		name: mod
		deps: deps.clone()
	}
}

pub fn (graph &DepGraph) resolve() &DepGraph {
	mut node_names := map[string]DepGraphNode
	mut node_deps := map[string]DepSet
	for _, node in graph.nodes {
		node_names[node.name] = node
		mut dep_set := DepSet{
		}
		for _, dep in node.deps {
			dep_set.add(dep)
		}
		node_deps[node.name] = dep_set
	}
	mut resolved := new_dep_graph()
	for node_deps.size != 0 {
		mut ready_set := DepSet{
		}
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

pub fn (graph &DepGraph) last_node() DepGraphNode {
	return graph.nodes[graph.nodes.len - 1]
}

pub fn (graph &DepGraph) display() string {
	mut out := '\n'
	for node in graph.nodes {
		for dep in node.deps {
			out += ' * $node.name -> $dep\n'
		}
	}
	return out
}

pub fn (graph &DepGraph) display_cycles() string {
	mut node_names := map[string]DepGraphNode
	for node in graph.nodes {
		node_names[node.name] = node
	}
	mut out := '\n'
	for node in graph.nodes {
		for dep in node.deps {
			if !(dep in node_names) {
				continue
			}
			dn := node_names[dep]
			if node.name in dn.deps {
				out += ' * $node.name -> $dep\n'
			}
		}
	}
	return out
}

