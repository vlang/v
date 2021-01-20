// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Directed acyclic graph
// this implementation is specifically suited to ordering dependencies
module depgraph

struct DepGraphNode {
pub mut:
	name string
	deps []string
}

struct DepGraph {
pub mut:
	acyclic bool
	nodes   []DepGraphNode
}

struct OrderedDepMap {
mut:
	keys []string
	data map[string][]string
}

pub fn (mut o OrderedDepMap) set(name string, deps []string) {
	if name !in o.data {
		o.keys << name
	}
	o.data[name] = deps
}

pub fn (mut o OrderedDepMap) add(name string, deps []string) {
	mut d := o.data[name]
	for dep in deps {
		if dep !in d {
			d << dep
		} else {
		}
	}
	o.set(name, d)
}

pub fn (o &OrderedDepMap) get(name string) []string {
	return o.data[name]
}

pub fn (mut o OrderedDepMap) delete(name string) {
	if name !in o.data {
		panic('delete: no such key: $name')
	}
	for i, _ in o.keys {
		if o.keys[i] == name {
			o.keys.delete(i)
			break
		}
	}
	o.data.delete(name)
}

pub fn (mut o OrderedDepMap) apply_diff(name string, deps []string) {
	mut diff := []string{}
	for dep in o.data[name] {
		if dep !in deps {
			diff << dep
		}
	}
	o.set(name, diff)
}

pub fn (o &OrderedDepMap) size() int {
	return o.data.len
}

pub fn new_dep_graph() &DepGraph {
	return &DepGraph{
		acyclic: true
	}
}

pub fn (mut graph DepGraph) add(mod string, deps []string) {
	graph.nodes << DepGraphNode{
		name: mod
		deps: deps.clone()
	}
}

pub fn (graph &DepGraph) resolve() &DepGraph {
	mut node_names := OrderedDepMap{}
	mut node_deps := OrderedDepMap{}
	for node in graph.nodes {
		node_names.add(node.name, node.deps)
		node_deps.add(node.name, node.deps)
	}
	mut resolved := new_dep_graph()
	for node_deps.size() != 0 {
		mut ready_set := []string{}
		for name in node_deps.keys {
			deps := node_deps.data[name]
			if deps.len == 0 {
				ready_set << name
			}
		}
		if ready_set.len == 0 {
			mut g := new_dep_graph()
			g.acyclic = false
			for name in node_deps.keys {
				g.add(name, node_names.data[name])
			}
			return g
		}
		for name in ready_set {
			node_deps.delete(name)
			resolved.add(name, node_names.data[name])
		}
		for name in node_deps.keys {
			node_deps.apply_diff(name, ready_set)
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
	mut node_names := map[string]DepGraphNode{}
	for node in graph.nodes {
		node_names[node.name] = node
	}
	mut out := '\n'
	for node in graph.nodes {
		for dep in node.deps {
			if dep !in node_names {
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
