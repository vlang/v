// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Directed acyclic graph
// this implementation is specifically suited to ordering dependencies
module depgraph

import v.dotgraph

struct DepGraphNode {
pub mut:
	name  string
	value i64
	deps  []string
}

pub struct DepGraph {
pub mut:
	acyclic bool
	nodes   []DepGraphNode
	values  map[string]i64
}

struct OrderedDepMap {
mut:
	keys []string
	data map[string][]string
}

pub fn new_ordered_dependency_map() OrderedDepMap {
	mut res := OrderedDepMap{}
	unsafe { res.keys.flags.set(.noslices) }
	return res
}

pub fn (mut o OrderedDepMap) set(name string, deps []string) {
	if name !in o.data {
		o.keys << name
	}
	o.data[name] = deps
}

pub fn (mut o OrderedDepMap) add(name string, deps []string) {
	mut d := o.get(name)
	for dep in deps {
		if dep !in d {
			d << dep
		}
	}
	o.set(name, d)
}

pub fn (o &OrderedDepMap) get(name string) []string {
	res := o.data[name] or { []string{} }
	return res
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
	deps_of_name := o.get(name)
	for dep in deps_of_name {
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
		nodes: []DepGraphNode{cap: 1024}
	}
}

pub fn (mut graph DepGraph) add(mod string, deps []string) {
	new_node := DepGraphNode{
		name: mod
		deps: deps.clone()
	}
	graph.nodes << new_node
	graph.values[mod] = 0
}

pub fn (mut graph DepGraph) add_with_value(mod string, deps []string, value i64) {
	new_node := DepGraphNode{
		name: mod
		value: value
		deps: deps.clone()
	}
	graph.nodes << new_node
	graph.values[mod] = value
}

pub fn (graph &DepGraph) resolve() &DepGraph {
	mut node_names := new_ordered_dependency_map()
	mut node_deps := new_ordered_dependency_map()
	for node in graph.nodes {
		node_names.add(node.name, node.deps)
		node_deps.add(node.name, node.deps)
	}
	mut iterations := 0
	mut resolved := new_dep_graph()
	for node_deps.size() != 0 {
		iterations++
		mut ready_set := []string{}
		for name in node_deps.keys {
			deps := node_deps.get(name)
			if deps.len == 0 {
				ready_set << name
			}
		}
		if ready_set.len == 0 {
			mut g := new_dep_graph()
			g.acyclic = false
			for name in node_deps.keys {
				g.add_with_value(name, node_names.get(name), graph.values[name])
			}
			return g
		}
		for name in ready_set {
			node_deps.delete(name)
			resolved_deps := node_names.get(name)
			resolved.add_with_value(name, resolved_deps, graph.values[name])
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
	mut out := []string{}
	for node in graph.nodes {
		for dep in node.deps {
			out << ' * $node.name -> $dep'
		}
	}
	return out.join('\n')
}

struct NodeNames {
mut:
	is_cycle map[string]bool
	names    map[string][]string
}

pub fn (graph &DepGraph) display_cycles() string {
	mut seen := false
	mut out := []string{}
	mut nn := NodeNames{}
	for node in graph.nodes {
		nn.names[node.name] = node.deps
	}
	for k, _ in nn.names {
		mut cycle_names := []string{}
		if k in nn.is_cycle {
			continue
		}
		seen, cycle_names = nn.is_part_of_cycle(k, cycle_names)
		if seen {
			out << ' * ' + cycle_names.join(' -> ')
			nn.is_cycle = map[string]bool{}
		}
	}
	return out.join('\n')
}

fn (mut nn NodeNames) is_part_of_cycle(name string, already_seen []string) (bool, []string) {
	mut seen := false
	mut new_already_seen := already_seen.clone()
	if name in nn.is_cycle {
		return nn.is_cycle[name], new_already_seen
	}
	if name in already_seen {
		new_already_seen << name
		nn.is_cycle[name] = true
		return true, new_already_seen
	}
	new_already_seen << name
	deps := nn.names[name]
	if deps.len == 0 {
		nn.is_cycle[name] = false
		return false, new_already_seen
	}
	for d in deps {
		mut d_already_seen := new_already_seen.clone()
		seen, d_already_seen = nn.is_part_of_cycle(d, d_already_seen)
		if seen {
			new_already_seen = d_already_seen.clone()
			nn.is_cycle[name] = true
			return true, new_already_seen
		}
	}
	nn.is_cycle[name] = false
	return false, new_already_seen
}

pub fn show(graph &DepGraph, path string) {
	mut dg := dotgraph.new('ModGraph', 'ModGraph for $path', 'blue')
	mbuiltin := 'builtin'
	for node in graph.nodes {
		is_main := node.name == 'main'
		dg.new_node(node.name, should_highlight: is_main)
		mut deps := node.deps.clone()
		if node.name != mbuiltin && mbuiltin !in deps {
			deps << mbuiltin
		}
		for dep in deps {
			dg.new_edge(node.name, dep, should_highlight: is_main)
		}
	}
	dg.finish()
}
