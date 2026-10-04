module main

import os
import v.help
import v.vmod

// DepGraph is the dependency graph of what is installed right now.
//
// An installed node is identified by the directory it resolves to. A dependency
// written as a registered name and the same dependency written as a git URL are
// therefore one node, while two packages whose v.mod files happen to declare the
// same `name` stay two nodes. A node is shown by its import path, which is what a
// user writes in `v.mod` and in `import`. A dependency that is not installed has
// no directory, so it is identified, and shown, by the string that named it.
struct DepGraph {
mut:
	deps      map[string][]string // id -> the ids of the nodes it requires
	labels    map[string]string   // id -> the name the node is shown as
	absent    map[string]bool     // id -> required by something, but not installed
	ids       map[string]string   // dependency string, as written -> id
	root_name string
	root_deps []string // the ids of the current project's dependencies
}

// vpm_why explains why a module is in the dependency graph, or prints the whole
// graph when no module is named.
//
// This reads only. It never resolves a remote, never contacts the registry, and
// never installs, updates or removes a module, so it works in a build with no
// network.
fn vpm_why(query []string) {
	if settings.is_help {
		help.print_and_exit('why')
	}
	if query.len > 1 {
		vpm_error('`v why` expects at most one module name.')
		exit(2)
	}
	graph := build_dep_graph() or {
		vpm_error(err.msg())
		exit(1)
	}
	if query.len == 0 {
		print_graph(graph, '')
		return
	}
	wanted := graph.node_of(query[0]) or {
		vpm_error('`${query[0]}` is not in the dependency graph of `${graph.root_name}`.')
		eprintln('Run `v why` with no argument to see the whole graph.')
		exit(1)
	}
	print_graph(graph, wanted)
}

// build_dep_graph walks out from the current project through the modules that are
// installed, reading each one's v.mod for the next step.
fn build_dep_graph() !DepGraph {
	project := vmod.get_cache().get_by_folder(os.getwd())
	if project.vmod_file == '' {
		return error('no v.mod found at or above `${os.getwd()}`')
	}
	root := vmod.from_file(project.vmod_file)!
	mut graph := DepGraph{
		root_name: root.name
	}
	roots := module_roots()
	mut queue := []string{}
	graph.root_deps = graph.node_ids(root.dependencies, roots, mut queue)
	for i := 0; i < queue.len; i++ {
		id := queue[i]
		// A checkout without a v.mod is still a module (vpm accepts registered
		// ones), so it stays a node. It just declares no dependencies of its own.
		manifest := vmod.from_file(os.join_path(id, 'v.mod')) or { vmod.Manifest{} }
		graph.deps[id] = graph.node_ids(manifest.dependencies, roots, mut queue)
	}
	return graph
}

// node_ids maps one module's dependency strings onto node ids, adding a node the
// first time a module is met. An installed module met for the first time is
// queued, so that its own v.mod is read in turn.
fn (mut g DepGraph) node_ids(raws []string, roots []string, mut queue []string) []string {
	mut ids := []string{}
	for raw in raws {
		mut id := g.ids[raw] or { '' }
		if id == '' {
			root, path := resolve_existing_module(roots, raw) or { '', '' }
			if path == '' {
				// An unresolvable dependency is recorded rather than dropped: knowing
				// that something is required but missing is the answer, not a failure.
				id = raw
				g.absent[id] = true
				g.labels[id] = raw
			} else {
				id = path
				if id !in g.labels {
					g.labels[id] = module_label(root, path, raw)
					queue << id
				}
			}
			g.ids[raw] = id
		}
		// The same module written twice, e.g. once by name and once by URL, is one
		// dependency rather than two.
		if id !in ids {
			ids << id
		}
	}
	return ids
}

// node_of finds the node that a module name refers to. The name may be written in
// any form that resolves to the same module, e.g. as the git URL of a registered
// package.
fn (g &DepGraph) node_of(name string) ?string {
	if id := g.ids[name] {
		return id
	}
	_, path := resolve_existing_module(module_roots(), name) or { return none }
	if path in g.deps {
		return path
	}
	return none
}

// nodes_leading_to returns every node from which `focus` can be reached, `focus`
// itself included. The focused view draws only those.
fn (g &DepGraph) nodes_leading_to(focus string) map[string]bool {
	mut parents := map[string][]string{}
	for node, children in g.deps {
		for child in children {
			parents[child] << node
		}
	}
	mut leads_to := {
		focus: true
	}
	mut queue := [focus]
	for i := 0; i < queue.len; i++ {
		for parent in parents[queue[i]] {
			if parent !in leads_to {
				leads_to[parent] = true
				queue << parent
			}
		}
	}
	return leads_to
}

// print_graph draws the graph as a tree. With an empty `focus` it draws everything,
// rooted at the current project. With a node it draws only the routes that reach
// it, which is the question `v why <module>` exists to answer.
fn print_graph(graph &DepGraph, focus string) {
	println(graph.root_name)
	leads_to := if focus == '' { map[string]bool{} } else { graph.nodes_leading_to(focus) }
	on_path := map[string]bool{}
	print_level(graph, graph.root_deps, '  ', focus, leads_to, &on_path)
}

// print_level draws one level of the tree. With a `focus`, it leaves out the nodes
// that do not lead to it, and does not descend below the focus itself.
//
// `on_path` is the cycle guard, and it is not optional: a dependency graph is
// allowed to contain a loop, and without it a single `v why` recurses until the
// stack gives out.
fn print_level(graph &DepGraph, level []string, prefix string, focus string, leads_to map[string]bool, on_path &map[string]bool) {
	shown := if focus == '' { level } else { level.filter(it in leads_to) }
	for i, node in shown {
		last := i == shown.len - 1
		branch := if last { '`-- ' } else { '|-- ' }
		label := graph.labels[node] or { node }
		missing := if graph.absent[node] { ' (not installed)' } else { '' }
		repeat := node in *on_path
		println('${prefix}${branch}${label}${missing}${if repeat { ' (cycle)' } else { '' }}')
		if repeat || node == focus {
			continue
		}
		mut next_on_path := (*on_path).clone()
		next_on_path[node] = true
		child_prefix := prefix + if last { '    ' } else { '|   ' }
		print_level(graph, graph.deps[node] or { []string{} }, child_prefix, focus, leads_to,
			&next_on_path)
	}
}

// module_roots are the folders a dependency is looked up in, in order: the current
// project's own folder, which is where `v install --local` puts packages and where
// the compiler looks first, and then the modules directory.
fn module_roots() []string {
	local := local_vmodules_path(os.getwd())
	global := os.vmodules_dir()
	if os.real_path(local) == os.real_path(global) {
		return [local]
	}
	return [local, global]
}

// resolve_existing_module finds the installed directory for a dependency string,
// and the root it was found under.
//
// get_path_of_existing_module does a similar lookup but reports a missing module as
// an error, which is wrong here: a graph walk asks about every dependency of every
// module it visits, and a missing one is a fact to record rather than a failure.
fn resolve_existing_module(roots []string, mod_name string) ?(string, string) {
	for root in roots {
		_, path := candidate_module_path(root, mod_name)
		if os.is_dir(path) {
			return root, path
		}
	}
	return none
}

// module_label is the import path of an installed module, e.g. `nedpals.args` for
// `<root>/nedpals/args`. A module linked in from elsewhere (`v link`) resolves
// outside its root, so it is shown the way it was asked for.
fn module_label(root string, path string, raw string) string {
	if path_is_below(path, os.real_path(root)) {
		return import_path_relative_to(path, root)
	}
	return raw
}
