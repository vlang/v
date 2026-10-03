module main

import os
import v.help
import v.vmod

// DepGraph is the dependency graph of what is installed right now.
//
// Nodes are keyed by display name. A dependency written as a registered name and
// the same dependency written as a git URL resolve to one node, because a node is
// identified by the module it points at rather than by the string that named it.
struct DepGraph {
mut:
	deps       map[string][]string // name -> the names it requires
	version    map[string]string   // name -> the version its own v.mod declares
	absent     map[string]bool     // name -> required by something, but not installed
	root_name  string
	root_deps_ []string // the current project's dependencies, as written
}

// vpm_why explains why a module is in the dependency graph, or prints the whole
// graph when no module is named.
//
// This reads only. It never resolves a remote, never contacts the registry, and
// never writes to the modules directory, so it works in a build with no network.
fn vpm_why(query []string) {
	if settings.is_help {
		help.print_and_exit('why')
	}
	graph := build_dep_graph() or {
		vpm_error(err.msg())
		exit(1)
	}
	if query.len == 0 {
		print_graph(graph, '')
		return
	}
	wanted := graph.canonical_name(query.join('.'))
	if wanted !in graph.deps && wanted !in graph.absent {
		vpm_error('`${query.join('.')}` is not in the dependency graph of `${graph.root_name}`.')
		eprintln('Run `v why` with no argument to see the whole graph.')
		exit(1)
	}
	print_graph(graph, wanted)
}

// build_dep_graph walks out from the current project through the modules that are
// installed, reading each one's v.mod for the next step.
fn build_dep_graph() !DepGraph {
	mut graph := DepGraph{
		deps:       map[string][]string{}
		version:    map[string]string{}
		absent:     map[string]bool{}
		root_deps_: []string{}
	}
	project := vmod.get_cache().get_by_folder(os.getwd())
	if project.vmod_file == '' {
		return error('no v.mod found at or above `${os.getwd()}`')
	}
	root := vmod.from_file(project.vmod_file)!
	graph.root_name = root.name
	graph.root_deps_ = root.dependencies.clone()

	mut queue := root.dependencies.clone()
	mut queued := map[string]bool{}
	for raw in queue {
		queued[raw] = true
	}
	mut seen := map[string]bool{}
	for raw in queue {
		// An unresolvable dependency is recorded rather than dropped: knowing that
		// something is required but missing is the answer, not a failure.
		path := resolve_existing_module(raw) or {
			graph.absent[raw] = true
			continue
		}
		manifest := vmod.from_file(os.join_path(path, 'v.mod')) or { continue }
		name := if manifest.name != '' { manifest.name } else { raw }
		if seen[name] {
			continue
		}
		seen[name] = true
		graph.deps[name] = manifest.dependencies
		graph.version[name] = manifest.version
		for dep in manifest.dependencies {
			if dep !in queued {
				queued[dep] = true
				queue << dep
			}
		}
	}
	return graph
}

// print_graph draws the graph as a tree. With an empty `focus` it draws everything,
// rooted at the current project. With a name it draws only the routes that reach
// it, which is the question `v why <module>` exists to answer.
fn print_graph(graph &DepGraph, focus string) {
	if focus == '' {
		println(graph.root_name)
		on_path := map[string]bool{}
		print_level(graph, graph.root_children(), '  ', &on_path)
		return
	}
	mut chains := [][]string{}
	on_path := map[string]bool{}
	collect_chains(graph, graph.root_children(), focus, []string{}, &on_path, &chains)
	println(graph.root_name)
	if chains.len == 0 {
		println('  `-- ${focus} (not installed)')
		return
	}
	for chain in chains {
		mut prefix := '  '
		for i, node in chain {
			last := i == chain.len - 1
			branch := if last { '`-- ' } else { '|-- ' }
			label := if graph.absent[node] { '${node} (not installed)' } else { node }
			println('${prefix}${branch}${label}')
			prefix += if last { '    ' } else { '|   ' }
		}
	}
}

// print_level draws one level of the whole-graph view.
//
// `on_path` is the same cycle guard collect_chains uses, and it is not optional: a
// dependency graph is allowed to contain a loop, and without it a single `v why`
// with no argument recurses until the stack gives out.
fn print_level(graph &DepGraph, level []string, prefix string, on_path &map[string]bool) {
	for i, node in level {
		last := i == level.len - 1
		branch := if last { '`-- ' } else { '|-- ' }
		label := if graph.absent[node] { '${node} (not installed)' } else { node }
		repeat := node in *on_path
		println('${prefix}${branch}${label}${if repeat { ' (cycle)' } else { '' }}')
		if repeat {
			continue
		}
		mut next_on_path := (*on_path).clone()
		next_on_path[node] = true
		child_prefix := prefix + if last { '    ' } else { '|   ' }
		print_level(graph, graph.child_names(node), child_prefix, &next_on_path)
	}
}

// root_children are the current project's dependencies, named the way the graph
// names its other nodes.
fn (g &DepGraph) root_children() []string {
	return g.canonicalise(g.root_deps_)
}

// child_names maps one node's raw dependency strings onto node names.
fn (g &DepGraph) child_names(node string) []string {
	return g.canonicalise(g.deps[node] or { []string{} })
}

fn (g &DepGraph) canonicalise(raws []string) []string {
	mut out := []string{}
	for raw in raws {
		out << g.canonical_name(raw)
	}
	return out
}

// canonical_name maps a dependency string onto the node name the graph uses, so a
// module referred to by URL and the same module referred to by registered name are
// one node rather than two.
fn (g &DepGraph) canonical_name(raw string) string {
	if raw in g.deps || raw in g.absent {
		return raw
	}
	path := resolve_existing_module(raw) or { return raw }
	manifest := vmod.from_file(os.join_path(path, 'v.mod')) or { return raw }
	if manifest.name != '' {
		return manifest.name
	}
	return raw
}

// collect_chains walks forward from one level looking for `focus`, and records
// every route it finds. `on_path` guards against a dependency cycle: a node
// already on the current route is not entered again, because entering it would
// produce an unbounded number of routes through a loop.
fn collect_chains(g &DepGraph, level []string, focus string, route []string, on_path &map[string]bool, chains &[][]string) {
	for node in level {
		mut extended := route.clone()
		extended << node
		if node == focus {
			*chains << extended
			continue
		}
		if node in *on_path {
			continue
		}
		mut next_on_path := (*on_path).clone()
		next_on_path[node] = true
		collect_chains(g, g.child_names(node), focus, extended, &next_on_path, chains)
	}
}

// resolve_existing_module finds the installed directory for a dependency string.
//
// get_path_of_existing_module does the same lookup but reports a missing module as
// an error, which is wrong here: a graph walk asks about every dependency of every
// module it visits, and a missing one is a fact to record rather than a failure.
fn resolve_existing_module(mod_name string) ?string {
	_, path, ok := candidate_module_path(mod_name)
	if !ok {
		return none
	}
	if os.exists(path) && os.is_dir(path) {
		return path
	}
	return none
}
