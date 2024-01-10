// https://en.wikipedia.org/wiki/Topological_sorting
// A DFS RECURSIVE ALGORITHM ....
// An alternative algorithm for topological sorting is based on depth-first search. The algorithm loops through each node of the graph, in an arbitrary order, initiating a depth-first search that terminates when it hits any node that has already been visited since the beginning
// of the topological sort or the node has no outgoing edges (i.e. a leaf node)
// Discussion: https://www.gatevidyalay.com/topological-sort-topological-sorting/
// $ v run dfs_topological_ordering.v
// Author: CCS

// THE DFS RECURSIVE .... classical searchig for leaves nodes
// the arguments are used in the function to avoid global variables....
fn dfs_recursive(u string, mut visited map[string]bool, graph map[string][]string, mut top_sorting []string) {
	print(' Visiting: ${u} -> ')
	visited[u] = true

	for v in graph[u] {
		if visited[v] == false {
			dfs_recursive(v, mut visited, graph, mut top_sorting)
		}
	}
	top_sorting << u
}

// Creating aa map to initialize with of visited nodes .... all with false in the init
// so these nodes are NOT VISITED YET
fn visited_init(a_graph map[string][]string) map[string]bool {
	mut array_of_keys := a_graph.keys() // get all keys of this map
	mut temp := map[string]bool{} // attention in these initializations with maps
	for i in array_of_keys {
		temp[i] = false
	}
	return temp
}

// attention here a map STRING ---> ONE BOOLEAN ... not a string

fn main() {
	// A map illustration to use in a graph
	// the graph: adjacency matrix
	graph_01 := {
		'A': ['C', 'B']
		'B': ['D']
		'C': ['D']
		'D': []
	}

	graph_02 := {
		'A': ['B', 'C', 'D']
		'B': ['E']
		'C': ['F']
		'D': ['G']
		'E': ['H']
		'F': ['H']
		'G': ['H']
		'H': [] // no cycles
	}
	// from: https://en.wikipedia.org/wiki/Topological_sorting
	graph_03 := {
		'5':  ['11']
		'7':  ['11', '8']
		'3':  ['8', '10']
		'11': ['2', '9', '10']
		'8':  ['9']
		'2':  []
		'9':  []
		'10': []
	}

	mut graph := map[string][]string{} // the graph: adjacency matrix
	for index, g_value in [graph_01, graph_02, graph_03] {
		println('Topological sorting for the graph ${index} using a DFS recursive')
		graph = g_value.clone() // graphs_sample[g].clone() // choice your SAMPLE

		// mut n_nodes := graph.len
		mut visited := visited_init(graph) // a map with nodes not visited

		// mut start := (graph.keys()).first() // arbitrary, any node if you wish
		mut top_sorting := []string{}
		// advantages of map ... getting all nodes
		for i in graph.keys() {
			if visited[i] != true {
				dfs_recursive(i, mut visited, graph, mut top_sorting)
			}
		}

		print('\n A topological sorting of graph ${index} : ')
		// println(g_value)
		println(top_sorting.reverse())
		println('')
	} // End of for
}
