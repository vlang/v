// The idea of this algorithm follow :
// https://www.gatevidyalay.com/topological-sort-topological-sorting/ (GREEDY)
// (no cycles are detected)
// https://en.wikipedia.org/wiki/Topological_sorting ... just the input data
// and the Kahn algorithm
// Author: CCS

// the idea is rude: https://www.gatevidyalay.com/topological-sort-topological-sorting/
fn topog_sort_greedy(graph map[string][]string) []string {
	n_nodes := graph.len // numbers of nodes of this graph
	mut top_order := []string{} // a vector with sequence of nodes visited
	mut count := 0
	/*
	IDEA ( a greedy algorythm ):

	 1. choose allways the node with smallest input degree
	 2. visit it
	 3. put it in the output vector
	 4. remove it from graph
	 5. update the graph (a new graph)
	 6. find a new vector degree
	 7. until all nodes has been visited
     Back to step 1 (used the variable count)

	 Maybe it seems the Kahn's algorithm
	*/
	mut v_degree := in_degree(graph) // return: map [string] int
	print('V Degree ${v_degree}')
	mut small_degree := min_degree(v_degree)
	mut new_graph := remove_node_from_graph(small_degree, graph)
	top_order << small_degree
	count++

	for (count < n_nodes) {
		v_degree = in_degree(new_graph) // return: map [string] int
		print('\nV Degree ${v_degree}')
		small_degree = min_degree(v_degree)
		new_graph = remove_node_from_graph(small_degree, new_graph)

		top_order << small_degree
		count++
	}
	// print("\n New Graph ${new_graph}")

	return top_order
}

// Give a node, return a list with all nodes incidents or fathers of this node
fn all_fathers(node string, a_map map[string][]string) []string {
	mut array_of_keys := a_map.keys() // get a key of this map
	mut all_incident := []string{}
	for i in array_of_keys {
		// in : function
		if node in a_map[i] {
			all_incident << i // a queue of this search
		}
	}
	return all_incident
}

// Input: a map with input degree values, return the key with smallest value
fn min_degree(a_map map[string]int) string {
	mut array_of_keys := a_map.keys() // get a key of this map
	mut key_min := array_of_keys.first()
	mut val_min := a_map[key_min]
	// print("\n MIN: ${val_min} \t  key_min: ${key_min}  \n the map inp_degree: ${a_map}")
	for i in array_of_keys {
		// there is a smaller
		if val_min > a_map[i] {
			val_min = a_map[i]
			key_min = i
		}
	}
	return key_min // the key with smallest value
}

// Given a graph ... return a list of integer with degree of each node
fn in_degree(a_map map[string][]string) map[string]int {
	mut array_of_keys := a_map.keys() // get a key of this map
	// print(array_of_keys)
	mut degree := map[string]int{}
	for i in array_of_keys {
		degree[i] = all_fathers(i, a_map).len
	}
	// print("\n Degree ${in_degree}" )
	return degree // a vector of the indegree graph
}

// REMOVE A NODE FROM A GRAPH AND RETURN ANOTHER GRAPH
fn remove_node_from_graph(node string, a_map map[string][]string) map[string][]string {
	// mut new_graph := map [string] string {}
	mut new_graph := a_map.clone() // copy the graph
	new_graph.delete(node)
	mut all_nodes := new_graph.keys() // get all nodes of this graph
	// FOR THE FUTURE with filter
	// for i in all_nodes {
	//	   new_graph[i] = new_graph[i].filter(index(it) != node)	
	// }
	// A HELP FROM V discussion	 GITHUB - thread
	for key in all_nodes {
		i := new_graph[key].index(node)
		if i >= 0 {
			new_graph[key].delete(i)
		}
	}
	// print("\n NEW ${new_graph}" )
	return new_graph
}

fn main() {
	// A map illustration to use in a graph
	// adjacency matrix
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
		'H': []
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

	println('\nA Topological Sort of G1:  ${topog_sort_greedy(graph_01)}')
	println('\nA Topological Sort of G2:  ${topog_sort_greedy(graph_02)}')
	println('\nA Topological Sort of G3:  ${topog_sort_greedy(graph_03)}')
	// ['2', '9', '10', '11', '5', '8', '7', '3']
}
