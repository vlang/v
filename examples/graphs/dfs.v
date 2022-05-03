// Author: ccs
// I follow literally code in C, done many years ago

fn main() {
	// Adjacency matrix as a map	
	// Example 01
	graph_01 := {
		'A': ['B', 'C']
		'B': ['A', 'D', 'E']
		'C': ['A', 'F']
		'D': ['B']
		'E': ['F', 'B', 'F']
		'F': ['C', 'E']
	}
	// Example 02
	graph_02 := {
		'A': ['B', 'C', 'D']
		'B': ['E']
		'C': ['F']
		'D': ['E']
		'E': ['H']
		'F': ['H']
		'G': ['H']
		'H': ['E', 'F', 'G']
	}
	// println('Graph: $graph')
	path_01 := depth_first_search_path(graph_01, 'A', 'F')
	println('\n Graph_01: a first path from node A to node F is: $path_01.reverse()')
	path_02 := depth_first_search_path(graph_02, 'A', 'H')
	println('\n Graph_02: a first path from node A to node F is: $path_02.reverse()')
}

// Depth-First Search (BFS) allows you to find a path between two nodes in the graph.
fn depth_first_search_path(graph map[string][]string, start string, target string) []string {
	mut path := []string{} // ONE PATH with SUCCESS = array
	mut stack := []string{} // a stack ... many nodes
	// all_nodes := graph.keys() // get a key of this map
	n_nodes := graph.len // numbers of nodes of this graph
	mut visited := a_map_nodes_bool(n_nodes) // a map fully
	// false ... not visited yet: {'A': false, 'B': false, 'C': false, 'D': false, 'E': false}

	stack << start // first push on the stack
	for stack.len > 0 {
		mut node := stack.pop() // get the top node and remove it from the stack

		// check if this node is already visited
		if visited[node] == false {
			// if no ... test it searchin for a final node
			visited[node] = true // means: node visited
			if node == target {
				path = build_path_reverse(graph, start, node, visited)
				return path
			}
			//  Exploring of node removed from  stack and add its relatives
			print('\n Exploring of node $node (true/false): ${graph[node]}')
			// graph[node].reverse() take a classical choice for DFS
			// at most os left in this case.
			// use vertex in graph[node] the choice is right

			// take  all nodes from the node
			for vertex in graph[node].reverse() {
				// println("\n ...${vertex}")
				// not explored yet
				if visited[vertex] == false {
					stack << vertex
				}
			}
			print('\n Stack: $stack (only not visited) \n Visited: $visited')
		}
	}
	path = ['Path not found, problem in the Graph, start or end nodes! ']
	return path
}

// Creating a map for nodes not VISITED visited ...
// starting by false ===> means this node was not visited yet
fn a_map_nodes_bool(size int) map[string]bool {
	mut my_map := map[string]bool{} // look this map ...
	for i in 0 .. size {
		my_map[u8(65 + i).ascii_str()] = false
	}
	return my_map
}

// Based in the current node that is final, search for his parent, that is already visited, up to the root or start node
fn build_path_reverse(graph map[string][]string, start string, final string, visited map[string]bool) []string {
	print('\n\n Nodes visited (true) or no (false): $visited')
	array_of_nodes := graph.keys()
	mut current := final
	mut path := []string{}
	path << current

	for current != start {
		for i in array_of_nodes {
			if (current in graph[i]) && (visited[i] == true) {
				current = i
				break // the first ocurrence is enough
			}
		}
		path << current // updating the path tracked
	}
	return path
}
