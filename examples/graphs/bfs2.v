// Author: CCS
// I follow literally code in C, done many years ago
fn main() {
	// Adjacency matrix as a map	
	graph := {
		'A': ['B', 'C']
		'B': ['A', 'D', 'E']
		'C': ['A', 'F']
		'D': ['B']
		'E': ['B', 'F']
		'F': ['C', 'E']
	}
	println('Graph: $graph')
	path := breadth_first_search_path(graph, 'A', 'F')
	println('\n The shortest path from node A to node F is: $path.reverse()')
}

// Breadth-First Search (BFS) allows you to find the shortest distance between two nodes in the graph.
fn breadth_first_search_path(graph map[string][]string, start string, target string) []string {
	mut path := []string{} // ONE PATH with SUCCESS = array
	mut queue := []string{} // a queue ... many paths
	// all_nodes := graph.keys() // get a key of this map
	// a map to store all the nodes visited to avoid cycles
	// start all them with False, not visited yet
	mut visited := visited_init(graph) // a map fully
	// false ==> not visited yet: {'A': false, 'B': false, 'C': false, 'D': false, 'E': false}
	queue << start // first arrival
	for queue.len != 0 {
		mut node := departure(mut queue) // get the front node and remove it
		if visited[node] == false { // check if this node is already visited
			// if no ... test it searchinf for a final node
			visited[node] = true // means: visit this node
			if node == target {
				path = build_path_reverse(graph, start, node, visited)
				return path
			}
			// Expansion of node removed from  queue
			print('\n Expansion of node $node (true/false): ${graph[node]}')
			// take  all nodes from the node
			for vertex in graph[node] { // println("\n ...${vertex}")	
				// not explored yet
				if visited[vertex] == false {
					queue << vertex
				}
			}
			print('\n QUEUE: $queue (only not visited) \n Visited: $visited')
		}
	}
	path = ['Path not found, problem in the Graph, start or end nodes! ']
	return path
}

// classical removing of a node from the start of a queue
fn departure(mut queue []string) string {
	mut x := queue[0]
	queue.delete(0)
	return x
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

// Based in the current node that is final, search for its parent, already visited, up to the root or start node
fn build_path_reverse(graph map[string][]string, start string, final string, visited map[string]bool) []string {
	print('\n\n Nodes visited (true) or no (false): $visited')
	array_of_nodes := graph.keys()
	mut current := final
	mut path := []string{}
	path << current

	for (current != start) {
		for i in array_of_nodes {
			if (current in graph[i]) && (visited[i] == true) {
				current = i
				break // the first ocurrence is enough
			}
		}
		path << current // update the path tracked
	}
	return path
}

//======================================================
