fn main() {
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
	println('The shortest path from node A to node F is: $path')
	assert path == ['A', 'C', 'F']
}

// Breadth-First Search (BFS) allows you to ﬁnd the shortest distance between two nodes in the graph.
fn breadth_first_search_path(graph map[string][]string, vertex string, target string) []string {
	mut path := []string{}
	mut visited := []string{init: vertex}
	mut queue := [][][]string{}
	queue << [[vertex], path]
	for queue.len > 0 {
		mut idx := queue.len - 1
		node := queue[idx][0][0]
		path = queue[idx][1]
		queue.delete(idx)
		if node == target {
			path << node
			return path
		}
		for child in graph[node] {
			mut tmp := path.clone()
			if child !in visited {
				visited << child
				tmp << node
				queue << [[child], tmp]
			}
		}
	}
	return path
}
