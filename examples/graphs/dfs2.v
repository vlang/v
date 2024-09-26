// Author: CCS & KeitoTobi1
// Backtracking Supported.
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
	println('\n Graph_01: all path pattern from node A to node F is:')
	print_pattern(path_01)
	path_02 := depth_first_search_path(graph_02, 'A', 'H')
	println('\n Graph_02: all path pattern from node A to node F is:')
	print_pattern(path_02)
}

fn depth_first_search_path(adj map[string][]string, start string, target string) [][]string {
	mut sol := Solution{}
	mut path := []string{}
	mut visited := visited_init(adj)

	// false ... not visited yet: {'A': false, 'B': false, 'C': false, 'D': false, 'E': false}
	sol.find_pattern(adj, mut visited, start, target, mut path)
	return sol.pattern
}

struct Solution {
pub mut:
	pattern [][]string
}

fn (mut s Solution) find_pattern(adj map[string][]string, mut visited map[string]bool, node string, target string,
	mut path []string) {
	path << node
	visited[node] = true
	if node == target {
		print('\n Founded pattern: ${path}')
		s.pattern << [path]
	}
	print('\n Exploring of node ${node} (true/false): ${adj[node]}')
	for i, _ in adj[node] {
		if !visited[adj[node][i]] {
			mut temp := path.clone()
			s.find_pattern(adj, mut visited, adj[node][i], target, mut temp)
		}
	}
	visited[node] = false
	print('\n Current: ${node} (only not visited) \n Visited: ${visited}')
}

fn print_pattern(pat [][]string) {
	for p in pat {
		println(p)
	}
}

// Creating aa map to initialize with of visited nodes .... all with false in the init
// so these nodes are NOT VISITED YET
fn visited_init(adj map[string][]string) map[string]bool {
	mut temp := map[string]bool{}
	for i, _ in adj {
		temp[i] = false
	}
	return temp
}
