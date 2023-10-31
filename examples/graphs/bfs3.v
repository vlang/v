// Author: CCS & KeitoTobi1
// Backtracking Supported.
import datatypes

fn find_pattern(adj map[string][]string, start string, target string) [][]string {
	mut visited := visited_init(adj)
	mut queue := datatypes.Queue[[]string]{}
	mut res := [][]string{}
	queue.push([start])
	for !queue.is_empty() {
		mut v := queue.pop() or { panic(err) }
		node := v.last()
		if node == target {
			res << v
		}
		print('Expansion of node ${node} (true/false): ${adj[node]}')
		for next in adj[node] {
			if visited[next] == false {
				mut copy := v.clone()
				copy << next
				queue.push(copy)
				visited[node] = true
			}
		}
		print('Expansion of node ${queue} (true/false): ${visited}\n')
	}
	return res
}

fn print_pattern(pat [][]string) {
	for p in pat {
		println(p)
	}
}

fn visited_init(adj map[string][]string) map[string]bool {
	mut temp := map[string]bool{}
	for i, _ in adj {
		temp[i] = false
	}
	return temp
}

fn main() {
	adj := {
		'A': ['B', 'C']
		'B': ['A', 'D', 'E']
		'C': ['A', 'F']
		'D': ['B']
		'E': ['B', 'F']
		'F': ['C', 'E']
	}

	path := find_pattern(adj, 'A', 'F')
	println('The all pattern path from node A to node F is:')
	print_pattern(path)
}
