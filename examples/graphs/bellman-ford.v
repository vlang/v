/*
A V program for Bellman-Ford's single source
shortest path algorithm.
literaly adapted from:
https://www.geeksforgeeks.org/bellman-ford-algorithm-dp-23/
// Adapted from this site... from C++ and Python codes

For Portugese reference
http://rascunhointeligente.blogspot.com/2010/10/o-algoritmo-de-bellman-ford-um.html

code by CCS
*/

const large = 999999 // almost inifinity

// a structure to represent a weighted edge in graph
struct EDGE {
mut:
	src    int
	dest   int
	weight int
}

// building a map of with all edges etc of a graph, represented from a matrix adjacency
// Input: matrix adjacency --> Output: edges list of src, dest and weight
fn build_map_edges_from_graph<T>(g [][]T) map[T]EDGE {
	n := g.len // TOTAL OF NODES for this graph -- its dimmension
	mut edges_map := map[int]EDGE{} // a graph represented by map of edges

	mut edge := 0 // a counter of edges
	for i in 0 .. n {
		for j in 0 .. n {
			// if exist an arc ... include as new edge
			if g[i][j] != 0 {
				edges_map[edge] = EDGE{i, j, g[i][j]}
				edge++
			}
		}
	}
	// print('${edges_map}')
	return edges_map
}

fn print_sol(dist []int) {
	n_vertex := dist.len
	print('\n Vertex   Distance from Source')
	for i in 0 .. n_vertex {
		print('\n   $i   -->   ${dist[i]}')
	}
}

// The main function that finds shortest distances from src
// to all other vertices using Bellman-Ford algorithm.  The
// function also detects negative weight cycle
fn bellman_ford<T>(graph [][]T, src int) {
	mut edges := build_map_edges_from_graph(graph)
	// this function was done to adapt a graph representation
	// by a adjacency matrix, to list of adjacency (using a MAP)
	n_edges := edges.len // number of EDGES

	// Step 1: Initialize distances from src to all other
	// vertices as INFINITE
	n_vertex := graph.len // adjc matrix ... n nodes or vertex
	mut dist := []int{len: n_vertex, init: large} // dist with -1 instead of INIFINITY
	// mut path := []int{len: n , init:-1} // previous node of each shortest paht
	dist[src] = 0

	// Step 2: Relax all edges |V| - 1 times. A simple
	// shortest path from src to any other vertex can have
	// at-most |V| - 1 edges

	for _ in 0 .. n_vertex {
		for j in 0 .. n_edges {
			mut u := edges[j].src
			mut v := edges[j].dest
			mut weight := edges[j].weight
			if (dist[u] != large) && (dist[u] + weight < dist[v]) {
				dist[v] = dist[u] + weight
			}
		}
	}

	// Step 3: check for negative-weight cycles.  The above
	// step guarantees shortest distances if graph doesn't
	// contain negative weight cycle.  If we get a shorter
	// path, then there is a cycle.
	for j in 0 .. n_vertex {
		mut u := edges[j].src
		mut v := edges[j].dest
		mut weight := edges[j].weight
		if (dist[u] != large) && (dist[u] + weight < dist[v]) {
			print('\n Graph contains negative weight cycle')
			// If negative cycle is detected, simply
			// return or an exit(1)
			return
		}
	}
	print_sol(dist)
}

fn main() {
	// adjacency matrix = cost or weight
	graph_01 := [
		[0, -1, 4, 0, 0],
		[0, 0, 3, 2, 2],
		[0, 0, 0, 0, 0],
		[0, 1, 5, 0, 0],
		[0, 0, 0, -3, 0],
	]
	// data from https://www.geeksforgeeks.org/bellman-ford-algorithm-dp-23/

	graph_02 := [
		[0, 2, 0, 6, 0],
		[2, 0, 3, 8, 5],
		[0, 3, 0, 0, 7],
		[6, 8, 0, 0, 9],
		[0, 5, 7, 9, 0],
	]
	// data from https://www.geeksforgeeks.org/prims-minimum-spanning-tree-mst-greedy-algo-5/
	/*
	The graph:
        2    3
    (0)--(1)--(2)
    |    / \    |
   6|  8/   \5  |7
    |  /     \  |
    (3)-------(4)
         9
	*/

	/*
	Let us create following weighted graph
 From https://www.geeksforgeeks.org/kruskals-minimum-spanning-tree-algorithm-greedy-algo-2/?ref=lbp
                   10
              0--------1
              |  \     |
             6|   5\   |15
              |      \ |
              2--------3
                  4
	*/
	graph_03 := [
		[0, 10, 6, 5],
		[10, 0, 0, 15],
		[6, 0, 0, 4],
		[5, 15, 4, 0],
	]

	// To find number of coluns
	// mut cols := an_array[0].len
	mut graph := [][]int{} // the graph: adjacency matrix
	// for index, g_value in [graph_01, graph_02, graph_03] {
	for index, g_value in [graph_01, graph_02, graph_03] {
		graph = g_value.clone() // graphs_sample[g].clone() // choice your SAMPLE
		// allways starting by node 0
		start_node := 0
		println('\n\n Graph ${index + 1} using Bellman-Ford algorithm (source node: $start_node)')
		bellman_ford(graph, start_node)
	}
	println('\n BYE -- OK')
}
