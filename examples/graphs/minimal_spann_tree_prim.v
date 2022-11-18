/*
Exploring  PRIMS,
The data example is from
https://www.geeksforgeeks.org/prims-minimum-spanning-tree-mst-greedy-algo-5/

by CCS

PS: all the pre-requisites of Dijkstra are considered

$ v run file_name.v

Creating a executable
$ v -o an_executable.EXE run file_name.v
$ ./an_executable.EXE

Code based from : Data Structures and Algorithms Made Easy: Data Structures and Algorithmic Puzzles, Fifth Edition (English Edition)
pseudo code written in C
This idea is quite different: it uses a priority queue to store the current
shortest path evaluted
The priority queue structure built using a list to simulate
the queue. A heap is not used in this case.
*/

// a structure
struct NODE {
mut:
	data     int // number of nodes
	priority int // Lower values priority indicate ==> higher priority
}

// Function to push according to priority ... the lower priority is goes ahead
// The "push" always sorted in pq
fn push_pq<T>(mut prior_queue []T, data int, priority int) {
	mut temp := []T{}
	lenght_pq := prior_queue.len

	mut i := 0
	for (i < lenght_pq) && (priority > prior_queue[i].priority) {
		temp << prior_queue[i]
		i++
	}
	// INSERTING SORTED in the queue
	temp << NODE{data, priority} // do the copy in the right place
	// copy the another part (tail) of original prior_queue
	for i < lenght_pq {
		temp << prior_queue[i]
		i++
	}
	prior_queue = temp.clone() // I am not sure if it the right way
}

// Change the priority of a value/node ... exist a value, change its priority
fn updating_priority<T>(mut prior_queue []T, search_data int, new_priority int) {
	mut i := 0
	mut lenght_pq := prior_queue.len

	for i < lenght_pq {
		if search_data == prior_queue[i].data {
			prior_queue[i] = NODE{search_data, new_priority} // do the copy in the right place	
			break
		}
		i++
		// all the list was examined
		if i >= lenght_pq {
			// print('\n Priority Queue:  ${prior_queue}')		
			// print('\n These data ${search_data} and ${new_priority} do not exist ... PRIORITY QUEUE problem\n')
			// if it does not find ... then push it
			push_pq(mut prior_queue, search_data, new_priority)
			// exit(1) // panic(s string)
		}
	} // end for
}

// a single departure or remove from queue
fn departure_priority<T>(mut prior_queue []T) int {
	mut x := prior_queue[0].data
	prior_queue.delete(0) // or .delete_many(0, 1 )
	return x
}

// give a NODE v, return a list with all adjacents
// Take care, only positive EDGES
fn all_adjacents<T>(g [][]T, v int) []int {
	mut temp := []int{}
	for i in 0 .. (g.len) {
		if g[v][i] > 0 {
			temp << i
		}
	}
	return temp
}

// print the costs from origin up to all nodes
// A utility function to print the
// constructed MST stored in parent[]
// print all  paths and their cost or weight
fn print_solution(path []int, g [][]int) {
	// print(' PATH:  ${path} ==> ${path.len}')
	print('   Edge \tWeight\n')
	mut sum := 0
	for node in 0 .. (path.len) {
		if path[node] == -1 {
			print('\n ${node} <== reference or start node')
		} else {
			print('\n ${node} <--> ${path[node]} \t${g[node][path[node]]}')
			sum += g[node][path[node]]
		}
	}
	print('\n Minimum Cost Spanning Tree: ${sum}\n\n')
}

// check structure from: https://www.geeksforgeeks.org/dijkstras-shortest-path-algorithm-greedy-algo-7/
// s: source for all nodes
// Two results are obtained ... cost and paths
fn prim_mst(g [][]int, s int) {
	mut pq_queue := []NODE{} // creating a priority queue
	push_pq(mut pq_queue, s, 0) // goes s with priority 0
	mut n := g.len

	mut dist := []int{len: n, init: -1} // dist with -1 instead of INIFINITY
	mut path := []int{len: n, init: -1} // previous node of each shortest paht

	// Distance of source vertex from itself is always 0
	dist[s] = 0

	for pq_queue.len != 0 {
		mut v := departure_priority(mut pq_queue)
		// for all W adjcents vertices of v
		mut adjs_of_v := all_adjacents(g, v) // all_ADJ of v ....
		// print('\n :${dist} :: ${pq_queue}')
		// print('\n ADJ ${v} is ${adjs_of_v}')
		mut new_dist := 0
		for w in adjs_of_v {
			new_dist = dist[v] + g[v][w]

			if dist[w] == -1 {
				dist[w] = g[v][w]
				push_pq(mut pq_queue, w, dist[w])
				path[w] = v // collecting the previous node -- lowest weight
			}

			if dist[w] > new_dist {
				dist[w] = g[v][w] // new_dist//
				updating_priority(mut pq_queue, w, dist[w])
				path[w] = v // father / previous node
			}
		}
	}

	// print('\n \n Previous node of shortest path: ${path}')
	// print_paths_dist(path , dist)
	print_solution(path, g)
}

/*
Solution Expected graph_02
Edge   Weight
0 - 1    2
1 - 2    3
0 - 3    6
1 - 4    5
*/

fn main() {
	// adjacency matrix = cost or weight
	graph_01 := [
		[0, 4, 0, 0, 0, 0, 0, 8, 0],
		[4, 0, 8, 0, 0, 0, 0, 11, 0],
		[0, 8, 0, 7, 0, 4, 0, 0, 2],
		[0, 0, 7, 0, 9, 14, 0, 0, 0],
		[0, 0, 0, 9, 0, 10, 0, 0, 0],
		[0, 0, 4, 14, 10, 0, 2, 0, 0],
		[0, 0, 0, 0, 0, 2, 0, 1, 6],
		[8, 11, 0, 0, 0, 0, 1, 0, 7],
		[0, 0, 2, 0, 0, 0, 6, 7, 0],
	]

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
		println('\n Minimal Spanning Tree of graph ${index + 1} using PRIM algorithm')
		graph = g_value.clone() // graphs_sample[g].clone() // choice your SAMPLE
		// starting by node x ... see the graphs dimmension
		start_node := 0
		prim_mst(graph, start_node)
	}
	println('\n BYE -- OK')
}
