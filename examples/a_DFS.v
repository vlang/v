//
// Depth-First Search (BFS) allows you to ﬁnd a path between two nodes in the graph.
// I  follow literally code in C, done many years ago
// coded by CCS
//
fn depth_first_search_path(graph map[string][]string, start string, target string) []string 
 {
	mut path := []string{} // ONE PATH with SUCCESS = array
	mut stack := []string{} // a stack ... many nodes 
	//all_nodes := graph.keys() // get a key of this map
	n_nodes := graph.len // numbers of nodes of this graph
	mut visited := a_map_nodes_bool (n_nodes) // a map fully
	//false ... not visited yet: {'A': false, 'B': false, 'C': false, 'D': false, 'E': false}
	
	stack.prepend(start) // first push .... prepend = PUSH on the stack
	for (stack.len != 0) // or  WHILE (stack.len > 0)
	{  //a pull from the stack
       mut node := get_from_top( mut stack ) // get the front node and remove it
	  
	   if  visited[ node ] == false
		{// check if this node is already visited
		  // if no ... test it searchin for a final node
		  visited[node] = true // means: node visited
		  if node == target {
		    path = build_path_reverse(graph, start, node, visited)
			return path
		}
	//  Exploring of node removed from  stack and add its relatives
	print("\n Exploring of node ${node} (true/false): ${graph[node]}")
	//graph[node].reverse() take a classical choice for DFS
	// at most os left in this case. 
	// use vertex in graph[node] the choice is right
	for vertex in graph[node].reverse()  // take  all nodes from the node
		{
		  //println("\n ...${vertex}")	
          if visited[vertex] == false // not explored yet
		  {
           stack.prepend( vertex )
		  }
		}
		print("\n Stack: ${stack} (only not visited) \n Visited: ${visited}")
		}
	} 
	path = ['Path not found, problem in the Graph, start or end nodes! ']
	return path
}

fn main() {
// Adjacency matrix as a map	
//Example 01
	graph_01 := {
		'A': ['B', 'C']
		'B': ['A', 'D', 'E']
		'C': ['A', 'F']
		'D': ['B']
		'E': ['F', 'B', 'F']
		'F': ['C', 'E']
	}
	//Example 02
	graph_02 := {
		'A': ['B', 'C','D']
		'B': ['E']
		'C': ['F']
		'D': ['E']
		'E': ['H']
		'F': ['H']
		'G': ['H']
		'H':  ['E', 'F','G']
	}
	//println('Graph: $graph')
	path_01 := depth_first_search_path(graph_01, 'A', 'F')
	println('\n Graph_01: a first path from node A to node F is: ${path_01.reverse()}')
	path_02 := depth_first_search_path(graph_02, 'A', 'H')
	println('\n Graph_02: a first path from node A to node F is: ${path_02.reverse()}')

}
//////////// AUXILIAR FUNCTIONS //////////////////
// Creating a map for nodes not VISITED visited ... 
// starting by false ===> means this node 
// was not visited yet
fn a_map_nodes_bool (size int)  map [string]  bool {
	mut my_map := map [string] bool {} // look this map ...
	base := 65
	mut	key := byte(base).ascii_str()
	for i in 0 .. size {
		key = byte(base+i).ascii_str()
		my_map[key] = false  
	}
   	return my_map
 }

// classical a departure node  from stack
fn get_from_top(mut a_stack [] string) string{
   mut x := a_stack.first() //a.first() equivalent to a[0]
   //REMOVE IT
   a_stack.delete(0) // del x in the position 0
   return x
}

// Based in the current node that is final,
// search for his father, already visited, up to the root or
// start node
fn build_path_reverse(graph map[string][]string, start string, final string, visited map [string] bool) []string {

print("\n\n Nodes visited (true) or no (false): ${visited}")
array_of_nodes := graph.keys()   
mut current := final
mut path := []string {}
path << current

for ( current != start)
{
 	for i in array_of_nodes 
	{
	 if (current in graph[i]) && (visited[i]==true)
	   { current = i
		 break // the first ocurrence is enough
	   }
	}
 	path << current //updating the path tracked
}
return path
}

/*

SOME TESTS
>>> mut stack := []int{}
>>> stack << [2]
>>> stack << [21,45,67]
>>> print(stack)
[2, 21, 45, 67]
>>> mut x := stack[0]
>>> print(x)
21
>>> stack << [77]
>>> print(stack)
[21, 45, 67, 77]

>>> println(graph['F'])
['C', 'E']
 
 */