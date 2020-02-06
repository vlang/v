// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
// Module graph implements solvers based on Graph theory
module graph

import vsl.math
import vsl.errno
import vsl.io
/* TODO: change map[string]* types to map[int]* */


pub enum SorthestPaths {
	fw/* FW: Floyd-Warshall method */

}

// Graph defines a graph structure
pub struct Graph {
pub:
// input
	edges     [][]int // [nedges][2] edges (connectivity)
	weights_e []f64 // [nedges] weights of edges
	verts     [][]f64 // [nverts][ndim] vertices
	weights_v []f64 // [nverts] weights of vertices
	// auxiliary
	shares    map[string][]int // [nverts] edges sharing a vertex
	key2edge  map[string]int // maps (i,j) vertex to edge index
	dist      [][]f64 // [nverts][nverts] distances
	next      [][]int // [nverts][nverts] next tree connection. -1 means no connection
}
// str_ints_map_append appends a new item to a map of slice.
// Note: this function creates a new slice in the map if key is not found.
fn str_ints_map_append(o map[string][]int, key string, item int) map[string][]int {
	mut m := o
	if key in m {
		mut slice := m[key]
		slice << item
		m[key] = slice
	}
	else {
		m[key] = [item]
	}
	return m
}

// graph initialises graph
// edges    -- [nedges][2] edges (connectivity)
// weights_e -- [nedges] weights of edges
// verts    -- [nverts][ndim] vertices
// weights_v -- [nverts] weights of vertices
pub fn graph(edges [][]int, weights_e []f64, verts [][]f64, weights_v []f64) Graph {
	mut key2edge := map[string]int
	mut shares := map[string][]int
	for k, edge in edges {
		i := edge[0]
		j := edge[1]
		shares = str_ints_map_append(shares, i.str(), k)
		shares = str_ints_map_append(shares, j.str(), k)
		key2edge[hash_edge_key(i, j)] = k
	}
	nv := shares.size
	mut dist := [[]f64].repeat(nv)
	mut next := [[]int].repeat(nv)
	for i := 0; i < nv; i++ {
		dist[i] = [f64(0)].repeat(nv)
		next[i] = [0].repeat(nv)
	}
	return Graph{
		edges: edges
		weights_e: weights_e
		verts: verts
		weights_v: weights_v
		shares: shares
		key2edge: key2edge
		dist: dist
		next: next
	}
}

// nverts returns the number of vertices
pub fn (g Graph) nverts() int {
	return g.shares.size
}

// get_edge performs a lookup on key2edge map and returs id of edge for given nodes ides
pub fn (g Graph) get_edge(i, j int) ?int {
	key := hash_edge_key(i, j)
	if key in g.key2edge {
		return g.key2edge[key]
	}
	return errno.vsl_error('cannot find edge from $i to $j', .efailed)
}

// shortest_paths computes the shortest paths in a graph defined as follows
//
// [10]
// 0 ––––––→ 3            numbers in brackets
// |         ↑            indicate weights
// [5] |         | [1]
// ↓         |
// 1 ––––––→ 2
// [3]                ∞ means that there are no
// connections from i to j
// graph:  j= 0  1  2  3
// -----------  i=
// 0  5  ∞ 10 |  0  ⇒  w(0→1)=5, w(0→3)=10
// ∞  0  3  ∞ |  1  ⇒  w(1→2)=3
// ∞  ∞  0  1 |  2  ⇒  w(2→3)=1
// ∞  ∞  ∞  0 |  3
// Input:
// method -- FW: Floyd-Warshall method
pub fn (g Graph) shortest_paths(method SorthestPaths) Graph {
	if method != .fw {
		panic('shortest_paths works with FW (Floyd-Warshall) method only for now')
	}
	g2 := g.calc_dist()
	nv := g2.dist.len
	mut dist := g2.dist
	mut next := g2.next
	for k := 0; k < nv; k++ {
		for i := 0; i < nv; i++ {
			for j := 0; j < nv; j++ {
				sum := dist[i][k] + dist[k][j]
				if dist[i][j] > sum {
					dist[i][j] = sum
					next[i][j] = next[i][k]
				}
			}
		}
	}
	return {
		g2 |
		dist:dist,
		next:next
	}
}

// path returns the path from source (s) to destination (t)
// Note: shortest_paths method must be called first
pub fn (g Graph) path(s, t int) []int {
	s_next := g.next[s]
	n := s_next[t]
	if n < 0 {
		return []
	}
	mut p := [s]
	mut u := s
	for u != t {
		u_next := g.next[u]
		u = u_next[t]
		p << u
	}
	return p
}

// calc_dist computes distances beetween all vertices and initialises 'next' matrix
pub fn (g Graph) calc_dist() Graph {
	mut dist := g.dist
	mut next := g.next
	verts := g.verts
	edges := g.edges
	weights_e := g.weights_e
	nv := dist.len
	for i := 0; i < nv; i++ {
		for j := 0; j < nv; j++ {
			if i == j {
				dist[i][j] = 0
			}
			else {
				dist[i][j] = math.max_f64
			}
			next[i][j] = -1
		}
	}
	for k, edge in edges {
		i := edge[0]
		j := edge[1]
		mut d := 1.0
		if verts.len != 0 {
			d = 0.0
			xa := verts[i]
			xb := verts[j]
			for dim := 0; dim < xa.len; dim++ {
				d += math.pow(xa[dim] - xb[dim], 2.0)
			}
			d = math.sqrt(d)
		}
		if weights_e.len != 0 {
			d *= weights_e[k]
		}
		dist[i][j] = d
		next[i][j] = j
		if dist[i][j] < 0 {
			panic('distance between vertices cannot be negative: $dist[i][j]')
		}
	}
	return {
		g |
		dist:dist,
		next:next
	}
}

// hash_edge_key creates a unique hash key identifying an edge
fn hash_edge_key(i, j int) string {
	key := i + 10000001 * j
	return key.str()
}

// str_dist_matrix returns a string representation of dist matrix
pub fn (g Graph) str_dist_matrix() string {
	nv := g.dist.len
	mut maxlen := 0
	for i := 0; i < nv; i++ {
		for j := 0; j < nv; j++ {
			i_dist := g.dist[i]
			if i_dist[j] < math.max_f64 {
				i_dist_str := io.safe_print<f64>('%g', i_dist[j])
				maxlen = int(math.max(maxlen, i_dist_str.len))
			}
		}
	}
	mut l := ''
	maxlen = int(math.max(3, maxlen))
	fmts := io.safe_print<int>('%%%ds', maxlen + 1)
	fmtn := io.safe_print<int>('%%%dg', maxlen + 1)
	for i := 0; i < nv; i++ {
		for j := 0; j < nv; j++ {
			i_dist := g.dist[i]
			if i_dist[j] < math.max_f64 {
				l += io.safe_print<f64>(fmtn, i_dist[j])
			}
			else {
				l += io.safe_print<string>(fmts, '∞')
			}
		}
		l += '\n'
	}
	return l
}

// get_adj returns adjacency list as a compressed storage format for METIS
pub fn (g Graph) get_adj() ([]int,[]int) {
	nv := g.nverts()
	mut szadj := 0
	for vid := 0; vid < nv; vid++ {
		szadj += g.shares[vid.str()].len // = number of connected vertices
	}
	mut xadj := [0].repeat(nv + 1)
	mut adjncy := [0].repeat(szadj)
	mut k := 0
	for vid := 0; vid < nv; vid++ {
		edges := g.shares[vid.str()]
		for eid in edges {
			edge := g.edges[eid]
			mut other_vid := edge[0]
			if other_vid == vid {
				other_vid = edge[1]
			}
			adjncy[k] = other_vid
			k++
		}
		xadj[1 + vid] = xadj[vid] + edges.len
	}
	return xadj,adjncy
}
