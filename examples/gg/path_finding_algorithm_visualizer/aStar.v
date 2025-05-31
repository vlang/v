module main

import gg // actual graphics lib
import gx // lib have some constants like colors 
import math // for math related function

const window_width = 800
const window_height = 800
const nrows = 50

// app struct that has property of current windows
struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	ui    Ui
	grid  [][]Cell
	start Point // start point of algorithm
	end   Point // end point or target point
}

// this needed to get the width and mouse position part of gg window
struct Ui {
mut:
	dpi_scale f32
}

// struct for a point
struct Point {
mut:
	x int
	y int
}

/*
RED -> Closed
GREEN -> Open
BLACK -> Barrier
WHITE -> Empty
ORANGE -> Start
TURQOIISE -> End
PINK -> Path
*/

// struct for a cell of grid
struct Cell {
mut:
	row       int
	col       int
	width     int
	pos       Point
	color     gx.Color
	flag      int // 0->empty, 1-> closed, 2-> open, 3-> barrier, 4-> start, 5-> end, 6-> path
	neighbors []Point
}

// this is a node for priority queue

struct Node {
mut:
	f_score int
	cell    Point
	count   int
}

// Min heap or priority queue

struct MinHeap {
mut:
	data []Node
}

// main function
fn main() {
	// app variable
	mut app := &App{}

	// setting values of app
	app.gg = gg.new_context(
		bg_color:      gx.black      // background color
		width:         window_width  // window width
		height:        window_height // window height
		create_window: true          // this will create a different window
		window_title:  'A* Path finding algorithm visusalizer' // title of the window
		frame_fn:      frame       // this is frame function update the frame
		event_fn:      on_event    // it calls on every event
		init_fn:       init_images // run at start of application
		user_data:     app         // store user data
	)
	mut grid := initialise_grid() // initialize the grid variable and populate the matrix with each cell as empty
	app.grid = grid // set grid to app attribute so you can access it by just passing app variable or with method of app
	app.ui.dpi_scale = 1.0 // set scale this is use to make it responsive
	app.start = &Point{ // set start point to -1, -1
		x: -1
		y: -1
	}
	app.end = &Point{ // set end point to -1, -1
		x: -1
		y: -1
	}
	app.gg.run() // run the app loop
}

// this function will run for every frame actually in a loop
fn frame(mut app App) {
	app.gg.begin()
	draw_grid(mut app, mut app.grid)
	draw_gridlines(mut app)
	app.gg.end()
}

// this will run at start of app
fn init_images(mut app App) {
	//	app.resize()
	return
}

// this will handle user event which is stored in gg.event variable
fn on_event(event &gg.Event, mut app App) {
	match event.typ {
		.mouse_down {
			x := int(event.mouse_x / app.ui.dpi_scale)
			y := int(event.mouse_y / app.ui.dpi_scale)
			btn := event.mouse_button
			app.handle_mouse_event(x, y, btn)
		}
		.key_down {
			app.on_key_down(event.key_code)
		}
		else {}
	}
}

// handle mouse event to make a cell either start  point end point or barrier or to clear
fn (mut app App) handle_mouse_event(x int, y int, btn_type gg.MouseButton) {
	gap := window_width / nrows
	row := int(y / gap)
	col := int(x / gap)
	match btn_type {
		.left {
			if app.start.x == -1 && !(row == app.end.y && col == app.end.x) {
				app.start.x = col
				app.start.y = row
				set_cell_type(mut app.grid, app.start.y, app.start.x, 'start')
			} else if app.end.x == -1 && !(row == app.start.y && col == app.start.x) {
				app.end.x = col
				app.end.y = row
				set_cell_type(mut app.grid, app.end.y, app.end.x, 'end')
			} else if !(row == app.start.y && col == app.start.x) && !(row == app.end.y
				&& col == app.end.x) {
				set_cell_type(mut app.grid, row, col, 'barrier')
			}
		}
		.right {
			if row == app.start.y && col == app.start.x {
				app.start.x = -1
				app.start.y = -1
			}
			if row == app.end.y && col == app.end.x {
				app.end.x = -1
				app.end.y = -1
			}

			set_cell_type(mut app.grid, row, col, 'reset')
		}
		else {}
	}
}

// handle keyboard interaction by user ''
fn (mut app App) on_key_down(key gg.KeyCode) {
	match key {
		.space {
			if app.start.x == -1 || app.end.x == -1 {
				println('Error: either start or end node is missing')
			} else {
				for row := 0; row < nrows; row++ {
					for j := 0; j < nrows; j++ {
						update_neighbors(mut app.grid, row, j)
					}
				}
				new_start := &Point{
					x: app.start.y
					y: app.start.x
				}
				new_end := &Point{
					x: app.end.y
					y: app.end.x
				}
				astar_path_finding(mut app, mut app.grid, new_start, new_end)
			}
		}
		.q {
			app.gg.quit()
		}
		.c {
			draw_grid(mut app, mut app.grid)
			draw_gridlines(mut app)
			mut grid := initialise_grid()
			app.grid = grid // set grid to app attribute so you can access it by just passing app variable or with method of app
			app.ui.dpi_scale = 1.0 // set scale this is use to make it responsive
			app.start = &Point{ // set start point to -1, -1
				x: -1
				y: -1
			}
			app.end = &Point{ // set end point to -1, -1
				x: -1
				y: -1
			}
		}
		else {}
	}
}

// draw grid lines
fn draw_gridlines(mut app App) {
	dx := window_width / nrows
	dy := window_height / nrows
	for i := 0; i < nrows; i++ {
		// horizontal lines
		app.gg.draw_line(0, i * dy, window_width, i * dy, gx.black)
		// vertical lines
		app.gg.draw_line(i * dx, 0, dx * i, window_height, gx.black)
	}
}

// heuristic function(point manhatten distance) that calculate approximate cost to reach from a given point to end(target)
fn hf(p1 Point, p2 Point) int {
	return math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y)
}

// get the position of mouse in terms of which cells' row and column
fn get_clicked_pos(pos Point, rows int, width int) (int, int) {
	x := pos.x
	y := pos.y
	row := y / rows
	col := x / rows
	return row, col
}

// initialize grid attribute of app
fn initialise_grid() [][]Cell {
	mut grid := [][]Cell{len: nrows, init: []Cell{len: nrows}}
	gap := window_width / nrows
	for i := 0; i < nrows; i++ {
		for j := 0; j < nrows; j++ {
			grid[i][j] = &Cell{
				row:   i
				col:   j
				width: gap
				pos:   &Point{
					x: j * gap
					y: i * gap
				}
				color: gx.white
				flag:  0
			}
		}
	}
	return grid
}

// draw the cells of grid
fn draw_grid(mut app App, mut grid [][]Cell) {
	for i := 0; i < nrows; i++ {
		for j := 0; j < nrows; j++ {
			pos := app.grid[i][j].pos
			width := app.grid[i][j].width
			color := app.grid[i][j].color
			app.gg.draw_rect_filled(pos.x, pos.y, width, width, color)
		}
	}
}

// update the neighbor of each cell in which cell you can visit (if it is not barrier or end or start)
fn update_neighbors(mut grid [][]Cell, row int, col int) {
	if row < nrows - 1 && grid[row + 1][col].flag != 3 {
		grid[row][col].neighbors << &Point{
			x: row + 1
			y: col
		}
	}
	if row > 0 && grid[row - 1][col].flag != 3 {
		grid[row][col].neighbors << &Point{
			x: row - 1
			y: col
		}
	}
	if col < nrows - 1 && grid[row][col + 1].flag != 3 {
		grid[row][col].neighbors << &Point{
			x: row
			y: col + 1
		}
	}
	if col > 0 && grid[row][col - 1].flag != 3 {
		grid[row][col].neighbors << &Point{
			x: row
			y: col - 1
		}
	}
}

// construct the path after finding it shows as pink color
fn reconstruct_path(mut grid [][]Cell, mut came_from [][]Point, start Point, end Point) {
	mut x := end.x
	mut y := end.y
	for !(x == -1 && y == -1) {
		set_cell_type(mut grid, x, y, 'path')
		x = came_from[x][y].x
		y = came_from[x][y].y
	}
}

// a* path finding algorithm
fn astar_path_finding(mut app App, mut grid [][]Cell, start Point, end Point) {
	mut priority_queue := &MinHeap{}
	mut g_score := [][]int{len: nrows, init: []int{len: nrows}}
	mut f_score := [][]int{len: nrows, init: []int{len: nrows}}
	mut came_from := [][]Point{len: nrows, init: []Point{len: nrows}}
	for i := 0; i < nrows; i++ {
		for j := 0; j < nrows; j++ {
			g_score[i][j] = 1_000_000_000_00
			f_score[i][j] = 1_000_000_000_00
			came_from[i][j] = &Point{
				x: -1
				y: -1
			}
		}
	}

	g_score[start.x][start.y] = 0
	f_score[start.x][start.y] = g_score[start.x][start.y] + hf(start, end)
	priority_queue.insert(Node{
		f_score: f_score[start.x][start.y]
		cell:    &Point{
			x: start.x
			y: start.y
		}
		count:   0
	})

	for priority_queue.len() > 0 {
		curr_node := priority_queue.pop() or {
			panic('There is nothing in queue how did it reach here idk')
		}
		curr_pos := curr_node.cell
		set_cell_type(mut grid, curr_pos.x, curr_pos.y, 'close')

		if curr_pos.x == end.x && curr_pos.y == end.y {
			set_cell_type(mut grid, start.x, start.y, 'start')
			set_cell_type(mut grid, end.x, end.y, 'end')
			came_from[end.x][end.y] = came_from[curr_pos.x][curr_pos.y]
			reconstruct_path(mut grid, mut came_from, start, end)
			set_cell_type(mut grid, start.x, start.y, 'start')
			set_cell_type(mut grid, end.x, end.y, 'end')
			return
		}

		for neighbor in grid[curr_pos.x][curr_pos.y].neighbors {
			mut temp_g_score := g_score[curr_pos.x][curr_pos.y] + 1
			if temp_g_score < g_score[neighbor.x][neighbor.y] {
				g_score[neighbor.x][neighbor.y] = temp_g_score
				if !(neighbor.x == start.x && neighbor.y == start.y) {
					priority_queue.insert(Node{
						f_score: g_score[neighbor.x][neighbor.y] + hf(neighbor, end)
						cell:    neighbor
						count:   curr_node.count + 1
					})
					came_from[neighbor.x][neighbor.y] = curr_pos
					set_cell_type(mut grid, neighbor.x, neighbor.y, 'open')
				}
			}
		}
	}
	set_cell_type(mut grid, start.x, start.y, 'start')
}

// change the property of a cell
fn set_cell_type(mut grid [][]Cell, row int, col int, typ string) {
	match typ {
		'reset' {
			grid[row][col].color = gx.white
			grid[row][col].flag = 0
		}
		'close' {
			grid[row][col].color = gx.red
			grid[row][col].flag = 1
		}
		'open' {
			grid[row][col].color = gx.green
			grid[row][col].flag = 2
		}
		'barrier' {
			grid[row][col].color = gx.black
			grid[row][col].flag = 3
		}
		'start' {
			grid[row][col].color = gx.orange
			grid[row][col].flag = 4
		}
		'end' {
			grid[row][col].color = gx.blue
			grid[row][col].flag = 5
		}
		'path' {
			grid[row][col].color = gx.pink
			grid[row][col].flag = 6
		}
		else {}
	}
}

// ------------------------------ HEAP -----------------------------

fn (mut heap MinHeap) insert(item Node) {
	heap.data << item
}

// get the minimum out of all node
fn (mut heap MinHeap) pop() !Node {
	if heap.len() == 0 {
		return error('empty heap')
	}
	mut i := 0
	mut curr := heap.data[0].f_score
	len := heap.len()
	for idx := 0; idx < len; idx++ {
		if curr > heap.data[idx].f_score {
			i = idx
			curr = heap.data[idx].f_score
		}
	}
	ele := heap.data[i]
	heap.data.delete(i)
	return ele
}

// see the top element of heap //TODO this won't give correct result as heap is not implemented correctly
fn (mut heap MinHeap) peek() !Node {
	if heap.data.len == 0 {
		return error('Heap is empty')
	}
	return heap.data[0]
}

// give length of heap total element present currently
fn (mut heap MinHeap) len() int {
	return heap.data.len
}

// Index of left child of a node in heap //TODO heap not implemented
fn (mut heap MinHeap) left_child(idx int) !int {
	child := 2 * idx + 1
	if child >= heap.data.len {
		return error('Out of Bound')
	}
	return child
}

// Index of right child of a node in heap //TODO heap not implemented
fn (mut heap MinHeap) right_child(idx int) !int {
	child := 2 * idx + 2
	if child >= heap.data.len {
		return error('Out of bound')
	}
	return child
}

// Index of parent of a node in heap //TODO heap not implemented
fn (mut heap MinHeap) parent(idx int) int {
	return (idx - 1) / 2
}

// comaparator of heap //TODO not used
fn comparator(n1 Node, n2 Node) bool {
	if n1.f_score == n2.f_score {
		return n1.count > n2.count
	}
	return n1.f_score > n2.f_score
}
