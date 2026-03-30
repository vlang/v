// AI heuristic inspired by the expectimax 2048 solver approach described at:
// https://github.com/nneonneo/2048-ai
import gg
import math
import math.easing
import os.asset
import rand
import time

const zooming_percent_per_frame = 5
const movement_percent_per_frame = 10

const default_window_width = 544
const default_window_height = 560

const possible_moves = [Direction.up, .right, .down, .left]
const ai_row_states = 1 << 16
const ai_tt_size = 1 << 18
const ai_time_budget_us = i64(5_000)
const ai_min_search_depth = 2
const ai_max_search_depth = 8
const ai_abort_score = -1.0e30
const ai_terminal_loss = -1.0e15
const ai_spawn_two_prob = 0.9
const ai_spawn_four_prob = 0.1
const ai_snake_path_row = [0, 1, 2, 3, 7, 6, 5, 4, 8, 9, 10, 11, 15, 14, 13, 12]!
const ai_snake_path_col = [0, 4, 8, 12, 13, 9, 5, 1, 2, 6, 10, 14, 15, 11, 7, 3]!
const ai_eval_weights = [
	[90.0, 70.0, 50.0, 30.0]!,
	[8.0, 6.0, 4.0, 2.0]!,
	[-2.0, -4.0, -6.0, -8.0]!,
	[-30.0, -50.0, -70.0, -90.0]!,
]

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	touch       TouchInfo
	ui          Ui
	theme       &Theme = themes[0]
	theme_idx   int
	board       Board
	undo        []Undo
	atickers    [4][4]f64
	mtickers    [4][4]f64
	state       GameState  = .play
	tile_format TileFormat = .normal
	moves       int
	updates     u64

	is_ai_mode bool
	ai_fpm     u64 = 8
	ai_engine  AiEngine
}

struct Ui {
mut:
	dpi_scale     f32
	tile_size     int
	border_size   int
	padding_size  int
	header_size   int
	font_size     int
	window_width  int
	window_height int
	x_padding     int
	y_padding     int
}

struct Theme {
	bg_color        gg.Color
	padding_color   gg.Color
	text_color      gg.Color
	game_over_color gg.Color
	victory_color   gg.Color
	tile_colors     []gg.Color
}

const themes = [
	&Theme{
		bg_color:        gg.rgb(250, 248, 239)
		padding_color:   gg.rgb(143, 130, 119)
		victory_color:   gg.rgb(100, 160, 100)
		game_over_color: gg.rgb(190, 50, 50)
		text_color:      gg.black
		tile_colors:     [
			gg.rgb(205, 193, 180), // Empty / 0 tile
			gg.rgb(238, 228, 218), // 2
			gg.rgb(237, 224, 200), // 4
			gg.rgb(242, 177, 121), // 8
			gg.rgb(245, 149, 99), // 16
			gg.rgb(246, 124, 95), // 32
			gg.rgb(246, 94, 59), // 64
			gg.rgb(237, 207, 114), // 128
			gg.rgb(237, 204, 97), // 256
			gg.rgb(237, 200, 80), // 512
			gg.rgb(237, 197, 63), // 1024
			gg.rgb(237, 194, 46),
		]
	},
	&Theme{
		bg_color:        gg.rgb(55, 55, 55)
		padding_color:   gg.rgb(68, 60, 59)
		victory_color:   gg.rgb(100, 160, 100)
		game_over_color: gg.rgb(190, 50, 50)
		text_color:      gg.white
		tile_colors:     [
			gg.rgb(123, 115, 108),
			gg.rgb(142, 136, 130),
			gg.rgb(142, 134, 120),
			gg.rgb(145, 106, 72),
			gg.rgb(147, 89, 59),
			gg.rgb(147, 74, 57),
			gg.rgb(147, 56, 35),
			gg.rgb(142, 124, 68),
			gg.rgb(142, 122, 58),
			gg.rgb(142, 120, 48),
			gg.rgb(142, 118, 37),
			gg.rgb(142, 116, 27),
		]
	},
	&Theme{
		bg_color:        gg.rgb(38, 38, 66)
		padding_color:   gg.rgb(58, 50, 74)
		victory_color:   gg.rgb(100, 160, 100)
		game_over_color: gg.rgb(190, 50, 50)
		text_color:      gg.white
		tile_colors:     [
			gg.rgb(92, 86, 140),
			gg.rgb(106, 99, 169),
			gg.rgb(106, 97, 156),
			gg.rgb(108, 79, 93),
			gg.rgb(110, 66, 76),
			gg.rgb(110, 55, 74),
			gg.rgb(110, 42, 45),
			gg.rgb(106, 93, 88),
			gg.rgb(106, 91, 75),
			gg.rgb(106, 90, 62),
			gg.rgb(106, 88, 48),
			gg.rgb(106, 87, 35),
		]
	},
]

struct Pos {
	x int = -1
	y int = -1
}

struct Board {
mut:
	field  [4][4]int
	oidxs  [4][4]u32 // old indexes of the fields, when != 0;  each index is an encoding of its y,x coordinates = y << 16 | x
	points int
	shifts int
}

struct Undo {
	board Board
	state GameState
}

struct TileLine {
mut:
	field  [5]int
	oidxs  [5]u32
	points int
	shifts int
}

struct TouchInfo {
mut:
	start Touch
	end   Touch
}

struct Touch {
mut:
	pos  Pos
	time time.Time
}

enum TileFormat {
	normal
	log
	exponent
	shifts
	none
	end // To know when to wrap around
}

enum GameState {
	play
	over
	victory
	freeplay
}

enum LabelKind {
	keys
	points
	moves
	tile
	victory
	game_over
	score_end
}

enum Direction {
	up
	down
	left
	right
}

type AiBoard = u64

struct AiEngine {
mut:
	initialized   bool
	row_left      []u16
	row_right     []u16
	row_heuristic []f64
	tt            []AiTtEntry
	generation    u32
}

struct AiTtEntry {
	board      AiBoard
	score      f64
	generation u32
	depth      u8
	kind       u8
}

struct AiSearchCtx {
	watch       time.StopWatch
	deadline_us i64
	generation  u32
mut:
	nodes      u64
	cache_hits u64
	aborted    bool
}

struct AiMoveResult {
mut:
	move       Direction
	score      f64
	depth      int
	nodes      u64
	cache_hits u64
	valid      bool
}

// Utility functions
@[inline]
fn avg(a int, b int) int {
	return (a + b) / 2
}

fn (b Board) transpose() Board {
	mut res := b
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			res.field[y][x] = b.field[x][y]
			res.oidxs[y][x] = b.oidxs[x][y]
		}
	}
	return res
}

fn (b Board) hmirror() Board {
	mut res := b
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			res.field[y][x] = b.field[y][3 - x]
			res.oidxs[y][x] = b.oidxs[y][3 - x]
		}
	}
	return res
}

fn (t TileLine) to_left() TileLine {
	right_border_idx := 4
	mut res := t
	mut zeros := 0
	mut nonzeros := 0
	// gather meta info about the line:
	for x in res.field {
		if x == 0 {
			zeros++
		} else {
			nonzeros++
		}
	}
	if nonzeros == 0 {
		// when all the tiles are empty, there is nothing left to do
		return res
	}
	if zeros > 0 {
		// we have some 0s, do shifts to compact them:
		mut remaining_zeros := zeros
		for x := 0; x < right_border_idx - 1; x++ {
			for res.field[x] == 0 && remaining_zeros > 0 {
				res.shifts++
				for k := x; k < right_border_idx; k++ {
					res.field[k] = res.field[k + 1]
					res.oidxs[k] = res.oidxs[k + 1]
				}
				remaining_zeros--
			}
		}
	}
	// At this point, the non 0 tiles are all on the left, with no empty spaces
	// between them. we can safely merge them, when they have the same value:
	for x := 0; x < right_border_idx - 1; x++ {
		if res.field[x] == 0 {
			break
		}
		if res.field[x] == res.field[x + 1] {
			for k := x; k < right_border_idx; k++ {
				res.field[k] = res.field[k + 1]
				res.oidxs[k] = res.oidxs[k + 1]
			}
			res.shifts++
			res.field[x]++
			res.points += 1 << res.field[x]
		}
	}
	return res
}

fn (b Board) to_left() Board {
	mut res := b
	for y in 0 .. 4 {
		mut hline := TileLine{}
		for x in 0 .. 4 {
			hline.field[x] = b.field[y][x]
			hline.oidxs[x] = b.oidxs[y][x]
		}
		reshline := hline.to_left()
		res.shifts += reshline.shifts
		res.points += reshline.points
		for x in 0 .. 4 {
			res.field[y][x] = reshline.field[x]
			res.oidxs[y][x] = reshline.oidxs[x]
		}
	}
	return res
}

fn yx2i(y int, x int) u32 {
	return u32(y) << 16 | u32(x)
}

@[inline]
fn reverse_row(row u16) u16 {
	part0 := u16((row & 0x000f) << 12)
	part1 := u16((row & 0x00f0) << 4)
	part2 := u16((row & 0x0f00) >> 4)
	part3 := u16((row & 0xf000) >> 12)
	return part0 | part1 | part2 | part3
}

fn build_ai_row_left(row u16) (u16, bool) {
	mut tiles := [4]u8{}
	mut compact := [4]u8{}
	mut compact_len := 0
	for idx in 0 .. 4 {
		tiles[idx] = u8((row >> (idx << 2)) & 0xf)
		if tiles[idx] != 0 {
			compact[compact_len] = tiles[idx]
			compact_len++
		}
	}
	mut merged := [4]u8{}
	mut read_idx := 0
	mut write_idx := 0
	for read_idx < compact_len {
		value := compact[read_idx]
		if read_idx + 1 < compact_len && compact[read_idx + 1] == value {
			merged[write_idx] = value + 1
			read_idx += 2
		} else {
			merged[write_idx] = value
			read_idx++
		}
		write_idx++
	}
	mut res := u16(0)
	mut changed := false
	for idx in 0 .. 4 {
		res |= u16(merged[idx]) << (idx << 2)
		if merged[idx] != tiles[idx] {
			changed = true
		}
	}
	return res, changed
}

fn ai_row_heuristic(row u16) f64 {
	mut tiles := [4]int{}
	mut empty_tiles := 0
	mut mergeable_pairs := 0
	mut smoothness := 0.0
	mut monotonicity := 0.0
	for idx in 0 .. 4 {
		tiles[idx] = int((row >> (idx << 2)) & 0xf)
		if tiles[idx] == 0 {
			empty_tiles++
		}
	}
	for idx in 0 .. 3 {
		curr := tiles[idx]
		next := tiles[idx + 1]
		if curr != 0 && next != 0 {
			smoothness -= math.abs(curr - next)
			if curr == next {
				mergeable_pairs++
			}
		}
	}
	mut left_penalty := 0.0
	mut right_penalty := 0.0
	for idx in 0 .. 3 {
		left_penalty += math.max(0, tiles[idx + 1] - tiles[idx])
		right_penalty += math.max(0, tiles[idx] - tiles[idx + 1])
	}
	monotonicity = -math.min(left_penalty, right_penalty)
	return f64(empty_tiles) * 240.0 + f64(mergeable_pairs) * 500.0 + smoothness * 20.0 +
		monotonicity * 70.0
}

fn (mut ai AiEngine) ensure_ready() {
	if ai.initialized {
		return
	}
	ai.row_left = []u16{len: ai_row_states}
	ai.row_right = []u16{len: ai_row_states}
	ai.row_heuristic = []f64{len: ai_row_states}
	ai.tt = []AiTtEntry{len: ai_tt_size}
	for i in 0 .. ai_row_states {
		row := u16(i)
		left, _ := build_ai_row_left(row)
		reversed := reverse_row(row)
		right_reversed, _ := build_ai_row_left(reversed)
		ai.row_left[i] = left
		ai.row_right[i] = reverse_row(right_reversed)
		ai.row_heuristic[i] = ai_row_heuristic(row)
	}
	ai.initialized = true
}

@[inline]
fn ai_row(board AiBoard, row_idx int) u16 {
	return u16((u64(board) >> (row_idx * 16)) & 0xffff)
}

@[inline]
fn ai_tile(board AiBoard, idx int) u8 {
	return u8((u64(board) >> (idx * 4)) & 0xf)
}

fn ai_transpose(board AiBoard) AiBoard {
	mut res := AiBoard(0)
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			src_idx := y << 2 + x
			dst_idx := x << 2 + y
			value := AiBoard(ai_tile(board, src_idx))
			res |= value << (dst_idx * 4)
		}
	}
	return res
}

@[inline]
fn ai_pack_exponent(exponent int) AiBoard {
	// AiBoard stores 4-bit exponents, so clamp larger freeplay tiles to avoid corrupting
	// adjacent cells in the packed representation.
	return AiBoard(u64(math.min(exponent, 15)))
}

fn board_to_ai(board Board) AiBoard {
	mut res := AiBoard(0)
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			res |= ai_pack_exponent(board.field[y][x]) << ((y << 2 + x) << 2)
		}
	}
	return res
}

@[inline]
fn ai_empty_count(board AiBoard) int {
	mut empty_tiles := 0
	for idx in 0 .. 16 {
		if ai_tile(board, idx) == 0 {
			empty_tiles++
		}
	}
	return empty_tiles
}

@[inline]
fn ai_has_moves(board AiBoard) bool {
	if ai_empty_count(board) > 0 {
		return true
	}
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			idx := y << 2 + x
			value := ai_tile(board, idx)
			if x < 3 && value == ai_tile(board, idx + 1) {
				return true
			}
			if y < 3 && value == ai_tile(board, idx + 4) {
				return true
			}
		}
	}
	return false
}

fn (ai &AiEngine) move_left(board AiBoard) (AiBoard, bool) {
	mut res := AiBoard(0)
	mut changed := false
	for row_idx in 0 .. 4 {
		row := ai_row(board, row_idx)
		next := ai.row_left[int(row)]
		res |= AiBoard(next) << (row_idx << 4)
		changed = changed || row != next
	}
	return res, changed
}

fn (ai &AiEngine) move_right(board AiBoard) (AiBoard, bool) {
	mut res := AiBoard(0)
	mut changed := false
	for row_idx in 0 .. 4 {
		row := ai_row(board, row_idx)
		next := ai.row_right[int(row)]
		res |= AiBoard(next) << (row_idx << 4)
		changed = changed || row != next
	}
	return res, changed
}

fn (ai &AiEngine) move_up(board AiBoard) (AiBoard, bool) {
	transposed := ai_transpose(board)
	moved, changed := ai.move_left(transposed)
	return ai_transpose(moved), changed
}

fn (ai &AiEngine) move_down(board AiBoard) (AiBoard, bool) {
	transposed := ai_transpose(board)
	moved, changed := ai.move_right(transposed)
	return ai_transpose(moved), changed
}

fn (ai &AiEngine) move_board(board AiBoard, move Direction) (AiBoard, bool) {
	return match move {
		.left { ai.move_left(board) }
		.right { ai.move_right(board) }
		.up { ai.move_up(board) }
		.down { ai.move_down(board) }
	}
}

@[direct_array_access]
fn (ai &AiEngine) evaluate(board AiBoard) f64 {
	mut score := 0.0
	transposed := ai_transpose(board)
	for row_idx in 0 .. 4 {
		score += ai.row_heuristic[int(ai_row(board, row_idx))]
		score += ai.row_heuristic[int(ai_row(transposed, row_idx))]
	}
	mut max_tile := u8(0)
	mut max_in_corner := false
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			value := ai_tile(board, y << 2 + x)
			score += f64(value) * ai_eval_weights[y][x]
			if value > max_tile {
				max_tile = value
				max_in_corner = (x == 0 && y == 0)
			}
		}
	}
	if max_in_corner {
		score += 1500.0 + f64(max_tile) * 180.0
	} else {
		score -= 300.0 + f64(max_tile) * 80.0
	}
	score += f64(ai_empty_count(board)) * 500.0
	score += ai_snake_score(board, ai_snake_path_row)
	score += ai_snake_score(board, ai_snake_path_col)
	return score
}

fn ai_snake_score(board AiBoard, path [16]int) f64 {
	mut score := 0.0
	mut weight := 1.0
	for idx in path {
		value := f64(ai_tile(board, idx))
		score += value * value * weight
		weight *= 0.5
	}
	return score * 220.0
}

@[inline]
fn (mut ctx AiSearchCtx) should_abort() bool {
	if ctx.aborted {
		return true
	}
	if ctx.nodes & 1023 == 0 && ctx.watch.elapsed().microseconds() >= ctx.deadline_us {
		ctx.aborted = true
		return true
	}
	return false
}

@[inline]
fn ai_tt_index(board AiBoard, depth int, kind u8) int {
	mut h := u64(board)
	h ^= u64(depth) * 0x9e3779b97f4a7c15
	h ^= u64(kind) * 0xbf58476d1ce4e5b9
	h ^= h >> 30
	h *= 0xbf58476d1ce4e5b9
	h ^= h >> 27
	h *= 0x94d049bb133111eb
	h ^= h >> 31
	return int(h & u64(ai_tt_size - 1))
}

fn (mut ai AiEngine) tt_lookup(board AiBoard, depth int, kind u8, mut ctx AiSearchCtx) ?f64 {
	start_idx := ai_tt_index(board, depth, kind)
	for probe in 0 .. 4 {
		idx := (start_idx + probe) & (ai_tt_size - 1)
		entry := ai.tt[idx]
		if entry.generation != ctx.generation {
			continue
		}
		if entry.board == board && entry.kind == kind && int(entry.depth) >= depth {
			ctx.cache_hits++
			return entry.score
		}
	}
	return none
}

fn (mut ai AiEngine) tt_store(board AiBoard, depth int, kind u8, score f64, ctx AiSearchCtx) {
	start_idx := ai_tt_index(board, depth, kind)
	mut best_idx := start_idx
	mut found_slot := false
	for probe in 0 .. 4 {
		idx := (start_idx + probe) & (ai_tt_size - 1)
		entry := ai.tt[idx]
		if entry.generation != ctx.generation || entry.board == board || int(entry.depth) <= depth {
			best_idx = idx
			found_slot = true
			break
		}
	}
	if !found_slot {
		best_idx = start_idx
	}
	ai.tt[best_idx] = AiTtEntry{
		board:      board
		score:      score
		generation: ctx.generation
		depth:      u8(depth)
		kind:       kind
	}
}

fn (mut ai AiEngine) expectimax_max(board AiBoard, depth int, mut ctx AiSearchCtx) f64 {
	ctx.nodes++
	if ctx.should_abort() {
		return ai_abort_score
	}
	if cached := ai.tt_lookup(board, depth, 0, mut ctx) {
		return cached
	}
	if depth == 0 {
		score := ai.evaluate(board)
		ai.tt_store(board, depth, 0, score, ctx)
		return score
	}
	mut best_score := ai_terminal_loss
	mut has_move := false
	for move in possible_moves {
		next_board, is_valid := ai.move_board(board, move)
		if !is_valid {
			continue
		}
		has_move = true
		score := ai.expectimax_chance(next_board, depth - 1, mut ctx)
		if ctx.aborted {
			return ai_abort_score
		}
		if score > best_score {
			best_score = score
		}
	}
	if !has_move {
		return ai_terminal_loss
	}
	ai.tt_store(board, depth, 0, best_score, ctx)
	return best_score
}

fn (mut ai AiEngine) expectimax_chance(board AiBoard, depth int, mut ctx AiSearchCtx) f64 {
	ctx.nodes++
	if ctx.should_abort() {
		return ai_abort_score
	}
	if cached := ai.tt_lookup(board, depth, 1, mut ctx) {
		return cached
	}
	empty_tiles := ai_empty_count(board)
	if empty_tiles == 0 {
		score := ai.expectimax_max(board, depth, mut ctx)
		if !ctx.aborted {
			ai.tt_store(board, depth, 1, score, ctx)
		}
		return score
	}
	cell_weight := 1.0 / f64(empty_tiles)
	mut total_score := 0.0
	for idx in 0 .. 16 {
		if ai_tile(board, idx) != 0 {
			continue
		}
		shift := idx << 2
		two_board := board | (AiBoard(1) << shift)
		two_score := ai.expectimax_max(two_board, depth, mut ctx)
		if ctx.aborted {
			return ai_abort_score
		}
		four_board := board | (AiBoard(2) << shift)
		four_score := ai.expectimax_max(four_board, depth, mut ctx)
		if ctx.aborted {
			return ai_abort_score
		}
		total_score += cell_weight * (ai_spawn_two_prob * two_score +
			ai_spawn_four_prob * four_score)
	}
	ai.tt_store(board, depth, 1, total_score, ctx)
	return total_score
}

fn (mut ai AiEngine) first_valid_move(board AiBoard) ?Direction {
	for move in possible_moves {
		_, is_valid := ai.move_board(board, move)
		if is_valid {
			return move
		}
	}
	return none
}

fn (mut ai AiEngine) best_move(board AiBoard) AiMoveResult {
	ai.ensure_ready()
	ai.generation++
	mut ctx := AiSearchCtx{
		watch:       time.new_stopwatch()
		deadline_us: ai_time_budget_us
		generation:  ai.generation
	}
	mut best := AiMoveResult{}
	if fallback := ai.first_valid_move(board) {
		best = AiMoveResult{
			move:  fallback
			score: ai_terminal_loss
			valid: true
		}
	} else {
		return best
	}
	empty_tiles := ai_empty_count(board)
	mut depth_limit := ai_max_search_depth
	if empty_tiles >= 6 {
		depth_limit = 6
	}
	for depth := ai_min_search_depth; depth <= depth_limit; depth++ {
		mut iter_best := AiMoveResult{
			score: ai_terminal_loss
		}
		mut iter_valid := false
		for move in possible_moves {
			next_board, is_valid := ai.move_board(board, move)
			if !is_valid {
				continue
			}
			score := ai.expectimax_chance(next_board, depth - 1, mut ctx)
			if ctx.aborted {
				break
			}
			if !iter_valid || score > iter_best.score {
				iter_best = AiMoveResult{
					move:  move
					score: score
					depth: depth
					valid: true
				}
				iter_valid = true
			}
		}
		if ctx.aborted {
			break
		}
		if iter_valid {
			best = iter_best
			best.nodes = ctx.nodes
			best.cache_hits = ctx.cache_hits
		}
	}
	best.nodes = ctx.nodes
	best.cache_hits = ctx.cache_hits
	return best
}

@[inline]
fn (b Board) has_moves() bool {
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			value := b.field[y][x]
			if value == 0 {
				return true
			}
			if (x < 3 && value == b.field[y][x + 1]) || (y < 3 && value == b.field[y + 1][x]) {
				return true
			}
		}
	}
	return false
}

fn (mut b Board) move(d Direction) (Board, bool) {
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			b.oidxs[y][x] = yx2i(y, x)
		}
	}
	new := match d {
		.left { b.to_left() }
		.right { b.hmirror().to_left().hmirror() }
		.up { b.transpose().to_left().transpose() }
		.down { b.transpose().hmirror().to_left().hmirror().transpose() }
	}
	// If the board hasn't changed, it's an illegal move, don't allow it.
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			if b.field[y][x] != new.field[y][x] {
				return new, true
			}
		}
	}
	return new, false
}

fn (mut b Board) is_game_over() bool {
	return !b.has_moves()
}

fn (mut app App) update_tickers() {
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			app.atickers[y][x] = math.clip(app.atickers[y][x] - f64(zooming_percent_per_frame) / 100.0,
				0.0, 1.0)
			app.mtickers[y][x] = math.clip(app.mtickers[y][x] - f64(movement_percent_per_frame) / 100.0,
				0.0, 1.0)
		}
	}
}

fn (mut app App) new_game() {
	app.board = Board{}
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			app.board.field[y][x] = 0
			app.atickers[y][x] = 0
			app.mtickers[y][x] = 0
		}
	}
	app.state = .play
	app.undo = []Undo{cap: 4096}
	app.moves = 0
	app.new_random_tile()
	app.new_random_tile()
}

@[inline]
fn (mut app App) check_for_victory() {
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			fidx := app.board.field[y][x]
			if fidx == 11 {
				app.state = .victory
				return
			}
		}
	}
}

@[inline]
fn (mut app App) check_for_game_over() {
	if app.board.is_game_over() {
		app.state = .over
	}
}

fn (mut b Board) place_random_tile() (Pos, int) {
	mut etiles := [16]Pos{}
	mut empty_tiles_max := 0
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			fidx := b.field[y][x]
			if fidx == 0 {
				etiles[empty_tiles_max] = Pos{x, y}
				empty_tiles_max++
			}
		}
	}
	if empty_tiles_max > 0 {
		new_random_tile_index := rand.intn(empty_tiles_max) or { 0 }
		empty_pos := etiles[new_random_tile_index]
		// 10% chance of getting a `4` tile
		value := rand.f64n(1.0) or { 0.0 }
		random_value := if value < 0.9 { 1 } else { 2 }
		b.field[empty_pos.y][empty_pos.x] = random_value
		b.oidxs[empty_pos.y][empty_pos.x] = yx2i(empty_pos.y, empty_pos.x)
		return empty_pos, random_value
	}
	return Pos{}, 0
}

fn (mut app App) new_random_tile() {
	// do not animate empty fields:
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			fidx := app.board.field[y][x]
			if fidx == 0 {
				app.atickers[y][x] = 0
				app.board.oidxs[y][x] = 0xFFFF_FFFF
			}
		}
	}
	empty_pos, random_value := app.board.place_random_tile()
	if random_value > 0 {
		app.atickers[empty_pos.y][empty_pos.x] = 1.0
	}
	if app.state != .freeplay {
		app.check_for_victory()
	}
	app.check_for_game_over()
}

fn (mut app App) apply_new_board(new Board) {
	old := app.board
	app.moves++
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			if old.oidxs[y][x] != new.oidxs[y][x] {
				app.mtickers[y][x] = 1.0
			}
		}
	}
	app.board = new
	app.undo << Undo{old, app.state}
	app.new_random_tile()
}

fn (mut app App) move(d Direction) {
	new, is_valid := app.board.move(d)
	if !is_valid {
		return
	}
	app.apply_new_board(new)
}

fn (mut app App) ai_move() {
	think_watch := time.new_stopwatch()
	search_result := app.ai_engine.best_move(board_to_ai(app.board))
	if !search_result.valid {
		return
	}
	elapsed_us := think_watch.elapsed().microseconds()
	cache_rate := if search_result.nodes > 0 {
		100.0 * f64(search_result.cache_hits) / f64(search_result.nodes)
	} else {
		0.0
	}
	eprintln('AI ${elapsed_us:5}µs | depth ${search_result.depth:2} | nodes ${search_result.nodes:7} | cache ${cache_rate:5.1f}% | move ${search_result.move:5} | score ${search_result.score:9.2f}')
	app.move(search_result.move)
}

fn (app &App) label_format(kind LabelKind) gg.TextCfg {
	match kind {
		.keys {
			return gg.TextCfg{
				color:          gg.Color{150, 150, 255, 200}
				align:          .center
				vertical_align: .bottom
				size:           app.ui.font_size / 4
			}
		}
		.points {
			return gg.TextCfg{
				color: if app.state in [.over, .victory] { gg.white } else { app.theme.text_color }
				align: .left
				size:  app.ui.font_size / 2
			}
		}
		.moves {
			return gg.TextCfg{
				color: if app.state in [.over, .victory] { gg.white } else { app.theme.text_color }
				align: .right
				size:  app.ui.font_size / 2
			}
		}
		.tile {
			return gg.TextCfg{
				color:          app.theme.text_color
				align:          .center
				vertical_align: .middle
				size:           app.ui.font_size
			}
		}
		.victory {
			return gg.TextCfg{
				color:          app.theme.victory_color
				align:          .center
				vertical_align: .middle
				size:           app.ui.font_size * 2
			}
		}
		.game_over {
			return gg.TextCfg{
				color:          app.theme.game_over_color
				align:          .center
				vertical_align: .middle
				size:           app.ui.font_size * 2
			}
		}
		.score_end {
			return gg.TextCfg{
				color:          gg.white
				align:          .center
				vertical_align: .middle
				size:           app.ui.font_size * 3 / 4
			}
		}
	}
}

@[inline]
fn (mut app App) set_theme(idx int) {
	theme := themes[idx]
	app.theme_idx = idx
	app.theme = theme
	app.gg.set_bg_color(theme.bg_color)
}

fn (mut app App) resize() {
	mut s := app.gg.scale
	if s == 0.0 {
		s = 1.0
	}
	window_size := app.gg.window_size()
	w := window_size.width
	h := window_size.height
	m := f32(math.min(w, h))
	app.ui.dpi_scale = s
	app.ui.window_width = w
	app.ui.window_height = h
	app.ui.padding_size = int(m / 38)
	app.ui.header_size = app.ui.padding_size
	app.ui.border_size = app.ui.padding_size * 2
	app.ui.tile_size = int((m - app.ui.padding_size * 5 - app.ui.border_size * 2) / 4)
	app.ui.font_size = int(m / 10)
	// If the window's height is greater than its width, center the board vertically.
	// If not, center it horizontally
	if w > h {
		app.ui.y_padding = 0
		app.ui.x_padding = (app.ui.window_width - app.ui.window_height) / 2
	} else {
		app.ui.y_padding = (app.ui.window_height - app.ui.window_width - app.ui.header_size) / 2
		app.ui.x_padding = 0
	}
}

fn (app &App) draw() {
	xpad, ypad := app.ui.x_padding, app.ui.y_padding
	ww := app.ui.window_width
	wh := app.ui.window_height
	m := math.min(ww, wh)
	labelx := xpad + app.ui.border_size
	labely := ypad + app.ui.border_size / 2
	app.draw_tiles()
	// TODO: Make transparency work in `gg`
	if app.state == .over {
		app.gg.draw_rect_filled(0, 0, ww, wh, gg.rgba(10, 0, 0, 180))
		app.gg.draw_text(ww / 2, (m * 4 / 10) + ypad, 'Game Over', app.label_format(.game_over))
		f := app.label_format(.tile)
		msg := $if android { 'Tap to restart' } $else { 'Press `r` to restart' }
		app.gg.draw_text(ww / 2, (m * 6 / 10) + ypad, msg, gg.TextCfg{
			...f
			color: gg.white
			size:  f.size * 3 / 4
		})
	}
	if app.state == .victory {
		app.gg.draw_rect_filled(0, 0, ww, wh, gg.rgba(0, 10, 0, 180))
		app.gg.draw_text(ww / 2, (m * 4 / 10) + ypad, 'Victory!', app.label_format(.victory))
		// f := app.label_format(.tile)
		msg1 := $if android { 'Tap to continue' } $else { 'Press `space` to continue' }
		msg2 := $if android { 'Tap to restart' } $else { 'Press `r` to restart' }
		app.gg.draw_text(ww / 2, (m * 6 / 10) + ypad, msg1, app.label_format(.score_end))
		app.gg.draw_text(ww / 2, (m * 8 / 10) + ypad, msg2, app.label_format(.score_end))
	}
	// Draw at the end, so that it's on top of the victory / game over overlays
	app.gg.draw_text(labelx, labely, 'Points: ${app.board.points}', app.label_format(.points))
	app.gg.draw_text(ww - labelx, labely, 'Moves: ${app.moves}', app.label_format(.moves))
	app.gg.draw_text(ww / 2, wh, 'Controls: WASD,V,<=,T,Enter,ESC', app.label_format(.keys))
}

fn (app &App) draw_tiles() {
	xstart := app.ui.x_padding + app.ui.border_size
	ystart := app.ui.y_padding + app.ui.border_size + app.ui.header_size
	toffset := app.ui.tile_size + app.ui.padding_size
	tiles_size := math.min(app.ui.window_width, app.ui.window_height) - app.ui.border_size * 2
	// Draw the padding around the tiles
	app.gg.draw_rounded_rect_filled(xstart, ystart, tiles_size, tiles_size, tiles_size / 24,
		app.theme.padding_color)

	// Draw empty tiles:
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			tw := app.ui.tile_size
			th := tw // square tiles, w == h
			xoffset := xstart + app.ui.padding_size + x * toffset
			yoffset := ystart + app.ui.padding_size + y * toffset
			app.gg.draw_rounded_rect_filled(xoffset, yoffset, tw, th, tw / 8,
				app.theme.tile_colors[0])
		}
	}

	// Draw the already placed and potentially moving tiles:
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			tidx := app.board.field[y][x]
			oidx := app.board.oidxs[y][x]
			if tidx == 0 || oidx == 0xFFFF_FFFF {
				continue
			}
			app.draw_one_tile(x, y, tidx)
		}
	}

	// Draw the newly placed random tiles on top of everything else:
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			tidx := app.board.field[y][x]
			oidx := app.board.oidxs[y][x]
			if oidx == 0xFFFF_FFFF && tidx != 0 {
				app.draw_one_tile(x, y, tidx)
			}
		}
	}
}

fn (app &App) draw_one_tile(x int, y int, tidx int) {
	xstart := app.ui.x_padding + app.ui.border_size
	ystart := app.ui.y_padding + app.ui.border_size + app.ui.header_size
	toffset := app.ui.tile_size + app.ui.padding_size
	oidx := app.board.oidxs[y][x]
	oy := oidx >> 16
	ox := oidx & 0xFFFF
	mut dx := 0
	mut dy := 0
	if oidx != 0xFFFF_FFFF {
		scaling := app.ui.tile_size * easing.in_out_quint(app.mtickers[y][x])
		if ox != x {
			dx = math.clip(int(scaling * (f64(ox) - f64(x))), -4 * app.ui.tile_size,
				4 * app.ui.tile_size)
		}
		if oy != y {
			dy = math.clip(int(scaling * (f64(oy) - f64(y))), -4 * app.ui.tile_size,
				4 * app.ui.tile_size)
		}
	}
	tile_color := if tidx < app.theme.tile_colors.len {
		app.theme.tile_colors[tidx]
	} else {
		// If there isn't a specific color for this tile, reuse the last color available
		app.theme.tile_colors.last()
	}
	anim_size := 1.0 - app.atickers[y][x]
	tw := int(f64(anim_size * app.ui.tile_size))
	th := tw // square tiles, w == h
	xoffset := dx + xstart + app.ui.padding_size + x * toffset + (app.ui.tile_size - tw) / 2
	yoffset := dy + ystart + app.ui.padding_size + y * toffset + (app.ui.tile_size - th) / 2
	app.gg.draw_rounded_rect_filled(xoffset, yoffset, tw, th, tw / 8, tile_color)
	if tidx != 0 { // 0 == blank spot
		xpos := xoffset + tw / 2
		ypos := yoffset + th / 2
		mut fmt := app.label_format(.tile)
		fmt = gg.TextCfg{
			...fmt
			size: int(anim_size * (fmt.size - 1))
		}
		match app.tile_format {
			.normal {
				app.gg.draw_text(xpos, ypos, '${1 << tidx}', fmt)
			}
			.log {
				app.gg.draw_text(xpos, ypos, '${tidx}', fmt)
			}
			.exponent {
				app.gg.draw_text(xpos, ypos, '2', fmt)
				fs2 := int(f32(fmt.size) * 0.67)
				app.gg.draw_text(xpos + app.ui.tile_size / 10, ypos - app.ui.tile_size / 8,
					'${tidx}', gg.TextCfg{
					...fmt
					size:  fs2
					align: gg.HorizontalAlign.left
				})
			}
			.shifts {
				fs2 := int(f32(fmt.size) * 0.6)
				app.gg.draw_text(xpos, ypos, '2<<${tidx - 1}', gg.TextCfg{
					...fmt
					size: fs2
				})
			}
			.none {} // Don't draw any text here, colors only
			.end {} // Should never get here
		}
		// oidx_fmt := gg.TextCfg{...fmt,size: 14}
		// app.gg.draw_text(xoffset + 50, yoffset + 15, 'y:${oidx >> 16}|x:${oidx & 0xFFFF}|m:${app.mtickers[y][x]:5.3f}',	oidx_fmt)
		// app.gg.draw_text(xoffset + 52, yoffset + 30, 'ox:${ox}|oy:${oy}', oidx_fmt)
		// app.gg.draw_text(xoffset + 52, yoffset + 85, 'dx:${dx}|dy:${dy}', oidx_fmt)
	}
}

fn (mut app App) handle_touches() {
	s, e := app.touch.start, app.touch.end
	adx, ady := math.abs(e.pos.x - s.pos.x), math.abs(e.pos.y - s.pos.y)
	if math.max(adx, ady) < 10 {
		app.handle_tap()
	} else {
		app.handle_swipe()
	}
}

fn (mut app App) handle_tap() {
	_, ypad := app.ui.x_padding, app.ui.y_padding
	w, h := app.ui.window_width, app.ui.window_height
	m := math.min(w, h)
	s, e := app.touch.start, app.touch.end
	avgx, avgy := avg(s.pos.x, e.pos.x), avg(s.pos.y, e.pos.y)
	// TODO: Replace "touch spots" with actual buttons
	// bottom left -> change theme
	if avgx < 50 && h - avgy < 50 {
		app.next_theme()
	}
	// bottom right -> change tile format
	if w - avgx < 50 && h - avgy < 50 {
		app.next_tile_format()
	}
	if app.state == .victory {
		if avgy > (m / 2) + ypad {
			if avgy < (m * 7 / 10) + ypad {
				app.state = .freeplay
			} else if avgy < (m * 9 / 10) + ypad {
				app.new_game()
			} else {
				// TODO: remove and implement an actual way to toggle themes on mobile
			}
		}
	} else if app.state == .over {
		if avgy > (m / 2) + ypad && avgy < (m * 7 / 10) + ypad {
			app.new_game()
		}
	}
}

fn (mut app App) handle_swipe() {
	// Currently, swipes are only used to move the tiles.
	// If the user's not playing, exit early to avoid all the unnecessary calculations
	if app.state !in [.play, .freeplay] {
		return
	}
	s, e := app.touch.start, app.touch.end
	w, h := app.ui.window_width, app.ui.window_height
	dx, dy := e.pos.x - s.pos.x, e.pos.y - s.pos.y
	adx, ady := math.abs(dx), math.abs(dy)
	dmin := if math.min(adx, ady) > 0 { math.min(adx, ady) } else { 1 }
	dmax := if math.max(adx, ady) > 0 { math.max(adx, ady) } else { 1 }
	tdiff := (e.time - s.time).milliseconds()
	// TODO: make this calculation more accurate (don't use arbitrary numbers)
	distance_factor := f64(math.min(w, h)) * f64(tdiff) / 100.0
	min_swipe_distance := int(math.sqrt(distance_factor)) + 20
	if dmax < min_swipe_distance {
		return
	}
	// Swipe was too short
	if dmax / dmin < 2 {
		return
	}
	// Swiped diagonally
	if adx > ady {
		if dx < 0 {
			app.move(.left)
		} else {
			app.move(.right)
		}
	} else {
		if dy < 0 {
			app.move(.up)
		} else {
			app.move(.down)
		}
	}
}

@[inline]
fn (mut app App) next_theme() {
	app.set_theme(if app.theme_idx == themes.len - 1 { 0 } else { app.theme_idx + 1 })
}

@[inline]
fn (mut app App) next_tile_format() {
	app.tile_format = unsafe { TileFormat(int(app.tile_format) + 1) }
	if app.tile_format == .end {
		app.tile_format = .normal
	}
}

@[inline]
fn (mut app App) undo() {
	if app.undo.len > 0 {
		undo := app.undo.pop()
		app.board = undo.board
		app.state = undo.state
		app.moves--
	}
}

fn (mut app App) on_key_down(key gg.KeyCode) {
	// these keys are independent from the game state:
	match key {
		.v { app.is_ai_mode = !app.is_ai_mode }
		.page_up { app.ai_fpm = dump(math.min(app.ai_fpm + 1, 60)) }
		.page_down { app.ai_fpm = dump(math.max(app.ai_fpm - 1, 1)) }
		//
		.escape { app.gg.quit() }
		.n, .r { app.new_game() }
		.backspace { app.undo() }
		.enter { app.next_tile_format() }
		.j { app.state = .over }
		.t { app.next_theme() }
		else {}
	}
	if app.state in [.play, .freeplay] {
		if !app.is_ai_mode {
			match key {
				.w, .up { app.move(.up) }
				.a, .left { app.move(.left) }
				.s, .down { app.move(.down) }
				.d, .right { app.move(.right) }
				else {}
			}
		}
	}
	if app.state == .victory {
		if key == .space {
			app.state = .freeplay
		}
	}
}

fn on_event(e &gg.Event, mut app App) {
	match e.typ {
		.key_down {
			app.on_key_down(e.key_code)
		}
		.resized, .restored, .resumed {
			app.resize()
		}
		.touches_began {
			if e.num_touches > 0 {
				t := e.touches[0]
				app.touch.start = Touch{
					pos:  Pos{
						x: int(t.pos_x / app.ui.dpi_scale)
						y: int(t.pos_y / app.ui.dpi_scale)
					}
					time: time.now()
				}
			}
		}
		.touches_ended {
			if e.num_touches > 0 {
				t := e.touches[0]
				app.touch.end = Touch{
					pos:  Pos{
						x: int(t.pos_x / app.ui.dpi_scale)
						y: int(t.pos_y / app.ui.dpi_scale)
					}
					time: time.now()
				}
				app.handle_touches()
			}
		}
		.mouse_down {
			app.touch.start = Touch{
				pos:  Pos{
					x: int(e.mouse_x / app.ui.dpi_scale)
					y: int(e.mouse_y / app.ui.dpi_scale)
				}
				time: time.now()
			}
		}
		.mouse_up {
			app.touch.end = Touch{
				pos:  Pos{
					x: int(e.mouse_x / app.ui.dpi_scale)
					y: int(e.mouse_y / app.ui.dpi_scale)
				}
				time: time.now()
			}
			app.handle_touches()
		}
		else {}
	}
}

fn frame(mut app App) {
	mut do_update := false
	if app.gg.timer.elapsed().milliseconds() > 15 {
		app.gg.timer.restart()
		do_update = true
		app.updates++
	}
	app.gg.begin()
	if do_update {
		app.update_tickers()
	}
	app.draw()
	app.gg.end()
	if do_update && app.is_ai_mode && app.state in [.play, .freeplay]
		&& app.updates % app.ai_fpm == 0 {
		app.ai_move()
	}
	if app.updates % 120 == 0 {
		// do GC once per 2 seconds
		// eprintln('> gc_memory_use: ${gc_memory_use()}')
		if gc_is_enabled() {
			// Avoid assert error when built with `-cg` on some systems
			gc_disable()
		}
		gc_enable()
		gc_collect()
		gc_disable()
	}
}

fn init(mut app App) {
	app.resize()
}

fn main() {
	mut app := &App{}
	app.new_game()
	app.gg = gg.new_context(
		bg_color:     app.theme.bg_color
		width:        default_window_width
		height:       default_window_height
		sample_count: 2 // higher quality curves
		window_title: 'V 2048'
		frame_fn:     frame
		event_fn:     on_event
		init_fn:      init
		user_data:    app
		font_path:    asset.get_path('../assets', 'fonts/RobotoMono-Regular.ttf')
	)
	app.gg.run()
}
