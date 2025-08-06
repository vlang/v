import arrays

pub fn Canvas.new(width int, height int) &Canvas {
	return &Canvas{
		layers: create_buffer(width, height)
	}
}

pub fn (canvas Canvas) size() (int, int) {
	return canvas.layers[0].len, canvas.layers.len
}

pub fn (canvas Canvas) string() string {
	output := buffer_to_braille(canvas.layers, dots_to_braille_rune_map)
	return arrays.join_to_string(output, '\n', |row| arrays.join_to_string(row, '', |character| '${character}'))
}

pub fn (mut canvas Canvas) set(x int, y int) {
	canvas.layers[y][x] = true
}

pub fn (mut canvas Canvas) unset(x int, y int) {
	canvas.layers[y][x] = false
}

pub fn (mut canvas Canvas) clear() {
	width, height := canvas.size()
	canvas.layers = create_buffer(width, height)
}

struct Canvas {
mut:
	layers [][]bool
}

const dots_to_braille = [u8(0), 3, 1, 4, 2, 5, 6, 7]
const dots_to_braille_rune_map = memoize_dots_to_braille_rune()

fn dots_to_braille_rune(dots u8) rune {
	mut mask := 0
	for index in 0 .. 8 {
		if ((dots >> index) & 1) == 1 {
			mask |= 1 << dots_to_braille[index]
		}
	}

	return rune(0x2800 + mask)
}

fn memoize_dots_to_braille_rune() [256]rune {
	mut memo := [256]rune{}

	for index in 0 .. 256 {
		memo[index] = dots_to_braille_rune(index)
	}

	return memo
}

fn buffer_to_braille(buffer [][]bool, braille_mapping [256]rune) [][]rune {
	out_rows := buffer.len / 4
	out_cols := buffer[0].len / 2
	mut output := [][]rune{len: out_rows, init: []rune{len: out_cols}}

	for row := 0; row < out_rows; row += 1 {
		row_offset := row * 4
		for column := 0; column < out_cols; column += 1 {
			col_offset := column * 2

			mut dots := u8(buffer[row_offset + 0][col_offset + 0])
			dots |= u8(buffer[row_offset + 0][col_offset + 1]) << 1
			dots |= u8(buffer[row_offset + 1][col_offset + 0]) << 2
			dots |= u8(buffer[row_offset + 1][col_offset + 1]) << 3
			dots |= u8(buffer[row_offset + 2][col_offset + 0]) << 4
			dots |= u8(buffer[row_offset + 2][col_offset + 1]) << 5
			dots |= u8(buffer[row_offset + 3][col_offset + 0]) << 6
			dots |= u8(buffer[row_offset + 3][col_offset + 1]) << 7

			output[row][column] = dots_to_braille_rune(dots)
		}
	}

	return output
}

fn create_buffer(width int, height int) [][]bool {
	return [][]bool{len: height, init: []bool{len: width}}
}

fn test_main() {
	braille_mapping := memoize_dots_to_braille_rune()
	width, height := 40, 48
	mut canvas := Canvas.new(width, height)
	mut buffer := [][]bool{len: height, init: []bool{len: width}}

	minr := 16 * 16
	maxr := 20 * 20
	for y := 0; y < 48; y += 1 {
		for x := 0; x < width; x += 1 {
			cy := y - 24
			big_cx := x - 20
			small_cx := x - 16
			big := (big_cx * big_cx) + (cy * cy)
			small := (small_cx * small_cx) + (cy * cy)
			if small > minr && big < maxr {
				buffer[y][x] = true
				canvas.set(x, y)
			}
		}
	}

	_ := buffer_to_braille(buffer, braille_mapping)
}
