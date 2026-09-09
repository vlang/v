import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_array_filter_uses_specialized_generic_call_return_type() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_array_filter_specialized_generic_return_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src_path := '${v3_bin}_program.v'
	os.write_file(src_path, '
import arrays { flat_map, flat_map_indexed, map_indexed }

fn ranges(size int, start int) [][]int {
	return [[]int{len: start, init: start - index - 1}, []int{len: size - start - 1, init: start + 1 + index}]
}

fn grid_cross_walk(size int, start_x int, start_y int) [][]int {
	return flat_map[int, []int]([start_x, start_y], fn [size] (elem int) [][]int {
		return ranges(size, elem)
	})
}

fn main() {
	data := [
		[3, 0, 3, 7, 3],
		[2, 5, 5, 1, 2],
		[6, 5, 3, 3, 2],
		[3, 3, 5, 4, 9],
		[3, 5, 3, 9, 0],
	]
	visible := flat_map_indexed[[]int, bool](data, fn [data] (y int, row []int) []bool {
		return map_indexed(row, fn [data, y] (x int, height int) bool {
			return map_indexed(grid_cross_walk(data.len, x, y), fn [data, x, y, height] (i int, walk []int) bool {
				return walk.map(fn [data, x, y, i] (pos int) int {
					return if i < 2 { data[y][pos] } else { data[pos][x] }
				}).all(it < height)
			}).any(it)
		}).filter(it)
	})
	println(visible.len)
}
') or {
		panic(err)
	}
	bin_path := '${v3_bin}.bin'
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${bin_path}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin_path)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '21'
}
