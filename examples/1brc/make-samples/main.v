import encoding.csv
import flag
import os
import rand

struct CityMean {
	city string
	mean f64
}

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	fp.version('1brc sample generator v1.0.0')
	fp.skip_executable()
	fp.application('Sample generator for 1 billion rows challenge')
	fp.description('The 1 billion rows challenge solved in V.\nFor details, see https://www.morling.dev/blog/one-billion-row-challenge/')
	input_file := fp.string('city-file', `i`, 'cities.txt', 'Path to input file with cities and means list')
	fp.limit_free_args_to_exactly(1)!
	sample_count := fp.remaining_parameters()[0].u64()

	content := os.read_file(input_file) or { panic(err) }
	mut reader := csv.new_reader(content, csv.ReaderConfig{ delimiter: `,` })
	mut means := []CityMean{}
	for {
		rec := reader.read() or { break }
		means << CityMean{
			city: rec[0]
			mean: rec[1].f64()
		}
	}

	for _ in 0 .. sample_count / 2 {
		mut city := rand.intn(means.len)!
		m1, m2 := rand.normal_pair(mu: means[city].mean, sigma: 10)!
		println('${means[city].city};${m1:.1f}')
		city = rand.intn(means.len)!
		println('${means[city].city};${m2:.1f}')
	}
}
