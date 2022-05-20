import json
import rand
import net.http

struct Weather {
	status      string [skip] // drop this field
	api_version string [skip]
	api_status  string [skip]
	lang        string [skip]
	unit        string [skip]
	tzshift     int    [skip]
	timezone    string [skip]
	server_time u32    [skip]
	location    []f32  [skip]
	result      Result //[json: result] if the field name is different in JSON, it can be specified
}

struct Result {
	realtime          Realtime [skip]
	minutely          Minutely [skip]
	hourly            Hourly   [skip]
	daily             Daily    [skip]
	primary           int      [skip]
	forecast_keypoint string
}

struct Realtime {}

struct Minutely {}

struct Hourly {}

struct Daily {}

fn main() {
	config := http.FetchConfig{
		user_agent: 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:88.0) Gecko/20100101 Firefox/88.0'
	}

	rnd := rand.f32()
	url := 'https://api.caiyunapp.com/v2.5/96Ly7wgKGq6FhllM/116.391912,40.010711/weather.jsonp?hourlysteps=120&random=$rnd'
	// println(url)

	resp := http.fetch(http.FetchConfig{ ...config, url: url }) or {
		println('failed to fetch data from the server')
		return
	}

	weather := json.decode(Weather, resp.body) or {
		println('failed to decode weather json')
		return
	}

	println('未来两小时天气:\n${weather.result.forecast_keypoint}.')
}
