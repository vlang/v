import net.http
import os
import rand
import x.json2
import x.json2.decoder2 as json

struct Weather {
	lang   string
	result Result
}

struct Result {
	forecast_keypoint string
}

const config = http.FetchConfig{
	user_agent: 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:88.0) Gecko/20100101 Firefox/88.0'
}

fn main() {
	dest_lang := if os.args.len > 1 { os.args[1] } else { 'en' }
	rnd := rand.f32()
	url := 'https://api.caiyunapp.com/v2.5/96Ly7wgKGq6FhllM/116.391912,40.010711/weather.jsonp?hourlysteps=120&random=${rnd}'

	resp := http.fetch(http.FetchConfig{ ...config, url: url }) or {
		println('failed to fetch data from the server')
		return
	}

	weather := json.decode[Weather](resp.body) or {
		println('failed to decode weather json')
		return
	}

	for ch in ['未来两小时天气', weather.result.forecast_keypoint] {
		println('${weather.lang:8}: ${ch}')
		t := translate(ch, weather.lang, dest_lang) or {
			println('failed to translate ${ch}: ${err}')
			continue
		}
		println('${dest_lang:8}: ${t}')
	}
}

// translate fetch google to print translate text `q` in languate `sl` into language `tl`.
// A translation typical response json is like: `[[["Weather in the next two hours",
// "未来两小时天气",null,null,3,null,null,[[null,"offline"]],
// [[["61549914d65604307a34fd1855292577","offline_launch_doc.md"],null,null,null,null,
// [[[6,8,0]]]]]]],null,"zh-CN",null,null,null,null,[]]`
// where translated text is located at position `json_resp[0][0][0]`.
fn translate(q string, sl string, tl string) !string {
	url := 'https://translate.googleapis.com/translate_a/single?client=gtx&sl=${sl}&tl=${tl}&dt=t&q=${q}'

	resp := http.fetch(http.FetchConfig{ ...config, url: url })!

	json_resp := json.decode[json2.Any](resp.body)!

	a := json_resp.arr()
	if a.len > 0 {
		a0 := a[0].arr()
		if a0.len > 0 {
			a00 := a0[0].arr()
			if a00.len > 0 {
				return a00[0].str()
			}
		}
	}
	return error('invalid translation response')
}
