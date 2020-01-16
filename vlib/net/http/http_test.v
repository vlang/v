import net.http
import json

struct HttpbinResponseBody {
	args    map[string]string
	data    string
	files   map[string]string
	form    map[string]string
	headers map[string]string
	json    ?map[string]string
	origin  string
	url     string
}

fn http_fetch_mock(_methods []string, _config http.FetchConfig) ?[]http.Response {
	url := 'https://httpbin.org/'
	methods := if _methods.len == 0 { ['GET', 'POST', 'PATCH', 'PUT', 'DELETE'] } else { _methods }
	mut config := _config
	mut result := []http.Response
	// Note: httpbin doesn't support head
	for method in methods {
		lmethod := method.to_lower()
		config.method = method
		res := http.fetch(url + lmethod, config) or {
			return error(err)
		}
		// TODO
		// body := json.decode(HttpbinResponseBody,res.text) or {
		// return error(err)
		// }
		result << res
	}
	return result
}

fn test_http_fetch_bare() {
	responses := http_fetch_mock([], http.FetchConfig{}) or {
		panic(err)
	}
	for response in responses {
		assert response.status_code == 200
	}
}

fn test_http_fetch_with_data() {
	responses := http_fetch_mock(['POST', 'PUT', 'PATCH', 'DELETE'], {
		data: 'hello world'
	}) or {
		panic(err)
	}
	for response in responses {
		payload := json.decode(HttpbinResponseBody,response.text) or {
			panic(err)
		}
		assert payload.data == 'hello world'
	}
}

fn test_http_fetch_with_params() {
	responses := http_fetch_mock([], {
		params: {
			'a': 'b',
			'c': 'd'
		}
	}) or {
		panic(err)
	}
	for response in responses {
		// payload := json.decode(HttpbinResponseBody,response.text) or {
		// panic(err)
		// }
		assert response.status_code == 200
		// TODO
		// assert payload.args['a'] == 'b'
		// assert payload.args['c'] == 'd'
	}
}

fn test_http_fetch_with_headers() {
	responses := http_fetch_mock([], {
		headers: {
			'Test-Header': 'hello world'
		}
	}) or {
		panic(err)
	}
	for response in responses {
		// payload := json.decode(HttpbinResponseBody,response.text) or {
		// panic(err)
		// }
		assert response.status_code == 200
		// TODO
		// assert payload.headers['Test-Header'] == 'hello world'
	}
}
