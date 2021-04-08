module http //internal tests have access to *everything in the module*

import json

struct HttpbinResponseBody {
	args    map[string]string
	data    string
	files   map[string]string
	form    map[string]string
	headers map[string]string
	json    map[string]string
	origin  string
	url     string
}


fn http_fetch_mock(_methods []string, _config FetchConfig) ?[]Response {
	url := 'https://httpbin.org/'
	methods := if _methods.len == 0 { ['GET', 'POST', 'PATCH', 'PUT', 'DELETE'] } else { _methods }
	mut config := _config
	mut result := []Response{}
	// Note: httpbin doesn't support head
	for method in methods {
		lmethod := method.to_lower()
		config.method = method_from_str(method)
		res := fetch(url + lmethod, config)?
		// TODO
		// body := json.decode(HttpbinResponseBody,res.text)?
		result << res
	}
	return result
}

fn test_http_fetch_bare() {
	$if !network ? { return }
	responses := http_fetch_mock([], FetchConfig{}) or {
		panic(err)
	}
	for response in responses {
		assert response.status_code == 200
	}
}

fn test_http_fetch_with_data() {
	$if !network ? { return }
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
	$if !network ? { return }
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

fn test_http_fetch_with_headers() ? {
	$if !network ? { return }
	mut header := new_header()
	header.add_custom('Test-Header', 'hello world') ?
	responses := http_fetch_mock([], {
		header: header
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
