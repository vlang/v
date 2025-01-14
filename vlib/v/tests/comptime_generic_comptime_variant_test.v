pub type DesiredCapabilities = FireFox | Edge

struct FireFox {
	browser_name          string = 'firefox'
	accept_insecure_certs bool   = true
	moz_debugger_address  bool   = true
}

struct Edge {
	browser_name string = 'MicrosoftEdge'
}

fn struct_values[T](s T) map[string]string {
	mut res := map[string]string{}
	$if T is $struct {
		$for field in T.fields {
			res[field.name] = s.$(field.name).str()
		}
	}
	return res
}

fn useit(dc DesiredCapabilities) string {
	$for v in dc.variants {
		if dc is v {
			$if v is $struct {
				result := struct_values(dc)
				return result.str()
			}
		}
	}
	return ''
}

fn test_main() {
	assert useit(Edge{}) == "{'browser_name': 'MicrosoftEdge'}"
	assert useit(FireFox{}) == "{'browser_name': 'firefox', 'accept_insecure_certs': 'true', 'moz_debugger_address': 'true'}"
}
