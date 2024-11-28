import json
import net.http

struct IpInfo {
	ip          string
	ip_decimal  int
	country     string
	country_iso string
	country_eu  bool
	region_name string
	region_code string
	zip_code    string
	city        string
	latitude    f64
	longitude   f64
	time_zone   string
	asn         string
	asn_org     string
	user_agent  UserAgent
}

struct UserAgent {
	product   string
	version   string
	comment   string
	raw_value string
}

url := 'http://ifconfig.co/json'
resp := http.fetch(
	url:        url
	user_agent: 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/117.0'
) or {
	eprintln('failed to fetch data from the server, error: ${err}')
	exit(1)
}
ip_info := json.decode(IpInfo, resp.body) or {
	println('failed to decode the json data returned by the server, error: ${err}')
	exit(2)
}
println(ip_info)
