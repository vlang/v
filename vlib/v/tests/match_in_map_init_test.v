fn test_match_in_map_init() {
	ret := foo()
	println(ret)
	assert ret == map{'token': 'a', 'sleep': '30', 'every': '1'}
}

fn foo() map[string]string {
	mut cfg := map[string][]string{}
	cfg['token'] = ['a', 'b']
	cfg['sleep'] = ['30', '60']
	cfg['every'] = ['1', '5']

	return map{
		'token': cfg['token'][0]
		'sleep': match cfg['sleep'][0].len {
			0 { '60' }
			else { cfg['sleep'][0] }
		}
		'every': match cfg['every'][0].len {
			0 { '5' }
			else { cfg['every'][0] }
		}
	}
}
