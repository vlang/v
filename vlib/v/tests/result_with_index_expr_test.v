import x.json2

struct SomeStruct {
	title string
}

fn test_result_with_index() {
	resp := '{
  		"userId": 1,
  		"id": 1,
  		"title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  		"body": "quia et suscipitsuscipit recusandae consequuntur expedita et cumreprehenderit molestiae ut ut quas totamnostrum rerum est autem sunt rem eveniet architecto"
}'
	raw_data := json2.raw_decode(resp)!

	data := raw_data as map[string]json2.Any

	mut ss := map[int]SomeStruct{}
	s := SomeStruct{
		title: data['title']!.str()
	}
	ss[data['id']!.int()] = s
	t := ss[data['id']!.int()]
	assert t.title == 'sunt aut facere repellat provident occaecati excepturi optio reprehenderit'
}
