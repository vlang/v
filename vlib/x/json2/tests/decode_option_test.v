import x.json2

struct F1 {
	f ?struct {
		a int
	}
}

fn test_main() {
	j1 := json2.decode[F1]('{"f":{"a":1}}')!
	assert '${j1}' == 'F1{
    f: Option(struct {
        a: 1
    })
}'

	j2 := json2.decode[F1]('{}')!
	assert '${j2}' == 'F1{
    f: Option(none)
}'
}
