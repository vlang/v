module toml

struct JsonToken {
	value	string
	typ 	string [json:'type']
}