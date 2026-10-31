import v.embed_file

@[params]
struct EmbedConfig[T] {
	source string
	model  T
}

struct EmbedModel {
	n int
}

struct EmbedSink {
mut:
	values []string
}

fn (mut sink EmbedSink) push(s string) ! {
	sink.values << s.trim_space()
}

fn (mut sink EmbedSink) push_file(f embed_file.EmbedFileData) ! {
	sink.values << f.to_string().trim_space()
}

fn (mut sink EmbedSink) push_config[T](c EmbedConfig[T]) ! {
	sink.values << c.source.trim_space()
}

fn (mut sink EmbedSink) push_all() ! {
	sink.push($embed_file('a.txt').to_string())!
}

fn always_fails() ! {
	return error('fail')
}

fn test_embed_file_in_call_args_of_unused_or_expr() {
	mut sink := EmbedSink{}
	sink.push($embed_file('a.txt').to_string()) or { panic(err) }
	sink.push_file($embed_file('a.txt')) or { panic(err) }
	sink.push_config[EmbedModel](
		source: $embed_file('a.txt').to_string()
		model:  EmbedModel{}
	) or { panic(err) }
	sink.push_all() or { panic(err) }
	assert sink.values == ['test', 'test', 'test', 'test']
}

fn test_embed_file_in_statements_of_or_block() {
	mut sink := EmbedSink{}
	always_fails() or { sink.push($embed_file('a.txt').to_string()) or { panic(err) } }
	always_fails() or {
		always_fails() or { sink.push($embed_file('a.txt').to_string()) or { panic(err) } }
	}
	always_fails() or {
		sink.values << $embed_file('a.txt').to_string().trim_space()
		sink.values << 'after'
	}
	assert sink.values == ['test', 'test', 'test', 'after']
}
