module c

import v3.flat
import v3.types

fn (mut g FlatGen) gen_fns() {
	mut cur_module := ''
	for i in 0 .. g.a.nodes.len {
		node := g.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_module = ''
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_module = cur_module
			continue
		}

		if kind_id == 61 {
			if !g.should_emit_fn_node_in_module(node, i, cur_module) {
				continue
			}
			qfn := qualified_fn_name_in_module(cur_module, node.value)
			if g.emitted_fn_contains(qfn) {
				continue
			}
			g.emitted_fns[qfn] = true
			g.tc.cur_module = cur_module
			g.gen_fn_in_module(node, cur_module)
		}
	}
}

fn (mut g FlatGen) should_emit_fn_node(node flat.Node, node_index int) bool {
	return g.should_emit_fn_node_in_module(node, node_index, g.tc.cur_module)
}

fn (mut g FlatGen) should_emit_fn_node_in_module(node flat.Node, node_index int, module_name string) bool {
	_ = node_index
	cfn := c_name(node.value)
	if cfn in ['Array_string__join', 'array_string_join'] {
		return false
	}
	if g.has_used_fn_filter() && is_generated_fn_after_markused(node.value) {
		return !g.has_generic_params(node)
	}
	if module_name == 'main' {
		if g.has_used_fn_filter() && !g.used_fn_contains(node.value) && !g.used_fn_contains(dfn)
			&& !g.used_fn_contains(cfn) && !g.used_fn_contains(qfn) {
			return false
		}
		return !g.has_generic_params(node)
	}
	dfn := dotted_fn_name_in_module(module_name, node.value)
	qfn := qualified_fn_name_in_module(module_name, node.value)
	if qfn.starts_with('http____anon_fn_') || qfn.starts_with('mbedtls____anon_fn_') {
		return false
	}
	if qfn in [
		'arrays__uniq',
		'base32__Encoding__encode_',
		'binary__big_endian_get_u32',
		'binary__little_endian_get_u32',
		'bcrypt__Hashed__hash_u8',
		'bcrypt__bcrypt',
		'big__Integer__dec',
		'big__Integer__mod_pow',
		'big__Integer__montgomery',
		'big__Integer__pow',
		'big__integer_from_regular_string',
		'big__toom3_multiply_digit_array',
		'blowfish__Blowfish__encrypt',
		'blowfish__expand_key',
		'blowfish__expand_key_with_salt',
		'blowfish__new_cipher',
		'blowfish__new_salted_cipher',
		'blowfish__setup_tables',
		'chunked__unhex',
		'deflate__flush_stream_chunks',
		'deflate__inflate_block_stream',
		'deflate__inflate_with_callback',
		'fasthttp__Server__run',
		'fasthttp__Server__stop_accepting',
		'fasthttp__accept_clients',
		'fasthttp__add_event',
		'fasthttp__close_all_conns',
		'fasthttp__close_conn',
		'fasthttp__complete_response',
		'fasthttp__delete_event',
		'fasthttp__ev_set',
		'fasthttp__handle_read',
		'fasthttp__handle_write',
		'fasthttp__has_chunked_transfer_encoding_in_buf',
		'fasthttp__new_server',
		'fasthttp__parse_content_length_from_buf',
		'fasthttp__process_request',
		'fasthttp__chunked_hex_digit_value',
		'fasthttp__send_file_bytes',
		'fasthttp__send_pending',
		'git__get_git_executable_path',
		'highlight__init_d',
		'highlight__init_v',
		'html__unescape_all',
		'http__H2Conn__fill_at_least',
		'http__H2Conn__read_response',
		'http__H2Conn__write_all',
		'http__H2ServerConn__fill_at_least',
		'http__H2ServerConn__write_all',
		'http__Header__keys',
		'http__Request__receive_all_data_from_cb_in_builder',
		'http__Request__h2_exchange',
		'http__H2HpackDecoder__decode',
		'http__H2HpackDecoder__lookup',
		'http__H2HpackDecoder__read_literal',
		'http__H2HpackEncoder__encode',
		'http__H2HpackEncoder__encode_field',
		'http__build_h2_huffman_table',
		'http__chunked_hex_value',
		'http__delete',
		'http__fetch',
		'http__get',
		'http__h2_encode_string',
		'http__h2_hpack_find_static',
		'http__h2_hpack_write_int',
		'http__h2_huffman_decode',
		'http__h2_huffman_encode',
		'http__h2_is_sensitive',
		'http__header_key_eq',
		'http__header_lower_ascii_byte',
		'http__head',
		'http__new_handler_worker',
		'http__new_tls_handler_worker',
		'http__patch',
		'http__parse_chunked_size_line',
		'http__parse_request',
		'http__post',
		'http__post_form',
		'http__post_form_with_cookies',
		'http__post_json',
		'http__post_multipart_form',
		'http__prepare',
		'http__read_proxy_connect_response',
		'json2__ReaderScanner__num_scan',
		'json2__ReaderScanner__text_scan',
		'json2__Any__json_str',
		'json2__Any__arr',
		'json2__Any__as_array',
		'json2__Array_Any__str',
		'json2__Decoder__check_array',
		'json2__Decoder__check_boolean',
		'json2__Decoder__check_json_format',
		'json2__Decoder__check_null',
		'json2__Decoder__check_number',
		'json2__Decoder__check_object',
		'json2__Decoder__check_string',
		'json2__Decoder__checker_error',
		'json2__Decoder__increment',
		'json2__Decoder__skip_whitespace',
		'json2__Decoder__sumtype_type_field_matches',
		'json2__LinkedList__last',
		'json2__LinkedList__push',
		'json2__map_stringAny__str',
		'json2__scientific_number_to_integer_string',
		'json2__Scanner__num_scan',
		'json2__Scanner__text_scan',
		'io__BufferedReader__fill_buffer',
		'io__BufferedWriter__shift_unwritten_to_front',
		'io__BufferedWriter__write',
		'io__ReaderWriterImpl__read',
		'io__cp',
		'log__free_logger',
		'log__level_from_tag',
		'log__tag_to_console',
		'log__tag_to_file',
		'markdown__BlockParser__parse_list_item',
		'markdown__Markdown__new',
		'markdown__ascii_lower',
		'markdown__normalize_label',
		'mbedtls__SSLListener__init_sni',
		'mbedtls__wait_for',
		'net__Addr__family',
		'net__Addr__len',
		'net__Addr__port',
		'net__Addr__str',
		'net__Ip6__str',
		'net__Ip__str',
		'net__RawSocket__select',
		'net__TcpSocket__set_default_options',
		'net__UdpSocket__remote',
		'net__UdpSocket__select',
		'net__addr_from_socket_handle',
		'net__canonical_ipv6_from_bytes',
		'net__format_ipv6_groups',
		'net__hex_digit',
		'net__is_ipv4_mapped',
		'net__longest_zero_run',
		'net__new_ip',
		'net__new_ip6',
		'net__parse_dotted_quad',
		'net__parse_hex_group',
		'net__parse_ipv6_interface_index',
		'net__parse_ipv6_multicast_addr',
		'net__parse_ipv6_to_bytes',
		'net__peer_addr_from_socket_handle',
		'net__resolve_addrs',
		'net__resolve_addrs_fuzzy',
		'net__resolve_ipaddrs',
		'net__select_deadline',
		'net__set_addr_family',
		'net__temp_unix',
		'net__wrap_getaddrinfo_error',
		'sha256__Digest__block_size',
		'sha256__Digest__checksum',
		'sha256__Digest__clone',
		'sha256__Digest__free',
		'sha256__Digest__init',
		'sha256__Digest__reset',
		'sha256__Digest__size',
		'sha256__Digest__sum',
		'sha256__Digest__write',
		'sha256__block',
		'sha256__block_generic',
		'sha256__hexhash',
		'sha256__new',
		'sha256__new224',
		'sha256__sum',
		'sha256__sum224',
		'sha256__sum256',
		'sha1__Digest__block_size',
		'sha1__Digest__checksum',
		'sha1__Digest__clone',
		'sha1__Digest__free',
		'sha1__Digest__init',
		'sha1__Digest__reset',
		'sha1__Digest__size',
		'sha1__Digest__sum',
		'sha1__Digest__write',
		'sha1__block',
		'sha1__block_generic',
		'sha1__hexhash',
		'sha1__new',
		'sha1__sum',
		'orm__bool_to_primitive',
		'orm__f32_to_primitive',
		'orm__f64_to_primitive',
		'orm__float_literal_to_primitive',
		'orm__DB__orm_release_savepoint',
		'orm__DB__orm_rollback_to',
		'orm__DB__orm_savepoint',
		'orm__i16_to_primitive',
		'orm__i64_to_primitive',
		'orm__i8_to_primitive',
		'orm__int_literal_to_primitive',
		'orm__int_to_primitive',
		'orm__orm_table_gen',
		'orm__primitive_array_len',
		'orm__primitive_to_aggregate_value',
		'orm__primitive_type',
		'orm__primitive_value',
		'orm__Savepoint__rollback',
		'orm__string_to_primitive',
		'orm__tenant_filter_array_primitive_type',
		'orm__tenant_filter_primitive_type',
		'orm__time_to_primitive',
		'orm__Tx__commit',
		'orm__Tx__rollback',
		'orm__Tx__savepoint',
		'orm__Tx__select',
		'orm__u16_to_primitive',
		'orm__u32_to_primitive',
		'orm__u64_to_primitive',
		'orm__u8_to_primitive',
		'pcre__Compiler__emit_class',
		'pcre__Regex__vm_match',
		'pcre__compile',
		'pcre__parse_nodes',
		'pcre__set_bitmap',
		'rand__int_u64',
		'regex__RE__case_transform_char',
		'regex__RE__get_code',
		'regex__RE__parse_bsls',
		'regex__RE__parse_groups',
		'regex__is_alnum',
		'sqlite__Stmt__sqlite_select_column',
		'sqlite__ConnectionPool__close',
		'sqlite__bind_array',
		'sqlite__sqlite_type_from_v',
		'socks__handshake',
		'ssl__new_ssl_conn',
		'stdatomic__AtomicVal__add',
		'stdatomic__AtomicVal__compare_and_swap',
		'stdatomic__AtomicVal__load',
		'stdatomic__AtomicVal__store',
		'stdatomic__AtomicVal__sub',
		'stdatomic__new_atomic',
		'strconv__atof64',
		'time__DateTimeParser__parse',
		'time__Time__format_rfc3339',
		'time__Time__format_rfc3339_micro',
		'time__Time__format_rfc3339_nano',
		'time__Time__push_to_http_header',
		'string__bytestr',
		'u8__is_alnum',
		'u8__is_hex_digit',
		'urllib__ishex',
		'urllib__unhex',
		'veb__Context__serve_compressed_static',
		'veb__ascii_eq_ignore_case',
		'veb__content_length_validation_response',
		'veb__read_exact_bytes',
		'veb__send_compressed_response',
		'zstd__CCtx__compress_stream2',
		'zstd__CCtx__free_cctx',
		'zstd__CCtx__set',
		'zstd__DCtx__decompress_stream',
		'zstd__DCtx__free_dctx',
		'zstd__DCtx__set',
		'zstd__check_error',
		'zstd__compress',
		'zstd__decompress',
		'zstd__default_c_level',
		'zstd__get_error_name',
		'zstd__is_error',
		'zstd__new_cctx',
		'zstd__new_dctx',
	] {
		return false
	}
	if g.has_used_fn_filter() && !g.used_fn_contains(node.value) && !g.used_fn_contains(dfn)
		&& !g.used_fn_contains(cfn) && !g.used_fn_contains(qfn) {
		return false
	}
	if g.has_generic_params(node) {
		return false
	}
	if g.fn_has_unresolved_generics(node) {
		return false
	}
	return true
}

fn is_generated_fn_after_markused(name string) bool {
	return name.starts_with('__anon_fn_')
}

fn (g &FlatGen) used_fn_contains(name string) bool {
	if name.len == 0 {
		return false
	}
	return g.used_fns[name]
}

fn (g &FlatGen) has_used_fn_filter() bool {
	return g.used_fns.len > 0 && g.used_fn_contains('main')
}

fn (g &FlatGen) emitted_fn_contains(name string) bool {
	return name.len > 0 && g.emitted_fns[name]
}

fn (g &FlatGen) qualified_fn_name(name string) string {
	return qualified_fn_name_in_module(g.tc.cur_module, name)
}

fn qualified_fn_name_in_module(module_name string, name string) string {
	if module_name == 'builtin' && name == 'free' {
		return 'v_free'
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return c_name('${module_name}.${name}')
	}
	if name == 'free' {
		return 'v_free'
	}
	return c_name(name)
}

fn (g &FlatGen) direct_call_name(name string) string {
	if name == 'free' {
		return 'v_free'
	}
	if name == 'int_str' {
		return 'int__str'
	}
	if name == 'bool_str' {
		return 'bool__str'
	}
	return c_name(name)
}

fn (g &FlatGen) dotted_fn_name(name string) string {
	return dotted_fn_name_in_module(g.tc.cur_module, name)
}

fn dotted_fn_name_in_module(module_name string, name string) string {
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return '${module_name}.${name}'
	}
	return name
}

fn qualify_name_in_module(module_name string, name string) string {
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin' {
		return name
	}
	if name.contains('.') {
		return name
	}
	return '${module_name}.${name}'
}

fn (mut g FlatGen) gen_fn(node flat.Node) {
	g.gen_fn_in_module(node, g.tc.cur_module)
}

fn (mut g FlatGen) gen_fn_in_module(node flat.Node, module_name string) {
	g.tc.cur_module = module_name
	g.cur_fn_name = node.value
	g.tc.push_scope()
	g.defers = []flat.NodeId{}
	g.fn_defers = []flat.NodeId{}
	g.fn_defer_counts = map[int]string{}
	g.defer_capture_names = []string{}
	g.defer_capture_types = map[string]types.Type{}
	g.set_cur_fn_ret(types.Type(types.void_))
	old_param_names := g.cur_param_names.clone()
	old_param_type_values := g.cur_param_type_values.clone()
	old_param_types := g.cur_param_types.clone()
	g.cur_param_names = []string{}
	g.cur_param_type_values = []types.Type{}
	g.cur_param_types = map[string]types.Type{}
	for i in 0 .. node.children_count {
		param_id := g.a.child(&node, i)
		p := g.a.node(param_id)
		if node_kind_id(p) == 75 && p.value.len > 0 {
			param_type := g.tc.parse_type(p.typ)
			g.cur_param_names << p.value
			g.cur_param_type_values << param_type
			g.cur_param_types[p.value] = param_type
			g.tc.cur_scope.insert(p.value, param_type)
		}
	}
	g.insert_cur_implicit_veb_ctx_param(node)
	fn_defer_ids := g.collect_function_defer_ids(node)
	g.prepare_function_defers(fn_defer_ids)
	is_entry_main := node.value == 'main' && (module_name.len == 0 || module_name == 'main')
	if is_entry_main {
		g.writeln('int main(int argc, char** argv) {')
		if g.has_builtins {
			g.writeln('\tg_main_argc = argc;')
			g.writeln('\tg_main_argv = argv;')
		}
		g.gen_compiler_vexe_env_setup()
		if g.runtime_inits.len > 0 || g.module_init_fns.len > 0 || g.global_inits.len > 0 {
			g.writeln('\t_vinit();')
		}
	} else {
		ret_type := g.tc.parse_type(node.typ)
		g.set_cur_fn_ret(ret_type)
		g.write(g.optional_type_name(ret_type))
		g.write(' ')
		g.write(qualified_fn_name_in_module(module_name, node.value))
		g.write('(')
		g.write_fn_node_params(node)
		g.writeln(') {')
	}
	g.indent++
	g.gen_function_defer_prelude()

	for i in 0 .. node.children_count {
		id := g.a.child(&node, i)
		child := g.a.node(id)
		if child.kind != .param {
			g.tc.cur_module = module_name
			g.gen_node(id)
		}
	}
	g.gen_all_defers()
	if is_entry_main {
		g.writeln('return 0;')
	} else if g.cur_fn_ret_is_optional {
		ct := g.optional_type_name(g.cur_fn_ret)
		g.writeln('return (${ct}){.ok = true};')
	}
	g.indent--
	g.writeln('}')
	g.writeln('')
	g.cur_param_names = old_param_names.clone()
	g.cur_param_type_values = old_param_type_values.clone()
	g.cur_param_types = old_param_types.clone()
	g.tc.pop_scope()
}

fn (mut g FlatGen) collect_function_defer_ids(node flat.Node) []flat.NodeId {
	mut ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		g.collect_function_defer_ids_from(g.a.child(&node, i), mut ids)
	}
	return ids
}

fn (mut g FlatGen) collect_function_defer_ids_from(id flat.NodeId, mut ids []flat.NodeId) {
	if !g.valid_node_id(id) {
		return
	}
	node := g.a.nodes[int(id)]
	if node.kind == .fn_decl || node.kind == .c_fn_decl || node.kind == .fn_literal {
		return
	}
	if node.kind == .defer_stmt && node.value == 'function' {
		ids << id
		return
	}
	for i in 0 .. node.children_count {
		g.collect_function_defer_ids_from(g.a.child(&node, i), mut ids)
	}
}

fn (mut g FlatGen) prepare_function_defers(fn_defer_ids []flat.NodeId) {
	for idx, defer_id in fn_defer_ids {
		g.fn_defer_counts[int(defer_id)] = '${c_name(g.cur_fn_name)}_defer_${idx}_count'
		defer_node := g.a.nodes[int(defer_id)]
		if defer_node.children_count > 0 {
			g.collect_function_defer_captures(g.a.child(&defer_node, 0))
		}
	}
}

fn (mut g FlatGen) collect_function_defer_captures(id flat.NodeId) {
	if !g.valid_node_id(id) {
		return
	}
	node := g.a.nodes[int(id)]
	if node.kind == .fn_decl || node.kind == .c_fn_decl || node.kind == .fn_literal {
		return
	}
	if node.kind == .ident {
		g.add_function_defer_capture(id, node.value)
	}
	for i in 0 .. node.children_count {
		g.collect_function_defer_captures(g.a.child(&node, i))
	}
}

fn (mut g FlatGen) add_function_defer_capture(id flat.NodeId, name string) {
	if name.len == 0 || name == '_' || name in g.cur_param_names || name in g.modules
		|| name in g.global_modules || name in g.defer_capture_types {
		return
	}
	typ := g.usable_expr_type(id)
	if typ is types.Void || typ is types.Unknown || typ is types.FnType {
		return
	}
	ct := g.tc.c_type(typ)
	if ct.len == 0 || ct == 'void' || ct.starts_with('fn_ptr:') {
		return
	}
	g.defer_capture_names << name
	g.defer_capture_types[name] = typ
}

fn (mut g FlatGen) gen_function_defer_prelude() {
	for _, count_name in g.fn_defer_counts {
		g.writeln('int ${count_name} = 0;')
	}
	for name in g.defer_capture_names {
		typ := g.defer_capture_types[name] or { continue }
		ct := g.tc.c_type(typ)
		g.write('${ct} ${c_name(name)} = ')
		g.gen_default_value_for_type(typ)
		g.writeln(';')
		g.tc.cur_scope.insert(name, typ)
	}
}

fn (mut g FlatGen) set_cur_fn_ret(ret_type types.Type) {
	g.cur_fn_ret = ret_type
	g.cur_fn_ret_is_optional = false
	g.cur_fn_ret_base = types.Type(types.void_)
	if ret_type is types.OptionType {
		g.cur_fn_ret_is_optional = true
		g.cur_fn_ret_base = ret_type.base_type
	} else if ret_type is types.ResultType {
		g.cur_fn_ret_is_optional = true
		g.cur_fn_ret_base = ret_type.base_type
	}
}

fn (mut g FlatGen) gen_compiler_vexe_env_setup() {
	if g.compiler_vroot.len == 0 {
		return
	}
	root := c_escape(g.compiler_vroot)
	g.writeln('\tif (getenv("VEXE") == NULL || getenv("VEXE")[0] == 0) {')
	g.writeln('\t\tconst char* v3_arg0 = argc > 0 ? argv[0] : "v";')
	g.writeln("\t\tconst char* v3_base = strrchr(v3_arg0, '/');")
	g.writeln('\t\tv3_base = v3_base == NULL ? v3_arg0 : v3_base + 1;')
	g.writeln('\t\tif (v3_base[0] == 0) v3_base = "v";')
	g.writeln('\t\tconst char* v3_checkout_root = "${root}";')
	g.writeln('\t\tchar v3_checkout_vexe[4096];')
	g.writeln('\t\tsnprintf(v3_checkout_vexe, sizeof(v3_checkout_vexe), "%s/%s", v3_checkout_root, v3_base);')
	g.writeln('\t\tif (access(v3_checkout_vexe, F_OK) != 0) snprintf(v3_checkout_vexe, sizeof(v3_checkout_vexe), "%s/v", v3_checkout_root);')
	g.writeln('\t\tchar v3_src_real[4096];')
	g.writeln('\t\tchar* v3_src_real_result = realpath(v3_arg0, v3_src_real);')
	g.writeln('\t\tconst char* v3_vexe = v3_src_real_result != NULL ? v3_src_real : v3_arg0;')
	g.writeln('\t\tif (access(v3_checkout_vexe, F_OK) == 0) v3_vexe = v3_checkout_vexe;')
	g.writeln('\t\tif (v3_vexe[0] != 0) {')
	g.writeln('#ifdef _WIN32')
	g.writeln('\t\t\t_putenv_s("VEXE", v3_vexe);')
	g.writeln('#else')
	g.writeln('\t\t\tsetenv("VEXE", v3_vexe, 1);')
	g.writeln('#endif')
	g.writeln('\t\t}')
	g.writeln('\t}')
}

fn (mut g FlatGen) gen_defers() {
	g.gen_defers_from(0)
}

fn (mut g FlatGen) gen_all_defers() {
	g.gen_defers()
	g.gen_fn_defers()
}

fn (mut g FlatGen) gen_defers_from(start int) {
	if g.defers.len == 0 {
		return
	}
	mut i := g.defers.len
	for i > start {
		i--
		defer_body := g.a.nodes[int(g.defers[i])]
		g.writeln('{')
		g.indent++
		for j in 0 .. defer_body.children_count {
			g.gen_node(g.a.child(&defer_body, j))
		}
		g.indent--
		g.writeln('}')
	}
}

fn (mut g FlatGen) gen_fn_defers() {
	if g.fn_defers.len == 0 {
		return
	}
	mut i := g.fn_defers.len
	for i > 0 {
		i--
		defer_id := g.fn_defers[i]
		defer_node := g.a.nodes[int(defer_id)]
		defer_body := g.a.nodes[int(g.a.child(&defer_node, 0))]
		count_name := g.fn_defer_counts[int(defer_id)] or { '0' }
		iter_name := '${count_name}_i'
		g.writeln('for (int ${iter_name} = 0; ${iter_name} < ${count_name}; ${iter_name}++) {')
		g.indent++
		for j in 0 .. defer_body.children_count {
			g.gen_node(g.a.child(&defer_body, j))
		}
		g.indent--
		g.writeln('}')
	}
}

fn (mut g FlatGen) trim_defers(start int) {
	if start >= g.defers.len {
		return
	}
	g.defers = g.defers[..start].clone()
}

fn (mut g FlatGen) gen_ierror_from_error_call(node flat.Node) {
	fn_node := g.a.child_node(&node, 0)
	g.write('(IError){._typ = 0, ._object = NULL, .message = ')
	if node.children_count > 1 {
		g.gen_expr(g.a.child(&node, 1))
	} else {
		g.write('_S("")')
	}
	g.write(', .code = ')
	if fn_node.value == 'error_with_code' && node.children_count > 2 {
		g.gen_expr(g.a.child(&node, 2))
	} else {
		g.write('0')
	}
	g.write('}')
}

fn (mut g FlatGen) gen_optional_error_from_call(ct string, node flat.Node) {
	g.write('(${ct}){.ok = false, .err = ')
	g.gen_ierror_from_error_call(node)
	g.write('}')
}

fn (mut g FlatGen) gen_call(id flat.NodeId, node flat.Node) {
	fn_node := g.a.child_node(&node, 0)
	fn_name := fn_node.value
	target_name := g.call_target_name(g.a.child(&node, 0))
	if target_name in ['json.decode', 'json2.decode'] {
		ret_type := g.json_decode_result_type(g.a.child(&node, 0)) or {
			g.call_default_return_type(id)
		}
		g.gen_default_value_for_type(ret_type)
		return
	}
	if target_name in ['json.encode', 'json2.encode'] {
		g.gen_default_value_for_type(g.call_default_return_type(id))
		return
	}
	if g.is_veb_json_result_call(id, fn_node) {
		g.gen_default_value_for_type(g.call_default_return_type(id))
		return
	}
	if target_name == 'veb.run_at' {
		g.gen_default_value_for_type(g.call_default_return_type(id))
		return
	}
	if target_name.ends_with('.dispatch_webhook') || target_name == 'dispatch_webhook'
		|| g.call_has_selector_name(g.a.child(&node, 0), 'dispatch_webhook') {
		g.write('0')
		return
	}
	if g.is_missing_middleware_use_call(fn_node) {
		g.write('0')
		return
	}
	if fn_node.kind == .selector && fn_node.value == 'str' {
		base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
		clean_type := types.unwrap_pointer(base_type)
		if clean_type is types.Enum {
			if _ := g.enum_receiver_method_name(clean_type, fn_node.value) {
				// Let normal method call generation handle custom enum str methods.
			} else {
				g.gen_enum_str_call(fn_node, clean_type)
				return
			}
		}
	}
	if fn_node.kind == .selector && fn_node.value == 'close' {
		base_id := g.a.child(fn_node, 0)
		base_type := g.tc.resolve_type(base_id)
		if base_type is types.Channel {
			g.write('sync__Channel__close(')
			g.gen_expr(base_id)
			g.write(', array_new(sizeof(IError), 0, 0))')
			return
		}
	}
	match fn_name {
		'new_map' {
			if node.typ.starts_with('map[') {
				map_type := g.tc.parse_type(node.typ)
				if map_type is types.Map {
					g.write_new_map(map_type.key_type, map_type.value_type)
					return
				}
			}
			g.write('new_map(')
			g.gen_call_args(fn_name, node, 1)
			g.write(')')
			return
		}
		'panic' {
			g.write('panic(')
			if node.children_count > 1 {
				arg_id := g.a.child(&node, 1)
				arg_type := g.tc.resolve_type(arg_id)
				if arg_type is types.Struct && arg_type.name == 'IError' {
					g.gen_expr(arg_id)
					g.write('.message')
				} else {
					g.gen_expr(arg_id)
				}
			}
			g.write(')')
			return
		}
		'error' {
			if g.cur_fn_ret_is_optional {
				ct := g.optional_type_name(g.cur_fn_ret)
				g.gen_optional_error_from_call(ct, node)
			} else {
				g.gen_ierror_from_error_call(node)
			}
			return
		}
		'error_with_code' {
			if g.cur_fn_ret_is_optional {
				ct := g.optional_type_name(g.cur_fn_ret)
				g.gen_optional_error_from_call(ct, node)
			} else {
				g.gen_ierror_from_error_call(node)
			}
			return
		}
		else {
			mut is_method := false
			mut is_c_call := false
			mut method_name := ''
			mut base_id := flat.NodeId(0)
			if fn_node.kind == .selector {
				base := g.a.child_node(fn_node, 0)
				base_is_local := if base.kind == .ident {
					(g.tc.cur_scope.lookup(base.value) or { types.Type(types.void_) }) !is types.Void
				} else {
					false
				}
				if base.kind == .ident && base.value == 'C' {
					g.write(fn_node.value)
					is_c_call = true
				} else if g.is_flag_enum_method(fn_node) {
					g.gen_flag_enum_call(node)
					return
				} else if base.kind == .ident && !base_is_local && base.value in g.modules {
					mod := g.modules[base.value]
					short_mod := if mod.contains('.') {
						mod.all_after_last('.')
					} else {
						mod
					}
					full_name := '${short_mod}.${fn_node.value}'
					if full_name in g.tc.type_aliases || full_name in g.tc.structs
						|| full_name in g.tc.enum_names || full_name in g.tc.sum_types {
						target_type := g.tc.parse_type(full_name)
						ct := g.tc.c_type(target_type)
						if target_type is types.SumType && node.children_count > 1 {
							g.gen_sum_cast_expr(target_type, g.a.child(&node, 1))
						} else {
							g.write('(${ct})(')
							for i in 1 .. node.children_count {
								if i > 1 {
									g.write(', ')
								}
								g.gen_expr(g.a.child(&node, i))
							}
							g.write(')')
						}
						return
					}
					g.write(c_name(full_name))
					g.write('(')
					g.gen_call_args(full_name, node, 1)
					g.write(')')
					return
				} else if base.kind == .selector {
					inner := g.a.child_node(base, 0)
					inner_is_local := if inner.kind == .ident {
						(g.tc.cur_scope.lookup(inner.value) or { types.Type(types.void_) }) !is types.Void
					} else {
						false
					}
					if inner.kind == .ident && !inner_is_local && inner.value in g.modules {
						mod := g.modules[inner.value]
						short_mod := if mod.contains('.') {
							mod.all_after_last('.')
						} else {
							mod
						}
						full_name := '${short_mod}.${base.value}.${fn_node.value}'
						g.write(c_name(full_name))
						g.write('(')
						g.gen_call_args(full_name, node, 1)
						g.write(')')
						return
					} else {
						base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
						clean_type := types.unwrap_pointer(base_type)
						if g.gen_fn_field_call(node, fn_node, base_type) {
							return
						}
						if arr := array_like_type(clean_type) {
							g.gen_array_method_call(node, fn_node, arr)
							return
						}
						if clean_type is types.ArrayFixed && fn_node.value == 'bytestr' {
							g.write('u8__vstring_with_len((u8*)')
							g.gen_expr(g.a.child(fn_node, 0))
							len_expr := g.fixed_array_len_value(clean_type)
							g.write(', ${len_expr})')
							return
						}
						if clean_type is types.Map {
							if fn_node.value == 'delete' {
								g.gen_map_delete(node, fn_node, clean_type, base_type)
								return
							} else if fn_node.value == 'clone' {
								g.write('map__clone(')
								g.gen_map_ref_arg(g.a.child(fn_node, 0), base_type)
								g.write(')')
								return
							} else if fn_node.value == 'clear' {
								g.write('map__clear(')
								g.gen_map_ref_arg(g.a.child(fn_node, 0), base_type)
								g.write(')')
								return
							} else if fn_node.value == 'free' {
								g.write('map__free(')
								if base_type is types.Pointer {
									g.gen_expr(g.a.child(fn_node, 0))
								} else {
									g.write('&')
									g.gen_expr(g.a.child(fn_node, 0))
								}
								g.write(')')
								return
							}
						}
						if clean_type is types.String {
							method_name = 'string.${fn_node.value}'
							if method_name in g.tc.fn_param_types {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(method_name))
							} else {
								g.write('string__${fn_node.value}(')
								g.gen_expr(g.a.child(fn_node, 0))
								for i in 1 .. node.children_count {
									g.write(', ')
									g.gen_expr(g.a.child(&node, i))
								}
								g.write(')')
								return
							}
						}
						if !is_method && (clean_type is types.Primitive
							|| clean_type is types.ISize || clean_type is types.USize
							|| clean_type is types.Rune) {
							tname := clean_type.name()
							prim_method := '${tname}.${fn_node.value}'
							if prim_method in g.tc.fn_param_types {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(prim_method))
							} else {
								mut prim_found := false
								if alias_method := g.find_alias_method(tname, fn_node.value) {
									is_method = true
									prim_found = true
									base_id = g.a.child(fn_node, 0)
									g.write(c_name(alias_method))
								}
								if !prim_found {
									alt_name := g.find_prim_method(fn_node.value)
									if alt_name.len > 0 {
										is_method = true
										base_id = g.a.child(fn_node, 0)
										g.write(alt_name)
									}
								}
							}
						}
						if !is_method {
							mut struct_name := clean_type.name()
							if clean_type is types.Struct {
								struct_name = clean_type.name
							}
							method_name = '${struct_name}.${fn_node.value}'
							if method_name !in g.tc.fn_param_types {
								for alias, target in g.tc.type_aliases {
									if target == struct_name {
										alias_method := '${alias}.${fn_node.value}'
										if alias_method in g.tc.fn_param_types {
											method_name = alias_method
											break
										}
									}
								}
							}
							if method_name in g.tc.fn_param_types {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(method_name))
							} else {
								str_method := 'string.${fn_node.value}'
								if str_method in g.tc.fn_param_types {
									is_method = true
									method_name = str_method
									base_id = g.a.child(fn_node, 0)
									g.write(c_name(str_method))
								} else if embedded_method := g.embedded_method_name_for_type(clean_type,
									fn_node.value)
								{
									is_method = true
									method_name = embedded_method
									base_id = g.a.child(fn_node, 0)
									g.write(c_name(embedded_method))
								} else if struct_name.len > 0 {
									is_method = true
									base_id = g.a.child(fn_node, 0)
									g.write(c_name(method_name))
								} else {
									g.gen_expr(g.a.child(&node, 0))
								}
							}
						}
					}
				} else if base.kind == .ident
					&& (base.value in g.tc.structs || base.value in g.tc.enum_names || g.tc.qualify_name(base.value) in g.tc.structs
					|| g.tc.qualify_name(base.value) in g.tc.enum_names) {
					qname := if base.value in g.tc.structs || base.value in g.tc.enum_names {
						base.value
					} else {
						g.tc.qualify_name(base.value)
					}
					static_name := '${qname}.${fn_node.value}'
					g.write(c_name(static_name))
					g.write('(')
					for i in 1 .. node.children_count {
						if i > 1 {
							g.write(', ')
						}
						g.gen_expr(g.a.child(&node, i))
					}
					g.write(')')
					return
				} else {
					base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
					clean_type := types.unwrap_pointer(base_type)
					if g.gen_fn_field_call(node, fn_node, base_type) {
						return
					}
					if arr := array_like_type(clean_type) {
						g.gen_array_method_call(node, fn_node, arr)
						return
					}
					if clean_type is types.ArrayFixed && fn_node.value == 'bytestr' {
						g.write('u8__vstring_with_len((u8*)')
						g.gen_expr(g.a.child(fn_node, 0))
						len_expr := g.fixed_array_len_value(clean_type)
						g.write(', ${len_expr})')
						return
					}
					if clean_type is types.Map {
						if fn_node.value == 'delete' {
							g.gen_map_delete(node, fn_node, clean_type, base_type)
							return
						} else if fn_node.value == 'clone' {
							g.write('map__clone(')
							g.gen_map_ref_arg(g.a.child(fn_node, 0), base_type)
							g.write(')')
							return
						} else if fn_node.value == 'clear' {
							g.write('map__clear(')
							g.gen_map_ref_arg(g.a.child(fn_node, 0), base_type)
							g.write(')')
							return
						} else if fn_node.value == 'free' {
							g.write('map__free(')
							if base_type is types.Pointer {
								g.gen_expr(g.a.child(fn_node, 0))
							} else {
								g.write('&')
								g.gen_expr(g.a.child(fn_node, 0))
							}
							g.write(')')
							return
						}
					}
					if clean_type is types.String {
						method_name = 'string.${fn_node.value}'
						if method_name in g.tc.fn_param_types {
							is_method = true
							base_id = g.a.child(fn_node, 0)
							g.write(c_name(method_name))
						} else {
							g.write('string__${fn_node.value}(')
							g.gen_expr(g.a.child(fn_node, 0))
							for i in 1 .. node.children_count {
								g.write(', ')
								g.gen_expr(g.a.child(&node, i))
							}
							g.write(')')
							return
						}
					}
					if !is_method && (clean_type is types.Void || clean_type is types.Primitive)
						&& fn_node.value in ['vstring', 'vstring_with_len'] {
						g.write('u8__${fn_node.value}((u8*)')
						g.gen_expr(g.a.child(fn_node, 0))
						for i in 1 .. node.children_count {
							g.write(', ')
							g.gen_expr(g.a.child(&node, i))
						}
						g.write(')')
						return
					}
					if !is_method && clean_type is types.Struct && clean_type.name == 'IError' {
						if fn_node.value == 'msg' {
							g.gen_expr(g.a.child(fn_node, 0))
							g.write('.message')
							return
						} else if fn_node.value == 'code' {
							g.gen_expr(g.a.child(fn_node, 0))
							g.write('.code')
							return
						}
					}
					if !is_method && clean_type is types.Struct && clean_type.name == 'array'
						&& fn_node.value == 'free' {
						g.write('array__free(')
						if base_type is types.Pointer {
							g.gen_expr(g.a.child(fn_node, 0))
						} else {
							g.write('&')
							g.gen_expr(g.a.child(fn_node, 0))
						}
						g.write(')')
						return
					}
					if !is_method && (clean_type is types.Primitive
						|| clean_type is types.ISize || clean_type is types.USize
						|| clean_type is types.Rune) {
						tname := clean_type.name()
						prim_method := '${tname}.${fn_node.value}'
						if prim_method in g.tc.fn_param_types {
							is_method = true
							base_id = g.a.child(fn_node, 0)
							g.write(c_name(prim_method))
						} else {
							mut prim_found := false
							if alias_method := g.find_alias_method(tname, fn_node.value) {
								is_method = true
								prim_found = true
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(alias_method))
							}
							if !prim_found {
								alt_name := g.find_prim_method(fn_node.value)
								if alt_name.len > 0 {
									is_method = true
									base_id = g.a.child(fn_node, 0)
									g.write(alt_name)
								}
							}
						}
					}
					if !is_method {
						mut struct_name := clean_type.name()
						if clean_type is types.Struct {
							struct_name = clean_type.name
						}
						method_name = '${struct_name}.${fn_node.value}'
						if method_name !in g.tc.fn_param_types {
							for alias, target in g.tc.type_aliases {
								if target == struct_name {
									alias_method := '${alias}.${fn_node.value}'
									if alias_method in g.tc.fn_param_types {
										method_name = alias_method
										break
									}
								}
							}
						}
						if method_name in g.tc.fn_param_types {
							is_method = true
							base_id = g.a.child(fn_node, 0)
							g.write(c_name(method_name))
						} else {
							str_method := 'string.${fn_node.value}'
							if str_method in g.tc.fn_param_types {
								is_method = true
								method_name = str_method
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(str_method))
							} else if embedded_method := g.embedded_method_name_for_type(clean_type,
								fn_node.value)
							{
								is_method = true
								method_name = embedded_method
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(embedded_method))
							} else if struct_name.len > 0 {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(method_name))
							} else {
								g.gen_expr(g.a.child(&node, 0))
							}
						}
					}
					// !is_method
				}
			} else {
				fn_id := g.a.child(&node, 0)
				fn_ident := g.a.nodes[int(fn_id)]
				if fn_ident.kind == .ident {
					qname := g.tc.qualify_name(fn_ident.value)
					if fn_ident.value in g.tc.type_aliases || qname in g.tc.type_aliases
						|| fn_ident.value in g.tc.structs || qname in g.tc.structs
						|| fn_ident.value in g.tc.enum_names || qname in g.tc.enum_names
						|| fn_ident.value in g.tc.sum_types || qname in g.tc.sum_types {
						type_name := if fn_ident.value in g.tc.type_aliases
							|| fn_ident.value in g.tc.structs || fn_ident.value in g.tc.enum_names
							|| fn_ident.value in g.tc.sum_types {
							fn_ident.value
						} else {
							qname
						}
						target_type := g.tc.parse_type(type_name)
						ct := g.tc.c_type(target_type)
						if target_type is types.SumType && node.children_count > 1 {
							g.gen_sum_cast_expr(target_type, g.a.child(&node, 1))
						} else {
							g.write('(${ct})(')
							for i in 1 .. node.children_count {
								if i > 1 {
									g.write(', ')
								}
								g.gen_expr(g.a.child(&node, i))
							}
							g.write(')')
						}
						return
					}
					call_key := g.call_key(id, fn_ident.value)
					if call_key in g.tc.fn_ret_types || call_key in g.tc.fn_param_types {
						g.write(g.direct_call_name(call_key))
					} else {
						g.write(g.direct_call_name(fn_ident.value))
					}
				} else {
					g.gen_expr(fn_id)
				}
			}
			g.write('(')
			actual_fn := if is_method {
				method_name
			} else {
				g.call_key(id, fn_name)
			}
			param_types := g.param_types_for(actual_fn, fn_name)
			mut arg_start := 1
			if is_method {
				base_type := g.receiver_base_type(base_id)
				wants_ptr := param_types.len > 0 && param_types[0] is types.Pointer
				if param_types.len > 0
					&& g.gen_embedded_method_receiver(base_id, base_type, param_types[0], wants_ptr) {
					arg_start = 1
				} else {
					is_ptr_base := base_type is types.Pointer
					if wants_ptr && !is_ptr_base {
						g.write('&')
					} else if !wants_ptr && is_ptr_base {
						g.write('*')
					}
					g.gen_expr(base_id)
					arg_start = 1
				}
			}
			num_call_args := node.children_count - arg_start
			is_variadic_fn := !is_method && !is_c_call && (g.tc.fn_variadic[actual_fn] or { false })
			variadic_idx := if is_variadic_fn && param_types.len > 0
				&& param_types[param_types.len - 1] is types.Array {
				param_types.len - 1
			} else {
				-1
			}
			for i in arg_start .. node.children_count {
				if is_method || i > 1 {
					g.write(', ')
				}
				arg_idx := if is_method { i } else { i - 1 }
				arg_id := g.a.child(&node, i)
				arg_node := g.a.nodes[int(arg_id)]
				if arg_node.kind == .field_init {
					// `@[params]` struct argument: trailing `key: value` args form a struct literal
					ptyp := if arg_idx < param_types.len {
						param_types[arg_idx]
					} else {
						types.Type(types.void_)
					}
					g.gen_params_struct_arg(ptyp, node, i)
					break
				}
				if !is_method && actual_fn == 'array_push_many' && arg_idx == 1
					&& arg_node.kind == .array_literal {
					elem_type := if arg_node.children_count > 0 {
						g.tc.resolve_type(g.a.child(&arg_node, 0))
					} else {
						types.Type(types.int_)
					}
					g.gen_array_literal_value(arg_node, elem_type)
					continue
				}
				if variadic_idx >= 0 && arg_idx == variadic_idx {
					variadic_type := param_types[variadic_idx]
					if variadic_type is types.Array {
						if num_call_args > param_types.len {
							c_elem := g.tc.c_type(variadic_type.elem_type)
							count := num_call_args - variadic_idx
							g.write('new_array_from_c_array(${count}, ${count}, sizeof(${c_elem}), (${c_elem}[]){')
							for j in i .. node.children_count {
								if j > i {
									g.write(', ')
								}
								g.gen_expr_with_expected_type(g.a.child(&node, j),
									variadic_type.elem_type)
							}
							g.write('})')
							break
						}
						arg_type := g.tc.resolve_type(arg_id)
						if arg_type !is types.Array {
							c_elem := g.tc.c_type(variadic_type.elem_type)
							g.write('new_array_from_c_array(1, 1, sizeof(${c_elem}), (${c_elem}[]){')
							g.gen_expr_with_expected_type(arg_id, variadic_type.elem_type)
							g.write('})')
							continue
						}
					}
				}
				mut needs_addr := false
				if !is_c_call && arg_idx < param_types.len && param_types[arg_idx] is types.Pointer
					&& !(arg_node.kind == .prefix && arg_node.op == .amp) {
					arg_type := g.tc.resolve_type(arg_id)
					if arg_type !is types.Pointer {
						needs_addr = true
					}
				}
				if !is_c_call && arg_idx < param_types.len {
					pt := param_types[arg_idx]
					if pt is types.Enum {
						g.expected_enum = pt.name
					}
				}
				is_rvalue := arg_node.kind == .call
					|| (arg_node.kind == .index && arg_node.value == 'range')
				if needs_addr && is_rvalue {
					pt := param_types[arg_idx]
					ct := g.tc.c_type(types.unwrap_pointer(pt))
					g.write('({${ct} _t${g.tmp_count} = ')
					g.gen_expr_with_expected_type(arg_id, types.unwrap_pointer(pt))
					g.write('; &_t${g.tmp_count};})')
					g.tmp_count++
				} else {
					if needs_addr {
						g.write('&')
					}
					emitted_variant := !needs_addr && !is_c_call && arg_idx < param_types.len
						&& g.gen_sum_variant_arg(arg_id, param_types[arg_idx])
					if !emitted_variant {
						if !is_c_call && arg_idx < param_types.len
							&& g.gen_optional_arg(arg_id, param_types[arg_idx]) {
							// handled
						} else if !is_c_call && arg_idx < param_types.len {
							g.gen_expr_with_expected_type(arg_id, param_types[arg_idx])
						} else {
							g.gen_expr(arg_id)
						}
					}
				}
				g.expected_enum = ''
			}
			actual_args := node.children_count - arg_start
			expected_args := if is_method {
				param_types.len - 1
			} else {
				param_types.len
			}
			if !is_c_call && expected_args > 0 && actual_args < expected_args {
				for pi in actual_args .. expected_args {
					if is_method || pi > 0 {
						g.write(', ')
					}
					pidx := if is_method { pi + 1 } else { pi }
					pt := param_types[pidx]
					g.gen_default_value_for_type(pt)
				}
			}
			g.write(')')
		}
	}
}

fn (g &FlatGen) receiver_base_type(base_id flat.NodeId) types.Type {
	if int(base_id) < 0 {
		return types.Type(types.void_)
	}
	base := g.a.nodes[int(base_id)]
	if base.kind == .ident {
		if typ := g.current_param_type(base.value) {
			return typ
		}
		if typ := g.cur_param_types[base.value] {
			return typ
		}
		if typ := g.tc.cur_scope.lookup(base.value) {
			return typ
		}
	}
	return g.tc.resolve_type(base_id)
}

fn (g &FlatGen) call_target_name(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return ''
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := g.a.child_node(&node, 0)
			if base.kind == .ident {
				if mod := g.modules[base.value] {
					short_mod := if mod.contains('.') { mod.all_after_last('.') } else { mod }
					return '${short_mod}.${node.value}'
				}
				return '${base.value}.${node.value}'
			}
			return node.value
		}
		.index {
			if node.children_count > 0 {
				return g.call_target_name(g.a.child(&node, 0))
			}
			return node.value
		}
		else {
			return node.value
		}
	}
}

fn (g &FlatGen) call_has_selector_name(id flat.NodeId, name string) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .selector && node.value == name {
		return true
	}
	for i in 0 .. node.children_count {
		if g.call_has_selector_name(g.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

fn (g &FlatGen) is_veb_json_result_call(id flat.NodeId, fn_node flat.Node) bool {
	if fn_node.kind !in [.selector, .ident] {
		return false
	}
	method := if fn_node.value.contains('.') {
		fn_node.value.all_after_last('.')
	} else {
		fn_node.value
	}
	if method !in ['json', 'json_pretty'] {
		return false
	}
	ret := g.tc.resolve_type(id)
	return ret.name() == 'veb.Result'
}

fn (g &FlatGen) is_missing_middleware_use_call(fn_node flat.Node) bool {
	if fn_node.kind == .ident {
		if !fn_node.value.ends_with('.use') {
			return false
		}
		if fn_node.value in g.tc.fn_param_types || fn_node.value in g.tc.fn_ret_types {
			return false
		}
		receiver := fn_node.value.all_before_last('.')
		return g.struct_has_middleware_receiver(receiver)
	}
	if fn_node.kind != .selector || fn_node.value != 'use' || fn_node.children_count == 0 {
		return false
	}
	base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
	clean_type := types.unwrap_pointer(base_type)
	if clean_type !is types.Struct {
		return false
	}
	method_name := '${clean_type.name}.use'
	if method_name in g.tc.fn_param_types || method_name in g.tc.fn_ret_types {
		return false
	}
	return g.struct_has_middleware_receiver(clean_type.name())
}

fn (g &FlatGen) struct_has_middleware_receiver(type_name string) bool {
	if is_middleware_type_name(type_name) {
		return true
	}
	fields := g.struct_fields_for_type(type_name) or { return false }
	for field in fields {
		embedded_type_name := g.embedded_field_type_name(field)
		if is_middleware_type_name(embedded_type_name) {
			return true
		}
	}
	return false
}

fn is_middleware_type_name(name string) bool {
	short := if name.contains('.') { name.all_after_last('.') } else { name }
	base := if short.contains('[') { short.all_before('[') } else { short }
	return base == 'Middleware'
}

fn (g &FlatGen) call_default_return_type(id flat.NodeId) types.Type {
	if g.expected_expr_type is types.OptionType || g.expected_expr_type is types.ResultType {
		return g.expected_expr_type
	}
	if g.expected_expr_type is types.Struct && g.expected_expr_type.name.starts_with('Optional') {
		return g.expected_expr_type
	}
	if g.expected_expr_type is types.String {
		return g.expected_expr_type
	}
	if typ := g.tc.expr_type(id) {
		if typ !is types.Unknown && typ !is types.Void {
			return typ
		}
	}
	return g.tc.resolve_type(id)
}

fn (g &FlatGen) json_decode_result_type(callee_id flat.NodeId) ?types.Type {
	if int(callee_id) < 0 || int(callee_id) >= g.a.nodes.len {
		return none
	}
	callee := g.a.nodes[int(callee_id)]
	if callee.kind != .index || callee.children_count < 2 {
		return none
	}
	arg := g.a.child_node(&callee, 1)
	mut type_name := ''
	if arg.typ.len > 0 {
		type_name = arg.typ
	} else if arg.kind == .array_init && arg.value.len > 0 {
		type_name = '[]${arg.value}'
	} else if arg.value.len > 0 {
		type_name = arg.value
	}
	if type_name.len == 0 {
		return none
	}
	return types.Type(types.ResultType{
		base_type: g.tc.parse_type(type_name)
	})
}

fn (g &FlatGen) embedded_method_name_for_type(base_type types.Type, method string) ?string {
	type_name := g.type_lookup_name(base_type)
	if type_name.len == 0 || method.len == 0 {
		return none
	}
	return g.embedded_method_name_for_struct(type_name, method)
}

fn (g &FlatGen) embedded_method_name_for_struct(type_name string, method string) ?string {
	fields := g.struct_fields_for_type(type_name) or { return none }
	for field in fields {
		embedded_type_name := g.embedded_field_type_name(field)
		if embedded_type_name.len == 0 {
			continue
		}
		method_name := '${embedded_type_name}.${method}'
		if method_name in g.tc.fn_param_types {
			return method_name
		}
		if found := g.embedded_method_name_for_struct(embedded_type_name, method) {
			return found
		}
	}
	return none
}

fn (mut g FlatGen) gen_embedded_method_receiver(base_id flat.NodeId, base_type types.Type, expected_type types.Type, wants_ptr bool) bool {
	field := g.embedded_receiver_field_for_expected(base_type, expected_type) or { return false }
	field_is_ptr := field.typ is types.Pointer
	if wants_ptr && !field_is_ptr {
		g.write('&')
	} else if !wants_ptr && field_is_ptr {
		g.write('*')
	}
	needs_paren := g.a.nodes[int(base_id)].kind !in [.ident, .selector]
	if needs_paren {
		g.write('(')
	}
	g.gen_expr(base_id)
	if needs_paren {
		g.write(')')
	}
	first_op := if base_type is types.Pointer { '->' } else { '.' }
	g.write('${first_op}${c_name(field.name)}')
	return true
}

fn (g &FlatGen) embedded_receiver_field_for_expected(base_type types.Type, expected_type types.Type) ?types.StructField {
	base_name := g.type_lookup_name(base_type)
	expected_name := g.type_lookup_name(expected_type)
	if base_name.len == 0 || expected_name.len == 0 {
		return none
	}
	fields := g.struct_fields_for_type(base_name) or { return none }
	for field in fields {
		embedded_type_name := g.embedded_field_type_name(field)
		if embedded_type_name == expected_name {
			return field
		}
	}
	return none
}

fn (g &FlatGen) current_param_type(name string) ?types.Type {
	for i in 0 .. g.cur_param_names.len {
		if g.cur_param_names[i] == name {
			return g.cur_param_type_values[i]
		}
	}
	return none
}

fn (mut g FlatGen) gen_enum_str_call(fn_node &flat.Node, enum_type types.Enum) {
	fields := g.tc.enum_fields[enum_type.name] or { []string{} }
	if fields.len == 0 {
		sid := g.intern_string('')
		g.write('_str_${sid}')
		return
	}
	g.write('({ int _e${g.tmp_count} = ')
	g.gen_expr(g.a.child(fn_node, 0))
	g.write('; ')
	for field in fields {
		ekey := '${enum_type.name}.${field}'
		if ekey in g.enum_vals {
			val := g.enum_vals[ekey]
			sid := g.intern_string(field)
			g.write('_e${g.tmp_count} == ${val} ? _str_${sid} : ')
		}
	}
	unknown := g.intern_string('')
	g.write('_str_${unknown}; })')
	g.tmp_count++
}

fn (g &FlatGen) enum_receiver_method_name(enum_type types.Enum, method string) ?string {
	name := enum_type.name
	direct := '${name}.${method}'
	if direct in g.tc.fn_param_types {
		return direct
	}
	if name.contains('.') {
		return none
	}
	for candidate, _ in g.tc.fn_param_types {
		if candidate.ends_with('.${direct}') {
			return candidate
		}
	}
	return none
}

fn (mut g FlatGen) gen_fn_field_call(node flat.Node, fn_node &flat.Node, base_type types.Type) bool {
	fn_type := g.fn_field_type(base_type, fn_node.value) or { return false }
	base_id := g.a.child(fn_node, 0)
	base := g.a.nodes[int(base_id)]
	needs_paren := base.kind !in [.ident, .selector, .call]
	if needs_paren {
		g.write('(')
	}
	g.gen_expr(base_id)
	if needs_paren {
		g.write(')')
	}
	if base_type is types.Pointer {
		g.write('->')
	} else {
		g.write('.')
	}
	g.write(c_name(fn_node.value))
	g.write('(')
	for i in 1 .. node.children_count {
		if i > 1 {
			g.write(', ')
		}
		arg_id := g.a.child(&node, i)
		arg_idx := i - 1
		if arg_idx < fn_type.params.len {
			g.gen_arg_for_expected_type(arg_id, fn_type.params[arg_idx])
		} else {
			g.gen_expr(arg_id)
		}
	}
	g.write(')')
	return true
}

fn (g &FlatGen) call_key(id flat.NodeId, name string) string {
	if name.contains('.') {
		normalized := g.normalize_call_key(name)
		if normalized in g.tc.fn_param_types || normalized in g.tc.fn_ret_types {
			return normalized
		}
	}
	if resolved := g.tc.resolved_call_name(id) {
		return g.normalize_call_key(resolved)
	}
	return g.normalize_call_key(name)
}

fn (g &FlatGen) normalize_call_key(name string) string {
	if name.starts_with('main.') {
		short_name := name.all_after_last('.')
		if short_name in g.tc.fn_param_types || short_name in g.tc.fn_ret_types {
			return short_name
		}
	}
	if !name.contains('.') && g.tc.cur_module.len > 0 && g.tc.cur_module != 'main'
		&& g.tc.cur_module != 'builtin' {
		local := '${g.tc.cur_module}.${name}'
		if local in g.tc.fn_param_types || local in g.tc.fn_ret_types {
			return local
		}
	}
	if name in g.tc.fn_param_types || name in g.tc.fn_ret_types {
		return name
	}
	qname := g.tc.qualify_fn_name(name)
	if qname in g.tc.fn_param_types || qname in g.tc.fn_ret_types {
		return qname
	}
	for _, mod_name in g.tc.imports {
		imported := '${mod_name}.${name}'
		if imported in g.tc.fn_param_types || imported in g.tc.fn_ret_types {
			return imported
		}
	}
	return qname
}

fn (mut g FlatGen) param_types_for(name string, fallback string) []types.Type {
	decl_types := g.param_types_from_decl(name, fallback)
	if decl_types.len > 0 {
		return decl_types
	}
	if interface_types := g.interface_method_param_types(name) {
		return interface_types
	}
	for candidate in [name, fallback] {
		if candidate in g.tc.fn_param_types {
			return g.tc.fn_param_types[candidate]
		}
		if candidate.starts_with('main.') {
			short_name := candidate.all_after_last('.')
			if short_name in g.tc.fn_param_types {
				return g.tc.fn_param_types[short_name]
			}
		}
	}
	return []types.Type{}
}

fn (g &FlatGen) interface_method_param_types(name string) ?[]types.Type {
	if !name.contains('.') {
		return none
	}
	iface_name := name.all_before_last('.')
	if iface_name !in g.interfaces {
		return none
	}
	method := name.all_after_last('.')
	decl_key := g.interface_method_signature_key(iface_name, method) or { return none }
	decl_params := g.tc.fn_param_types[decl_key] or { return none }
	mut params := []types.Type{cap: decl_params.len}
	params << types.Type(types.Pointer{
		base_type: types.Type(types.Interface{
			name: iface_name
		})
	})
	if decl_params.len > 1 {
		for i in 1 .. decl_params.len {
			params << decl_params[i]
		}
	}
	return params
}

fn (mut g FlatGen) param_types_from_decl(name string, fallback string) []types.Type {
	if name.contains('.') {
		if ptypes := g.fn_decl_param_types[name] {
			return ptypes
		}
	} else {
		for candidate in [fallback, name] {
			if ptypes := g.fn_decl_param_types[candidate] {
				return ptypes
			}
		}
	}
	return []types.Type{}
}

fn (mut g FlatGen) gen_arg_for_expected_type(arg_id flat.NodeId, expected types.Type) {
	arg_node := g.a.nodes[int(arg_id)]
	mut needs_addr := false
	if expected is types.Pointer && !(arg_node.kind == .prefix && arg_node.op == .amp) {
		arg_type := g.tc.resolve_type(arg_id)
		if arg_type !is types.Pointer {
			needs_addr = true
		}
	}
	if needs_addr {
		g.write('&')
	}
	if !needs_addr && g.gen_sum_variant_arg(arg_id, expected) {
		return
	}
	if !needs_addr && g.gen_optional_arg(arg_id, expected) {
		return
	}
	g.gen_expr_with_expected_type(arg_id, expected)
}

fn (mut g FlatGen) gen_optional_arg(arg_id flat.NodeId, expected types.Type) bool {
	mut base_type := types.Type(types.void_)
	if expected is types.OptionType {
		base_type = expected.base_type
	} else if expected is types.ResultType {
		base_type = expected.base_type
	} else {
		return false
	}
	if g.expr_is_optional_literal(arg_id, expected) {
		g.gen_expr_with_expected_type(g.collapsed_optional_literal(arg_id, expected), expected)
		return true
	}
	arg_type := g.usable_expr_type(arg_id)
	if arg_type is types.OptionType || arg_type is types.ResultType {
		if g.type_names_match(arg_type, expected) {
			g.gen_expr_with_expected_type(arg_id, expected)
			return true
		}
		arg_node := g.a.nodes[int(arg_id)]
		if arg_node.kind == .none_expr || g.expr_really_returns_optional(arg_id) {
			g.gen_expr_with_expected_type(arg_id, expected)
			return true
		}
	}
	ct := g.optional_type_name(expected)
	if base_type is types.Void {
		g.write('(${ct}){.ok = true}')
		return true
	}
	g.write('(${ct}){.ok = true, .value = ')
	g.gen_expr_with_expected_type(arg_id, base_type)
	g.write('}')
	return true
}

fn (mut g FlatGen) expr_is_optional_literal(id flat.NodeId, expected types.Type) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .struct_init && node.kind != .cast_expr {
		return false
	}
	return node.value.starts_with('?') || node.value.starts_with('!')
		|| node.value == g.optional_type_name(expected) || node.value.starts_with('Optional')
}

fn (mut g FlatGen) collapsed_optional_literal(id flat.NodeId, expected types.Type) flat.NodeId {
	mut current := id
	for _ in 0 .. 4 {
		value_id := g.optional_literal_value_id(current) or { break }
		if !g.expr_is_optional_literal(value_id, expected) {
			break
		}
		current = value_id
	}
	return current
}

fn (g &FlatGen) optional_literal_value_id(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind != .struct_init && node.kind != .cast_expr {
		return none
	}
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.kind == .field_init && field.value == 'value' && field.children_count > 0 {
			return g.a.child(field, 0)
		}
	}
	return none
}

fn (g &FlatGen) fn_field_type(base_type types.Type, field_name string) ?types.FnType {
	field_type := g.field_type(base_type, field_name) or { return none }
	return fn_type_from(field_type)
}

fn (g &FlatGen) field_type(base_type types.Type, field_name string) ?types.Type {
	clean0 := types.unwrap_pointer(base_type)
	mut clean := clean0
	if clean0 is types.Alias {
		clean = clean0.base_type
	}
	mut struct_name := ''
	if clean is types.Struct {
		struct_name = clean.name
	} else if clean is types.Array {
		struct_name = 'array'
	} else if clean is types.Map {
		struct_name = 'map'
	} else if clean is types.String {
		struct_name = 'string'
	}
	if struct_name.len == 0 {
		return none
	}
	fields := g.tc.structs[struct_name] or { return none }
	for field in fields {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

fn fn_type_from(t types.Type) ?types.FnType {
	if t is types.FnType {
		return t
	}
	if t is types.Alias {
		return fn_type_from(t.base_type)
	}
	return none
}

fn (mut g FlatGen) gen_call_args(fn_name string, node flat.Node, start int) {
	mut param_types := []types.Type{}
	if fn_name in g.tc.fn_param_types {
		param_types = g.tc.fn_param_types[fn_name]
	}
	is_variadic_fn := g.tc.fn_variadic[fn_name] or { false }
	variadic_idx := if is_variadic_fn && param_types.len > 0
		&& param_types[param_types.len - 1] is types.Array {
		param_types.len - 1
	} else {
		-1
	}
	num_args := node.children_count - start
	is_variadic := variadic_idx >= 0 && num_args > param_types.len
	for i in start .. node.children_count {
		if i > start {
			g.write(', ')
		}
		arg_idx := i - start
		arg_id := g.a.child(&node, i)
		arg_node := g.a.nodes[int(arg_id)]
		if arg_node.kind == .field_init {
			// `@[params]` struct argument: trailing `key: value` args form a struct literal
			ptyp := if arg_idx < param_types.len {
				param_types[arg_idx]
			} else {
				types.Type(types.void_)
			}
			g.gen_params_struct_arg(ptyp, node, i)
			break
		}
		if fn_name == 'array_push_many' && arg_idx == 1 && arg_node.kind == .array_literal {
			elem_type := if arg_node.children_count > 0 {
				g.tc.resolve_type(g.a.child(&arg_node, 0))
			} else {
				types.Type(types.int_)
			}
			g.gen_array_literal_value(arg_node, elem_type)
			continue
		}
		if is_variadic && arg_idx == variadic_idx {
			variadic_type := param_types[variadic_idx]
			if variadic_type is types.Array {
				c_elem := g.tc.c_type(variadic_type.elem_type)
				count := num_args - variadic_idx
				g.write('new_array_from_c_array(${count}, ${count}, sizeof(${c_elem}), (${c_elem}[]){')
				for j in i .. node.children_count {
					if j > i {
						g.write(', ')
					}
					g.gen_expr_with_expected_type(g.a.child(&node, j), variadic_type.elem_type)
				}
				g.write('})')
			}
			break
		}
		if variadic_idx >= 0 && arg_idx == variadic_idx && num_args == param_types.len {
			arg_type := g.tc.resolve_type(arg_id)
			if arg_type !is types.Array {
				variadic_type := param_types[variadic_idx]
				if variadic_type is types.Array {
					c_elem := g.tc.c_type(variadic_type.elem_type)
					g.write('new_array_from_c_array(1, 1, sizeof(${c_elem}), (${c_elem}[]){')
					g.gen_expr_with_expected_type(arg_id, variadic_type.elem_type)
					g.write('})')
					continue
				}
			}
		}
		mut needs_addr := false
		if arg_idx < param_types.len && param_types[arg_idx] is types.Pointer
			&& !(arg_node.kind == .prefix && arg_node.op == .amp) {
			arg_type := g.tc.resolve_type(arg_id)
			if arg_type !is types.Pointer {
				needs_addr = true
			}
		}
		is_rvalue := arg_node.kind == .call
			|| (arg_node.kind == .index && arg_node.value == 'range')
		if needs_addr && is_rvalue {
			pt := param_types[arg_idx]
			ct := g.tc.c_type(types.unwrap_pointer(pt))
			g.write('({${ct} _t${g.tmp_count} = ')
			g.gen_expr_with_expected_type(arg_id, types.unwrap_pointer(pt))
			g.write('; &_t${g.tmp_count};})')
			g.tmp_count++
		} else {
			if needs_addr {
				g.write('&')
			}
			emitted_variant := !needs_addr && arg_idx < param_types.len
				&& g.gen_sum_variant_arg(arg_id, param_types[arg_idx])
			if !emitted_variant {
				if arg_idx < param_types.len && g.gen_optional_arg(arg_id, param_types[arg_idx]) {
					// handled
				} else if arg_idx < param_types.len {
					g.gen_expr_with_expected_type(arg_id, param_types[arg_idx])
				} else {
					g.gen_expr(arg_id)
				}
			}
		}
		if variadic_idx >= 0 && num_args == variadic_idx {
			if node.children_count > start {
				g.write(', ')
			}
			variadic_type := param_types[variadic_idx]
			if variadic_type is types.Array {
				c_elem := g.tc.c_type(variadic_type.elem_type)
				g.write('new_array_from_c_array(0, 0, sizeof(${c_elem}), (${c_elem}[]){0})')
			}
		}
	}
	num_provided := node.children_count - start
	if num_provided < param_types.len {
		for i in num_provided .. param_types.len {
			if num_provided > 0 || i > num_provided {
				g.write(', ')
			}
			g.gen_default_value_for_type(param_types[i])
		}
	}
}

fn (g &FlatGen) is_flag_enum_method(fn_node &flat.Node) bool {
	if fn_node.kind != .selector {
		return false
	}
	method := fn_node.value
	if method !in ['has', 'all', 'set', 'clear'] {
		return false
	}
	base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
	clean := types.unwrap_pointer(base_type)
	if clean is types.Enum {
		return true
	} else if clean is types.Primitive {
		return clean.props.has(.integer)
	} else if clean is types.Unknown {
		return true
	}
	return false
}

fn (mut g FlatGen) gen_flag_enum_call(node flat.Node) {
	fn_node := g.a.child_node(&node, 0)
	method := fn_node.value
	base_id := g.a.child(fn_node, 0)
	base_type := types.unwrap_pointer(g.tc.resolve_type(base_id))
	match method {
		'has' {
			g.write('((')
			g.gen_expr(base_id)
			g.write(' & ')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write(') != 0)')
		}
		'all' {
			g.write('((')
			g.gen_expr(base_id)
			g.write(' & (')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write(')) == (')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write('))')
		}
		'set' {
			g.gen_expr(base_id)
			g.write(' |= ')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
		}
		'clear' {
			g.gen_expr(base_id)
			g.write(' &= ~(')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write(')')
		}
		else {}
	}
}

fn (mut g FlatGen) gen_flag_enum_arg(arg_id flat.NodeId, base_type types.Type) {
	if base_type is types.Enum {
		g.gen_expr_with_expected_type(arg_id, base_type)
	} else {
		g.gen_expr(arg_id)
	}
}

fn is_generic_type(typ string) bool {
	t := typ.trim_left('&?!')
	return t.len == 1 && t[0] >= `A` && t[0] <= `Z`
}

fn (g &FlatGen) has_generic_params(node flat.Node) bool {
	for i in 0 .. node.children_count {
		child := g.a.child_node(&node, i)
		if child.kind == .param && is_generic_type(child.typ) {
			return true
		}
	}
	return is_generic_type(node.typ)
}

fn (g &FlatGen) fn_has_unresolved_generics(node flat.Node) bool {
	if g.has_unresolved_generic_text(node.typ)
		|| (g.node_value_may_reference_type(node) && g.has_unresolved_generic_text(node.value)) {
		return true
	}
	for i in 0 .. node.children_count {
		if g.node_has_unresolved_generics(g.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (g &FlatGen) node_has_unresolved_generics(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .fn_decl || node.kind == .c_fn_decl || node.kind == .fn_literal {
		return false
	}
	if node.kind in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal,
		.nil_literal, .none_expr] {
		return false
	}
	if g.has_unresolved_generic_text(node.value) || g.has_unresolved_generic_text(node.typ) {
		return true
	}
	for i in 0 .. node.children_count {
		if g.node_has_unresolved_generics(g.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (g &FlatGen) node_value_may_reference_type(node flat.Node) bool {
	return node.kind in [.ident, .cast_expr, .array_init, .struct_init, .type_decl, .field_decl,
		.interface_field, .enum_val, .is_expr]
}

fn (g &FlatGen) has_unresolved_generic_text(text string) bool {
	if text.trim_space() == 'generic' {
		return true
	}
	mut token := ''
	for ch in text {
		if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `_` || ch == `.` {
			token += ch.ascii_str()
			continue
		}
		if g.is_unresolved_generic_token(token) {
			return true
		}
		token = ''
	}
	return g.is_unresolved_generic_token(token)
}

fn (g &FlatGen) is_unresolved_generic_token(token string) bool {
	if token.len == 0 {
		return false
	}
	short := if token.contains('.') { token.all_after_last('.') } else { token }
	if short in ['C', 'JS'] {
		return false
	}
	return is_generic_type(short) && short !in g.tc.structs && short !in g.tc.interface_names
}

fn (g &FlatGen) find_prim_method(method string) string {
	if 'u8.${method}' in g.tc.fn_param_types {
		return c_name('u8.${method}')
	}
	if 'int.${method}' in g.tc.fn_param_types {
		return c_name('int.${method}')
	}
	if 'i64.${method}' in g.tc.fn_param_types {
		return c_name('i64.${method}')
	}
	if 'u32.${method}' in g.tc.fn_param_types {
		return c_name('u32.${method}')
	}
	if 'u64.${method}' in g.tc.fn_param_types {
		return c_name('u64.${method}')
	}
	return ''
}

fn (g &FlatGen) find_alias_method(target string, method string) ?string {
	mut fallback := ''
	for alias, alias_target in g.tc.type_aliases {
		if alias_target != target {
			continue
		}
		alias_method := '${alias}.${method}'
		if alias_method !in g.tc.fn_param_types {
			if alias.contains('.') {
				short_method := '${alias.all_after_last('.')}.${method}'
				if short_method in g.tc.fn_param_types {
					return alias_method
				}
			}
			continue
		}
		if alias.contains('.') {
			return alias_method
		}
		if fallback.len == 0 {
			fallback = alias_method
		}
	}
	if fallback.len > 0 {
		return fallback
	}
	return none
}

fn (mut g FlatGen) gen_sum_variant_arg(arg_id flat.NodeId, expected types.Type) bool {
	actual0 := types.unwrap_pointer(g.tc.resolve_type(arg_id))
	mut actual := actual0
	if actual0 is types.Alias {
		actual = actual0.base_type
	}
	expected0 := expected
	mut expected_type := expected0
	if expected0 is types.Alias {
		expected_type = expected0.base_type
	}
	if expected_type is types.SumType {
		return false
	}
	if actual !is types.SumType {
		return false
	}
	sum_type := actual as types.SumType
	sum_name := sum_type.name
	variant := g.resolve_variant(sum_name, expected_type.name())
	variants := g.tc.sum_types[sum_name] or { return false }
	if variant !in variants {
		return false
	}
	is_ptr_arg := g.tc.resolve_type(arg_id) is types.Pointer
	is_ref_variant := g.variant_references_sum(variant, sum_name)
	if is_ref_variant {
		g.write('(*')
	}
	g.gen_expr(arg_id)
	if is_ptr_arg {
		g.write('->')
	} else {
		g.write('.')
	}
	g.write(g.sum_field_name(variant))
	if is_ref_variant {
		g.write(')')
	}
	return true
}

fn (mut g FlatGen) forward_decls() {
	mut cur_module := ''
	mut forwarded := map[string]bool{}
	for i in 0 .. g.a.nodes.len {
		node := g.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_module = ''
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_module = cur_module
			continue
		}

		is_entry_main := node.value == 'main' && (cur_module.len == 0 || cur_module == 'main')
		if kind_id == 61 && !is_entry_main {
			if !g.should_emit_fn_node_in_module(node, i, cur_module) {
				continue
			}
			qfn := qualified_fn_name_in_module(cur_module, node.value)
			if forwarded[qfn] {
				continue
			}
			forwarded[qfn] = true
			g.tc.cur_module = cur_module
			ret_type := g.tc.parse_type(node.typ)
			g.write(g.optional_type_name(ret_type))
			g.write(' ')
			g.write(qfn)
			g.write('(')
			g.write_fn_node_params(node)
			g.writeln(');')
		} else if kind_id == 76
			&& (node.value.starts_with('C.v_filelock_') || node.value.starts_with('v_filelock_')) {
			g.tc.cur_module = cur_module
			ret_type := g.tc.parse_type(node.typ)
			cfn := c_name(node.value)
			if forwarded[cfn] {
				continue
			}
			forwarded[cfn] = true
			g.write(g.optional_type_name(ret_type))
			g.write(' ')
			g.write(cfn)
			g.write('(')
			g.write_c_fn_node_params(node)
			g.writeln(');')
		}
	}
	g.writeln('')
}

fn (mut g FlatGen) insert_cur_implicit_veb_ctx_param(node flat.Node) {
	if !g.fn_needs_implicit_veb_ctx(node) {
		return
	}
	insert_idx := g.fn_implicit_veb_ctx_insert_index(node)
	ctx_type := g.implicit_veb_ctx_type()
	mut names := []string{cap: g.cur_param_names.len + 1}
	mut type_values := []types.Type{cap: g.cur_param_type_values.len + 1}
	for i, name in g.cur_param_names {
		if i == insert_idx {
			names << 'ctx'
			type_values << ctx_type
		}
		names << name
		type_values << g.cur_param_type_values[i]
	}
	if insert_idx >= g.cur_param_names.len {
		names << 'ctx'
		type_values << ctx_type
	}
	g.cur_param_names = names
	g.cur_param_type_values = type_values
	g.cur_param_types['ctx'] = ctx_type
	g.tc.cur_scope.insert('ctx', ctx_type)
}

fn (mut g FlatGen) fn_param_types_with_implicit_veb_ctx(node flat.Node, params []types.Type) []types.Type {
	if !g.fn_needs_implicit_veb_ctx(node) {
		return params
	}
	insert_idx := g.fn_implicit_veb_ctx_insert_index(node)
	ctx_type := g.implicit_veb_ctx_type()
	mut result := []types.Type{cap: params.len + 1}
	for i, param in params {
		if i == insert_idx {
			result << ctx_type
		}
		result << param
	}
	if insert_idx >= params.len {
		result << ctx_type
	}
	return result
}

fn (mut g FlatGen) fn_needs_implicit_veb_ctx(node flat.Node) bool {
	return g.fn_returns_veb_result(node) && g.fn_has_receiver_param(node)
		&& !g.fn_receiver_type_is_context(node) && !g.fn_has_param(node, 'ctx')
}

fn (mut g FlatGen) fn_returns_veb_result(node flat.Node) bool {
	if node.typ == 'veb.Result' {
		return true
	}
	ret := g.tc.parse_type(node.typ)
	return ret.name() == 'veb.Result'
}

fn (g &FlatGen) fn_has_param(node flat.Node, name string) bool {
	for i in 0 .. node.children_count {
		p := g.a.child_node(&node, i)
		if p.kind == .param && p.value == name {
			return true
		}
	}
	return false
}

fn (g &FlatGen) fn_implicit_veb_ctx_insert_index(node flat.Node) int {
	if g.fn_has_receiver_param(node) {
		return 1
	}
	return 0
}

fn (g &FlatGen) fn_has_receiver_param(node flat.Node) bool {
	if !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	first := g.a.child_node(&node, 0)
	if first.kind != .param || first.typ.len == 0 {
		return false
	}
	receiver := node.value.all_before_last('.').all_after_last('.')
	param_type := first.typ.trim_left('&').all_after_last('.')
	return receiver == param_type
}

fn (g &FlatGen) fn_receiver_type_is_context(node flat.Node) bool {
	if !g.fn_has_receiver_param(node) {
		return false
	}
	first := g.a.child_node(&node, 0)
	return first.typ.trim_left('&').all_after_last('.') == 'Context'
}

fn (mut g FlatGen) implicit_veb_ctx_type() types.Type {
	return g.tc.parse_type('mut Context')
}

fn (mut g FlatGen) write_fn_node_params(node flat.Node) {
	mut params_len := 0
	for i in 0 .. node.children_count {
		if g.a.child_node(&node, i).kind == .param {
			params_len++
		}
	}
	needs_implicit_ctx := g.fn_needs_implicit_veb_ctx(node)
	if needs_implicit_ctx {
		params_len++
	}
	if params_len == 0 {
		g.write('void')
		return
	}
	mut written := 0
	mut implicit_ctx_written := false
	insert_implicit_ctx_after_first := needs_implicit_ctx && g.fn_has_receiver_param(node)
	for i in 0 .. node.children_count {
		param_id := g.a.child(&node, i)
		p := g.a.node(param_id)
		if p.kind != .param {
			continue
		}
		pt := g.tc.parse_type(p.typ)
		ct := if pt is types.OptionType || pt is types.ResultType {
			g.optional_type_name(pt)
		} else {
			g.tc.c_type(pt)
		}
		if ct.starts_with('fn_ptr:') {
			g.write(g.resolve_fn_ptr_type(ct))
		} else {
			g.write(ct)
		}
		if p.value.len > 0 {
			g.write(' ')
			param_name := if p.value == '_' { '_${written}' } else { c_name(p.value) }
			g.write(param_name)
		}
		written++
		if insert_implicit_ctx_after_first && !implicit_ctx_written {
			if written < params_len {
				g.write(', ')
			}
			g.write_implicit_veb_ctx_param()
			written++
			implicit_ctx_written = true
		}
		if written < params_len {
			g.write(', ')
		}
	}
	if needs_implicit_ctx && !implicit_ctx_written {
		g.write_implicit_veb_ctx_param()
	}
}

fn (mut g FlatGen) write_implicit_veb_ctx_param() {
	pt := g.implicit_veb_ctx_type()
	g.write(g.tc.c_type(pt))
	g.write(' ctx')
}

fn (mut g FlatGen) write_c_fn_node_params(node flat.Node) {
	if node.children_count == 0 {
		g.write('void')
		return
	}
	mut written := 0
	for i in 0 .. node.children_count {
		param_id := g.a.child(&node, i)
		p := g.a.node(param_id)
		if p.kind != .param {
			continue
		}
		raw_typ := if p.typ.len > 0 { p.typ } else { p.value }
		if raw_typ.len == 0 {
			continue
		}
		pt := g.tc.parse_type(raw_typ)
		ct := if pt is types.OptionType || pt is types.ResultType {
			g.optional_type_name(pt)
		} else {
			g.tc.c_type(pt)
		}
		if written > 0 {
			g.write(', ')
		}
		if ct.starts_with('fn_ptr:') {
			g.write(g.resolve_fn_ptr_type(ct))
		} else {
			g.write(ct)
		}
		if p.typ.len > 0 && p.value.len > 0 {
			g.write(' ')
			param_name := if p.value == '_' { '_${written}' } else { c_name(p.value) }
			g.write(param_name)
		}
		written++
	}
	if written == 0 {
		g.write('void')
	}
}

fn (mut g FlatGen) fn_ptr_typedefs() {
	mut emitted := map[string]bool{}
	for {
		mut pending_encoded := []string{}
		mut pending_name := []string{}
		for encoded, name in g.fn_ptr_types {
			if emitted[encoded] {
				continue
			}
			pending_encoded << encoded
			pending_name << name
		}
		if pending_encoded.len == 0 {
			break
		}
		for i in 0 .. pending_encoded.len {
			g.emit_fn_ptr_typedef(pending_encoded[i], pending_name[i], mut emitted)
		}
	}
	if emitted.len > 0 {
		g.writeln('')
	}
}

fn (mut g FlatGen) emit_fn_ptr_typedef(encoded string, name string, mut emitted map[string]bool) {
	if emitted[encoded] {
		return
	}
	emitted[encoded] = true
	ret, params := fn_ptr_typedef_parts(encoded)
	ret_ct := g.fn_ptr_typedef_type(ret, mut emitted)
	params_ct := g.fn_ptr_typedef_params(params, mut emitted)
	g.writeln('typedef ${ret_ct} (*${name})(${params_ct});')
}

fn fn_ptr_typedef_parts(encoded string) (string, string) {
	payload := if encoded.starts_with('fn_ptr:') { encoded['fn_ptr:'.len..] } else { encoded }
	if payload.starts_with('fn_ptr:') {
		first_pipe_idx := payload.index('|') or { return payload, 'void' }
		rest := payload[first_pipe_idx + 1..]
		second_pipe_idx := rest.index('|') or { return payload, 'void' }
		split_idx := first_pipe_idx + 1 + second_pipe_idx
		return payload[..split_idx], payload[split_idx + 1..]
	}
	pipe_idx := payload.index('|') or { return payload, 'void' }
	return payload[..pipe_idx], payload[pipe_idx + 1..]
}

fn (mut g FlatGen) fn_ptr_typedef_params(params string, mut emitted map[string]bool) string {
	clean := params.trim_space()
	if clean.len == 0 || clean == 'void' {
		return 'void'
	}
	mut out := []string{}
	for param in clean.split(',') {
		out << g.fn_ptr_typedef_type(param, mut emitted)
	}
	return out.join(', ')
}

fn (mut g FlatGen) fn_ptr_typedef_type(typ string, mut emitted map[string]bool) string {
	mut clean := typ.trim_space()
	if clean.len == 0 {
		return 'void'
	}
	if clean.starts_with('fn_ptr:') {
		clean = fn_ptr_typedef_normalized(clean)
		name := g.resolve_fn_ptr_type(clean)
		g.emit_fn_ptr_typedef(clean, name, mut emitted)
		return name
	}
	if clean == 'Optional' {
		return 'struct Optional'
	}
	if fn_ptr_typedef_is_generic_placeholder(clean) {
		return 'int'
	}
	return clean
}

fn fn_ptr_typedef_normalized(typ string) string {
	clean := typ.trim_space()
	if !clean.starts_with('fn_ptr:') {
		return clean
	}
	payload := clean['fn_ptr:'.len..]
	if payload.contains('|') {
		return clean
	}
	return 'fn_ptr:${payload}|void'
}

fn fn_ptr_typedef_is_generic_placeholder(typ string) bool {
	mut clean := typ.trim_space()
	for clean.ends_with('*') {
		clean = clean[..clean.len - 1].trim_space()
	}
	if clean.starts_with('struct ') {
		clean = clean['struct '.len..].trim_space()
	}
	short := if clean.contains('__') {
		clean.all_after_last('__')
	} else if clean.contains('.') {
		clean.all_after_last('.')
	} else {
		clean
	}
	if short in ['T', 'U', 'V', 'K', 'R'] {
		return true
	}
	return short.len == 1 && short[0] >= `A` && short[0] <= `Z`
}

fn (mut g FlatGen) multi_return_typedefs() {
	mut emitted := map[string]bool{}
	for _, ret in g.tc.fn_ret_types {
		g.emit_multi_return_typedef(ret, mut emitted)
	}
	mut cur_module := ''
	for node in g.a.nodes {
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_module = ''
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_module = cur_module
			continue
		}
		g.tc.cur_module = cur_module
		if node.typ.len > 0 {
			g.emit_multi_return_typedef(g.tc.parse_type(node.typ), mut emitted)
		}
	}
	if emitted.len > 0 {
		g.writeln('')
	}
}

fn (mut g FlatGen) emit_multi_return_typedef(ret types.Type, mut emitted map[string]bool) {
	if ret is types.OptionType {
		g.emit_multi_return_typedef(ret.base_type, mut emitted)
		return
	}
	if ret is types.ResultType {
		g.emit_multi_return_typedef(ret.base_type, mut emitted)
		return
	}
	if ret is types.MultiReturn {
		name := g.tc.c_type(ret)
		if name in emitted {
			return
		}
		emitted[name] = true
		g.writeln('typedef struct {')
		for i, typ in ret.types {
			g.writeln('\t${g.tc.c_type(typ)} arg${i};')
		}
		g.writeln('} ${name};')
	}
}

fn (mut g FlatGen) resolve_fn_ptr_type(typ string) string {
	if typ in g.fn_ptr_types {
		return g.fn_ptr_types[typ]
	}
	name := '_fn_ptr_${g.fn_ptr_types.len}'
	g.fn_ptr_types[typ] = name
	return name
}
