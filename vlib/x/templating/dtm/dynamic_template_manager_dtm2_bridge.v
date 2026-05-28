module dtm

import x.templating.dtm2

// new_dtm2_render_engine is the compatibility boundary between the historical
// DTM API and the DTM2 runtime template engine. DTM2 remains unaware
// of legacy cache concepts; it only parses templates and renders them.
fn new_dtm2_render_engine(template_dir string, compress_html bool) &dtm2.Manager {
	return dtm2.initialize(
		template_dir:              template_dir
		compress_html:             compress_html
		reload_modified_templates: true
	)
}

fn (mut tm DynamicTemplateManager) expand_with_dtm2_render_engine(tmpl_path string, tmpl_var TemplateCacheParams) string {
	validate_legacy_placeholder_sizes(tmpl_path, tmpl_var.placeholders) or {
		eprintln(err.msg())
		return internat_server_error
	}
	if isnil(tm.render_engine) {
		tm.render_engine = new_dtm2_render_engine(tm.template_folder, tm.compress_html)
	}
	placeholders := convert_legacy_placeholders(tmpl_var.placeholders)
	return tm.render_engine.expand(tmpl_path,
		placeholders:               &placeholders
		missing_placeholder_prefix: '$'
	)
}

fn validate_legacy_placeholder_sizes(tmpl_path string, placeholders &map[string]DtmMultiTypeMap) ! {
	for key, value in placeholders {
		if key.len > max_placeholders_key_size {
			return error('${message_signature_error} Length of placeholder key "${key}" exceeds the maximum allowed size for template content in file: ${tmpl_path}. Max allowed size: ${max_placeholders_key_size} characters.')
		}
		casted_value := legacy_placeholder_value_to_string(*value)
		if casted_value.len > max_placeholders_value_size {
			return error('${message_signature_error} Length of placeholder value for key "${key}" exceeds the maximum allowed size for template content in file: ${tmpl_path}. Max allowed size: ${max_placeholders_value_size} characters.')
		}
	}
}

fn convert_legacy_placeholders(placeholders &map[string]DtmMultiTypeMap) map[string]string {
	mut converted := map[string]string{}
	for key, value in placeholders {
		converted[key.clone()] = legacy_placeholder_value_to_string(*value).clone()
	}
	return converted
}

fn legacy_placeholder_value_to_string(value DtmMultiTypeMap) string {
	return match value {
		f32 { value.str() }
		f64 { value.str() }
		i16 { value.str() }
		i64 { value.str() }
		i8 { value.str() }
		int { value.str() }
		string { value.clone() }
		u16 { value.str() }
		u32 { value.str() }
		u64 { value.str() }
		u8 { value.str() }
	}
}
