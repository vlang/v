module canvas

// Translate the closed set of Graphite-generated canvas shaders after paint
// and clip assembly, keeping one source for the geometry and paint formulas.
// This is not a general MSL compiler: unsupported interfaces and layouts fail.
struct GlslShaderField {
	typ        string
	name       string
	annotation string
}

fn glsl_matrix_templates(text string) !string {
	mut source := text
	for {
		start := source.index('template ') or { break }
		brace := source.index_after('{', start) or { return error('gg.canvas: unterminated shader matrix helper') }
		end := glsl_matching_delimiter(source, brace, `{`, `}`)!
		mut helper := source[start..end + 1]
		if !helper.starts_with('template <typename T>matrix<T, 2, 2> mat2_inverse(')
			&& !helper.starts_with('template <typename T>matrix<T, 3, 3> mat3_inverse(') {
			return error('gg.canvas: unsupported shader template')
		}
		helper = helper.replace('template <typename T>', '')
		helper = helper.replace('matrix<T, 2, 2>', 'mat2').replace('matrix<T, 3, 3>', 'mat3')
		helper = replace_identifier(helper, 'T', 'float')
		source = source[..start] + helper + source[end + 1..]
	}
	return source
}

// Metal permits an assignment expression as a reference argument. GLSL
// requires an l-value, so sequence the rrect helpers' input copy before the
// call. The existing comma expression still copies the updated value back.
fn glsl_reference_assignments(text string) !string {
	mut lines := []string{}
	for line in text.split_into_lines() {
		if !line.starts_with('((K_Qvf2f22f2f2f2f2(')
			&& !line.starts_with('((L_Qvf2f22f2f4f4f4(') {
			lines << line
			continue
		}
		start := line.index('(_skTemp') or {
			return error('gg.canvas: unsupported shader reference argument')
		}
		end := glsl_matching_delimiter(line, start, `(`, `)`)!
		assignment := line[start + 1..end]
		name := assignment.all_before('=').trim_space()
		if !assignment.contains(' = ') || !name.bytes().all(is_identifier_byte(it)) {
			return error('gg.canvas: unsupported shader reference assignment')
		}
		lines << '${assignment};'
		lines << line[..start] + name + line[end + 1..]
	}
	return lines.join('\n')
}

fn glsl_matching_delimiter(text string, start int, open u8, close u8) !int {
	mut depth := 0
	for i := start; i < text.len; i++ {
		if text[i] == open {
			depth++
		} else if text[i] == close {
			depth--
			if depth == 0 { return i }
		}
	}
	return error('gg.canvas: unbalanced shader expression')
}

// MSL comparisons return boolean vectors; GLSL spells them with relational
// builtins. The embedded programs use these only in all/any and select.
fn glsl_vector_comparisons(text string) !string {
	mut source := text
	for function in ['all', 'any', 'select'] {
		mut offset := 0
		for {
			start := source.index_after('${function}(', offset) or { break }
			open := start + function.len
			end := glsl_matching_delimiter(source, open, `(`, `)`)!
			argument := source[open + 1..end]
			mut replacement := ''
			if function == 'select' {
				args := glsl_split_arguments(argument)!
				if args.len != 3 { return error('gg.canvas: unsupported shader select') }
				replacement = 'mix(${args[0]}, ${args[1]}, ${glsl_vector_relation(args[2])!})'
			} else {
				replacement = '${function}(${glsl_vector_relation(argument)!})'
			}
			source = source[..start] + replacement + source[end + 1..]
			offset = start + replacement.len
		}
	}
	return source
}

fn glsl_split_arguments(text string) ![]string {
	mut args := []string{}
	mut start := 0
	mut depth := 0
	for i, c in text {
		if c == `(` || c == `[` {
			depth++
		} else if c == `)` || c == `]` {
			depth--
		} else if c == `,` && depth == 0 {
			args << text[start..i].trim_space()
			start = i + 1
		}
	}
	if depth != 0 { return error('gg.canvas: unbalanced shader arguments') }
	args << text[start..].trim_space()
	return args
}

fn glsl_vector_relation(text string) !string {
	mut expr := text.trim_space()
	for expr.starts_with('(') {
		end := glsl_matching_delimiter(expr, 0, `(`, `)`)!
		if end != expr.len - 1 { break }
		expr = expr[1..expr.len - 1].trim_space()
	}
	mut depth := 0
	for i, c in expr {
		if c == `(` || c == `[` {
			depth++
		} else if c == `)` || c == `]` {
			depth--
		} else if depth == 0 {
			for pair in [['==', 'equal'], ['!=', 'notEqual'], ['<=', 'lessThanEqual'],
				['>=', 'greaterThanEqual'], ['<', 'lessThan'], ['>', 'greaterThan']] {
				if has_at(expr, i, pair[0]) {
					left := expr[..i].trim_space()
					right := expr[i + pair[0].len..].trim_space()
					if left.len == 0 || right.len == 0 { break }
					return '${pair[1]}(${left}, ${right})'
				}
			}
		}
	}
	return error('gg.canvas: unsupported shader vector comparison `${text}`')
}

const glsl_math_helpers = '
float saturate(float x) { return clamp(x, 0.0, 1.0); }
vec2 saturate(vec2 x) { return clamp(x, 0.0, 1.0); }
vec3 saturate(vec3 x) { return clamp(x, 0.0, 1.0); }
vec4 saturate(vec4 x) { return clamp(x, 0.0, 1.0); }
'

const glsl_sample_helper = '
vec4 canvas_sample(sampler2D image, vec2 p) { return texture(image, p, -0.475); }
'

// The GL depth/stencil-only pipelines require a fragment stage; their color
// write masks remain disabled.
const glsl_depth_fragment_source = '#version 430 core\nvoid main() {}\n'

fn glsl_load_msaa_sources() (string, string) {
	return '#version 430 core
void main() {
    vec2 position = vec2(float(gl_VertexID >> 1), float(gl_VertexID & 1));
    gl_Position = vec4(2.0 * position - 1.0, 0.0, 1.0);
}
', '#version 430 core
uniform sampler2D canvas_texture;
layout(location = 0) out vec4 canvas_fragment_color;
void main() {
    canvas_fragment_color = texelFetch(canvas_texture, ivec2(gl_FragCoord.xy), 0);
}
'
}

fn glsl_struct_fields(source string, name string) ![]GlslShaderField {
	marker := 'struct ${name} {'
	start := source.index(marker) or { return []GlslShaderField{} }
	end := source.index_after('};', start + marker.len) or { return error('gg.canvas: unterminated shader structure ${name}') }
	mut fields := []GlslShaderField{}
	for line in source[start + marker.len..end].split_into_lines() {
		text := line.trim_space()
		if text.len == 0 { continue }
		if !text.ends_with(';') { return error('gg.canvas: unsupported shader field `${text}`') }
		declaration := text.all_before('[[').trim_right('; ').trim_space()
		field_name := declaration.all_after_last(' ')
		typ := declaration.all_before_last(' ').trim_space()
		if field_name.len == 0 || typ.len == 0 || field_name == declaration || field_name.contains_any('[]{}*&') {
			return error('gg.canvas: unsupported shader field `${text}`')
		}
		annotation := if text.contains('[[') { text.all_after('[[').all_before(']]') } else { '' }
		fields << GlslShaderField{typ, field_name, annotation}
	}
	return fields
}

fn glsl_remove_struct(source string, name string) !string {
	marker := 'struct ${name} {'
	start := source.index(marker) or { return source }
	end := source.index_after('};', start + marker.len) or { return error('gg.canvas: unterminated shader structure ${name}') }
	return source[..start] + source[end + 2..]
}

// Calculate the std430 array stride. These field alignments and sizes also
// match Metal; reject any type whose storage representation is not checked.
fn glsl_paint_uniform_size(msl string) !int {
	fields := glsl_struct_fields(msl, 'CombinedUniformData')!
	mut size := 0
	mut alignment := 1
	for field in fields {
		if field.annotation != '' {
			return error('gg.canvas: unsupported storage field annotation ${field.annotation}')
		}
		mut typ := field.typ
		mut count := 1
		if typ.starts_with('array<') {
			element, n := glsl_array_type(typ)!
			typ = element
			count = n
			if typ != 'float4' {
				return error('gg.canvas: unsupported storage array element ${typ}')
			}
		}
		field_alignment, field_size := match typ {
			'float', 'int', 'uint' { 4, 4 }
			'float2' { 8, 8 }
			'float4' { 16, 16 }
			'float4x4' { 16, 64 }
			else { return error('gg.canvas: unsupported storage field type ${typ}') }
		}
		size = (size + field_alignment - 1) / field_alignment * field_alignment
		size += field_size * count
		if field_alignment > alignment { alignment = field_alignment }
	}
	return (size + alignment - 1) / alignment * alignment
}

fn glsl_interface_field(field GlslShaderField, vertex bool, input bool) !(string, string) {
	if field.typ !in ['float', 'float2', 'float3', 'float4', 'uint'] {
		return error('gg.canvas: unsupported shader interface type ${field.typ}')
	}
	a := field.annotation
	if vertex && input && a.starts_with('attribute(') && a.ends_with(')') {
		location := a['attribute('.len..a.len - 1]
		glsl_location(location)!
		name := 'canvas_attribute_${field.name}'
		return 'layout(location = ${location}) in ${field.typ} ${name};\n', name
	}
	if vertex && !input && a == 'position' { return '', 'gl_Position' }
	if vertex && !input && a == 'point_size' { return '', 'gl_PointSize' }
	if !vertex && !input && a == 'color(0)' {
		return 'layout(location = 0) out ${field.typ} canvas_fragment_color;\n', 'canvas_fragment_color'
	}
	if vertex != input && a.starts_with('user(locn') {
		location := a['user(locn'.len..].all_before(')')
		glsl_location(location)!
		qualifier := a.all_after(')').trim_space()
		interpolation := match qualifier {
			'' { '' }
			'flat' { 'flat ' }
			'center_no_perspective' { 'noperspective ' }
			else { return error('gg.canvas: unsupported shader interpolation ${qualifier}') }
		}
		direction := if input { 'in' } else { 'out' }
		name := 'canvas_varying_${field.name}'
		return 'layout(location = ${location}) ${interpolation}${direction} ${field.typ} ${name};\n', name
	}
	return error('gg.canvas: unsupported shader interface annotation `${a}`')
}

fn glsl_location(location string) ! {
	if location.len == 0 || !location.bytes().all(it.is_digit()) {
		return error('gg.canvas: unsupported shader interface location `${location}`')
	}
}

fn glsl_array_type(typ string) !(string, int) {
	if !typ.starts_with('array<') || !typ.ends_with('>') {
		return error('gg.canvas: unsupported shader array `${typ}`')
	}
	parts := typ['array<'.len..typ.len - 1].split(',')
	if parts.len != 2 { return error('gg.canvas: unsupported shader array `${typ}`') }
	count := parts[1].trim_space()
	if count.len == 0 || !count.bytes().all(it.is_digit()) || count.int() <= 0 {
		return error('gg.canvas: unsupported shader array size `${count}`')
	}
	return parts[0].trim_space(), count.int()
}

fn glsl_array_declarations(text string) !string {
	mut source := text
	for {
		start := source.index('array<') or { break }
		end := source.index_after('>', start) or { return error('gg.canvas: unterminated shader array') }
		typ, count := glsl_array_type(source[start..end + 1])!
		mut name_start := end + 1
		for name_start < source.len && source[name_start].is_space() { name_start++ }
		mut name_end := name_start
		for name_end < source.len && is_identifier_byte(source[name_end]) { name_end++ }
		if name_start == name_end {
			return error('gg.canvas: unsupported shader array declaration')
		}
		name := source[name_start..name_end]
		source = source[..start] + '${typ} ${name}[${count}]' + source[name_end..]
	}
	return source
}

fn glsl_shader_source(msl string, vertex bool) !string {
	inputs := glsl_struct_fields(msl, 'Inputs')!
	outputs := glsl_struct_fields(msl, 'Outputs')!
	fields := glsl_struct_fields(msl, 'CombinedUniformData')!
	mut source := msl
	if source.contains('struct sampler2D {') {
		source = source.replace(gradient_snippets()['preamble'], '')
	}
	for name in ['Inputs', 'Outputs', 'CombinedUniformData', 'CombinedUniforms', 'IntrinsicUniforms',
		'Globals'] {
		source = glsl_remove_struct(source, name)!
	}
	mut declarations := '#version 430 core\nuniform vec4 viewport;\nuniform vec4 dstReadBounds;\n'
	paint_instance := if vertex { 'canvas_vertex_paint' } else { 'canvas_fragment_paint' }
	if fields.len > 0 {
		glsl_paint_uniform_size(msl)!
		declarations += 'struct CombinedUniformData {\n'
		for field in fields { declarations += '${field.typ} ${field.name};\n' }
		block := if vertex { 'CanvasVertexPaint' } else { 'CanvasFragmentPaint' }
		binding := if vertex { 0 } else { 8 }
		declarations += '};\nlayout(std430, binding = ${binding}) readonly buffer ${block} {\nCombinedUniformData combinedUniformData[];\n} ${paint_instance};\n'
	}
	for field in inputs {
		declaration, name := glsl_interface_field(field, vertex, true)!
		declarations += declaration
		source = replace_identifier(source, '_in.${field.name}', name)
	}
	for field in outputs {
		declaration, name := glsl_interface_field(field, vertex, false)!
		declarations += declaration
		source = replace_identifier(source, '_out.${field.name}', name)
	}
	if source.contains('_globals.ditherLUT_') || source.contains('_globals.image_4') {
		declarations += 'uniform sampler2D canvas_texture;\n'
		declarations += glsl_sample_helper
		for name in ['ditherLUT_6', 'ditherLUT_9', 'image_4'] {
			source = source.replace('_globals.${name}', 'canvas_texture')
		}
	}
	for index in 0 .. 2 {
		for name in ['viewport', 'dstReadBounds'] {
			source = source.replace('_globals._anonInterface${index}->${name}', name)
		}
	}
	source = source.replace('_globals._anonInterface0->combinedUniformData', '${paint_instance}.combinedUniformData')
	source = source.replace('_globals.uniformSsboIndex', 'canvas_uniform_index')
	entry := if vertex { 'vertex Outputs vertexMain(' } else { 'fragment Outputs fragmentMain(' }
	start := source.index(entry) or { return error('gg.canvas: unsupported GLSL shader entry point') }
	body := source.index_after('{', start) or { return error('gg.canvas: missing GLSL shader entry body') }
	source = source[..start] + 'void main() {\nuint canvas_uniform_index = 0u;\n' + source[body + 1..]
	mut lines := []string{}
	for line in source.split_into_lines() {
		t := line.trim_space()
		if t in ['#include <metal_stdlib>', '#include <simd/simd.h>', '#ifdef __clang__', '#endif']
			|| t == '#pragma clang diagnostic ignored "-Wall"' || t == 'using namespace metal;'
			|| t.starts_with('Globals _globals{') || t == '(void)_globals;'
			|| t == 'Outputs _out;' || t == '(void)_out;' {
			continue
		}
		lines << line
	}
	source = lines.join('\n')
	source = replace_identifier(source, 'sk_VertexID', 'uint(gl_VertexID)')
	source = replace_identifier(source, 'sk_InstanceID', 'uint(gl_InstanceID)')
	source = replace_identifier(source, '_frontFacing', 'gl_FrontFacing')
	source = replace_identifier(source, '_fragCoord', 'gl_FragCoord')
	if vertex {
		// Metal clip depth [0,w] becomes OpenGL [-w,w]. The GL surface uses
		// positive y scale, so logical canvas row zero is GL texture row zero.
		source = source.replace('return _out;', 'gl_Position.z = 2.0 * gl_Position.z - gl_Position.w;\nreturn;')
	} else {
		source = source.replace('return _out;', 'return;')
	}
	source = glsl_reference_assignments(source)!
	source = glsl_matrix_templates(source)!
	source = glsl_array_declarations(declarations + glsl_math_helpers + source)!
	for names in [['float2x2', 'mat2'], ['float3x3', 'mat3'], ['float4x4', 'mat4'], ['float2',
		'vec2'], ['float3', 'vec3'], ['float4', 'vec4'], ['uint2', 'uvec2'], ['uint3', 'uvec3'],
		['uint4', 'uvec4'], ['rsqrt', 'inversesqrt'], ['powr', 'pow'], ['sample', 'canvas_sample']] {
		source = replace_identifier(source, names[0], names[1])
	}
	source = source.replace('thread vec2&', 'inout vec2').replace('thread vec4&', 'inout vec4')
	source = glsl_vector_comparisons(source)!
	for marker in ['[[', ']]', '->', 'template ', 'thread ', 'device ', 'constant ', 'texture2d<',
		'sampler ', 'using namespace', '#include', '(void)', 'array<', '_globals', '_anonInterface',
		'return _out', 'Inputs ', 'Outputs '] {
		if source.contains(marker) {
			return error('gg.canvas: unsupported Metal shader construct `${marker}`')
		}
	}
	return source
}
