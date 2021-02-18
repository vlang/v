//------------------------------------------------------------------------------
//  Shader code for texcube-sapp sample.
//
//  NOTE: This source file also uses the '#pragma sokol' form of the
//  custom tags.
//------------------------------------------------------------------------------
//#pragma sokol @ctype mat4 hmm_mat4

#pragma sokol @vs vs_i
uniform vs_params_i {
    mat4 mvp;
};

in vec4 pos;
in vec4 color0;
in vec2 texcoord0;

in vec4 inst_pos;

out vec4 color;
out vec4 color_inst;
out vec2 uv;

const vec4 palette[10] = vec4[10](
	vec4(1,0,0,1),
	vec4(0,1,0,1),
	vec4(0,0,1,1),
	vec4(1,1,0,1),
	vec4(0,1,1,1),
	vec4(1,1,1,1),
	vec4(0,0,0,1),
	vec4(0.2,0.2,0.2,1),
	vec4(0.3,0.3,0.3,1),
	vec4(0.9,0.9,0.9,1)
);

void main() {
		vec4 delta_pos = vec4(inst_pos.xyz,0);
		float w = inst_pos.w;
		color_inst = palette[int(w)];
    gl_Position = mvp * (pos + delta_pos);
    color = color0;
    uv = texcoord0/4;
}
#pragma sokol @end

#pragma sokol @fs fs_i
uniform sampler2D tex;

in vec4 color;
in vec4 color_inst;
in vec2 uv;
out vec4 frag_color;

void main() {
	vec4 c = color;
	vec4 txt = texture(tex, uv);
	c = txt * c * color_inst;	
	frag_color = c ;
}

#pragma sokol @end

#pragma sokol @program instancing vs_i fs_i
