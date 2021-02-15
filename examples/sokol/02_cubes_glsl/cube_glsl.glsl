//------------------------------------------------------------------------------
//  Shader code for texcube-sapp sample.
//
//  NOTE: This source file also uses the '#pragma sokol' form of the
//  custom tags.
//------------------------------------------------------------------------------
//#pragma sokol @ctype mat4 my_mat4

#pragma sokol @vs vs
uniform vs_params {
    mat4 mvp;
};

in vec4 pos;
in vec4 color0;
in vec2 texcoord0;

out vec4 color;
out vec2 uv;

void main() {
    gl_Position = mvp * pos;
    color = color0;
    uv = texcoord0;
}
#pragma sokol @end

#pragma sokol @fs fs
uniform sampler2D tex;
uniform fs_params {
	vec2 text_res;
	float iTime;
};

in vec4 color;
in vec2 uv;
out vec4 frag_color;

//*********************************************************
// RAY TRACE
// original code from: https://www.shadertoy.com/view/ldS3DW
//*********************************************************
float sphere(vec3 ray, vec3 dir, vec3 center, float radius)
{
 vec3 rc  = ray-center;
 float c  = dot(rc, rc) - (radius*radius);
 float b  = dot(dir, rc);
 float d  = b*b - c;
 float t  = -b - sqrt(abs(d));
 float st = step(0.0, min(t,d));
 return mix(-1.0, t, st);
}

vec3 background(float t, vec3 rd)
{
	vec3 light = normalize(vec3(sin(t), 0.6, cos(t)));
	float sun = max(0.0, dot(rd, light));
	float sky = max(0.0, dot(rd, vec3(0.0, 1.0, 0.0)));
	float ground = max(0.0, -dot(rd, vec3(0.0, 1.0, 0.0)));
	return (pow(sun, 256.0)+0.2*pow(sun, 2.0))*vec3(2.0, 1.6, 1.0) +
					pow(ground, 0.5)*vec3(0.4, 0.3, 0.2) +
					pow(sky, 1.0)*vec3(0.5, 0.6, 0.7);
}

vec4 mainImage(vec2 fragCoord)
{
 vec2 uv = (fragCoord-vec2(0.4,0.4))*2.0;
 
 //vec2 uv  = (-1.0 + 2.0*fc.xy / text_res.xy) * vec2(text_res.x/text_res.y, 1.0);
 vec3 ro    = vec3(0.0, 0.0, -3.0);
 vec3 rd    = normalize(vec3(uv, 1.0));
 vec3 p     = vec3(0.0, 0.0, 0.0);
 float t    = sphere(ro, rd, p, 1.0);
 vec3 nml   = normalize(p - (ro+rd*t));
 vec3 bgCol = background(iTime, rd);
 rd         = reflect(rd, nml);
 vec3 col   = background(iTime, rd) * vec3(0.9, 0.8, 1.0);
 vec4 fragColor = vec4( mix(bgCol, col, step(0.0, t)), 1.0 );
 return fragColor;
}
//*********************************************************
//*********************************************************

void main() {
		vec4 c = color;
    vec4 txt = texture(tex, uv/4.0);
		c = txt * c;
		vec4 col_ray = mainImage(uv);
		float txt_mix = mod(iTime,5);
		frag_color = c*txt_mix*0.1 +	col_ray	;
}

#pragma sokol @end

#pragma sokol @program cube vs fs
