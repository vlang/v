// Pieces of the MSL that Skia Graphite generates for dithered linear and
// radial gradient paints, which gpu_gradient.v combines with the solid-color
// shaders of each render step.
// @@ preamble
struct sampler2D {texture2d<float> tex;sampler smp;};float4 sample(sampler2D i, float2 p, float b=-0.475) { return i.tex.sample(i.smp, p, bias(b)); }float4 sample(sampler2D i, float3 p, float b=-0.475) { return i.tex.sample(i.smp, p.xy / p.z, bias(b)); }float4 sampleLod(sampler2D i, float2 p, float lod) { return i.tex.sample(i.smp, p, level(lod)); }float4 sampleLod(sampler2D i, float3 p, float lod) {return i.tex.sample(i.smp, p.xy / p.z, level(lod));}float4 sampleGrad(sampler2D i, float2 p, float2 dPdx, float2 dPdy) {return i.tex.sample(i.smp, p, gradient2d(dPdx, dPdy));}
// @@ fields_4
float4 upper2x2_2;
float2 translation_2;
array<float4, 4> colors_4;
float4 offsets_4;
int tilemode_4;
int colorSpace_4;
int doUnPremul_4;
float mode_5;
float range_6;
// @@ fields_8
float4 upper2x2_2;
float2 translation_2;
array<float4, 8> colors_4;
array<float4, 2> offsets_4;
int tilemode_4;
int colorSpace_4;
int doUnPremul_4;
float mode_5;
float range_6;
// @@ protos_color
float3 a_Qh3h3(float3 a);
float3 css_hsl_to_srgb_Qh3h3(float3 b);
float3 css_lab_to_xyz_Qh3h3(float3 a);
float3 css_oklab_gamut_map_to_linear_srgb_Qh3h3(float3 a);
float3 css_oklab_to_linear_srgb_Qh3h3(float3 a);
// @@ protos_tile
float4 interpolated_to_rgb_unpremul_Qh4h4ii(float4 a, int b, int c);
float2 v_Qf2if2(int a, float2 b);
// @@ proto_colorize_4
float4 w_Qh4f4f4f2(array<float4, 4> a, float4 b, float2 c);
// @@ proto_colorize_8
float4 x_Qh4f4f4f2(array<float4, 8> a, array<float4, 2> b, float2 c);
// @@ bodies_color
float3 a_Qh3h3(float3 a) {
return float3(a.z, a.y * cos(((a.x) * 0.0174532925)), a.y * sin(((a.x) * 0.0174532925)));
}
float3 css_hsl_to_srgb_Qh3h3(float3 b) {
    float _skTemp0;
    float _skTemp1;
    float3 _skTemp2;
    float _skTemp3;
b.x = (_skTemp0 = b.x, _skTemp1 = 360.0f, _skTemp0 - _skTemp1 * floor(_skTemp0 / _skTemp1));
if (b.x < 0.0f) b.x = b.x + 360.0f;
b.yz = b.yz * 0.01f;
float3 c = (_skTemp2 = float3(0.0f, 8.0f, 4.0f) + b.x * 0.0333333351f, _skTemp3 = 12.0f, _skTemp2 - _skTemp3 * floor(_skTemp2 / _skTemp3));
float d = b.y * min(b.z, 1.0f - b.z);
return b.z - d * clamp(min(c - 3.0f, 9.0f - c), -1.0f, 1.0f);
}
float3 css_lab_to_xyz_Qh3h3(float3 a) {
float3 d;
d.y = (a.x + 16.0f) * 0.00862069f;
d.x = a.y * 0.002f + d.y;
d.z = d.y - a.z * 0.005f;
float3 g = powr(d, float3(3.0f));
float3 h = float3(g.x > 0.008856452f ? g.x : (116.0f * d.x - 16.0f) * 0.00110705639f, a.x > 8.000001f ? g.y : a.x * 0.00110705639f, g.z > 0.008856452f ? g.z : (116.0f * d.z - 16.0f) * 0.00110705639f);
return h * float3(0.9642956f, 1.0f, 0.825104535f);
}
float3 css_oklab_gamut_map_to_linear_srgb_Qh3h3(float3 a) {
const float2 r = float2(-0.014804f, -0.162608f);
const float2 s = float2(-0.276786f, 0.004193f);
float2 z = a.yz;
float A;
float2 B;
float2 C;
if (dot(z, float2(0.409702f, -0.912219f)) < 0.0f) if (dot(z, float2(0.460276f, 0.887776f)) < 0.0f) if (dot(z, float2(-0.171122f, 0.98525f)) < 0.0f) {
A = 0.102047f;
B = r;
C = s;
} else {
A = 0.092029f;
B = float2(-0.038533f, -0.00165f);
C = float2(-0.232572f, -0.094331f);
} else if (dot(z, float2(0.947925f, 0.318495f)) < 0.0f) {
A = 0.081709f;
B = float2(-0.034601f, -0.002215f);
C = float2(0.012185f, 0.338031f);
} else {
A = 0.091132f;
B = float2(0.07037f, 0.034139f);
C = float2(0.01817f, 0.37855f);
} else if (dot(z, float2(-0.9068f, 0.421562f)) < 0.0f) if (dot(z, float2(-0.397919f, -0.917421f)) < 0.0f) {
A = 0.113902f;
B = float2(0.090836f, 0.036251f);
C = float2(0.226781f, 0.018764f);
} else {
A = 0.161739f;
B = float2(-0.008202f, -0.264819f);
C = float2(0.187156f, -0.284304f);
} else {
A = 0.102047f;
B = r;
C = s;
}
float D = 1.0f;
float E = dot(B, z);
if (E > 0.0f) {
float F = 1.0f - a.x;
float G = A * F;
if (G < E) D = min(D, G / E);
}
float F = dot(C, z);
if (F > 0.0f) {
float G = a.x;
float H = A * G;
if (H < F) D = min(D, H / F);
}
a.yz = a.yz * D;
return css_oklab_to_linear_srgb_Qh3h3(a);
}
float3 css_oklab_to_linear_srgb_Qh3h3(float3 a) {
float b = (a.x + 0.396337777f * a.y) + 0.215803757f * a.z;
float c = (a.x - 0.105561346f * a.y) - 0.06385417f * a.z;
float d = (a.x - 0.08948418f * a.y) - 1.29148555f * a.z;
float e = (b * b) * b;
float f = (c * c) * c;
float g = (d * d) * d;
return float3((4.0767417f * e - 3.3077116f * f) + 0.230969936f * g, (-1.268438f * e + 2.60975742f * f) - 0.341319382f * g, (-0.00419608643f * e - 0.7034186f * f) + 1.70761466f * g);
}
// @@ bodies_tile
float4 interpolated_to_rgb_unpremul_Qh4h4ii(float4 a, int b, int c) {
if (bool(c)) switch (b) {
case 2:
case 3:
case 4:
a = float4(a.xyz / max(a.w, 0.0001f), a.w);
break;
case 5:
case 6:
case 7:
case 9:
case 10:
a = float4(a.x, a.yz / max(a.w, 0.0001f), a.w);
break;
}
switch (b) {
case 2:
a.xyz = css_lab_to_xyz_Qh3h3(a.xyz);
break;
case 3:
a.xyz = css_oklab_to_linear_srgb_Qh3h3(a.xyz);
break;
case 4:
a.xyz = css_oklab_gamut_map_to_linear_srgb_Qh3h3(a.xyz);
break;
case 5:
a.xyz = css_lab_to_xyz_Qh3h3(a_Qh3h3(a.xyz));
break;
case 6:
a.xyz = css_oklab_to_linear_srgb_Qh3h3(a_Qh3h3(a.xyz));
break;
case 7:
a.xyz = css_oklab_gamut_map_to_linear_srgb_Qh3h3(a_Qh3h3(a.xyz));
break;
case 9:
a.xyz = css_hsl_to_srgb_Qh3h3(a.xyz);
break;
case 10:
float3 _3_a = a.xyz;
float3 _4_b;
_3_a.yz = _3_a.yz * 0.01f;
if (_3_a.y + _3_a.z >= 1.0f) _4_b = float3(_3_a.y / (_3_a.y + _3_a.z)); else {
_4_b = css_hsl_to_srgb_Qh3h3(float3(_3_a.x, 100.0f, 50.0f));
_4_b *= (1.0f - _3_a.y) - _3_a.z;
_4_b += _3_a.y;
}
a.xyz = _4_b;
break;
}
return a;
}
float2 v_Qf2if2(int a, float2 b) {
switch (a) {
case 0:
b.x = saturate(b.x);
break;
case 1:
b.x = fract(b.x);
break;
case 2:
{
float c = b.x - 1.0;
b.x = (c - 2.0 * floor(c * 0.5)) - 1.0;
if (false) b.x = clamp(b.x, -1.0, 1.0);
b.x = abs(b.x);
break;
}
case 3:
if (b.x < 0.0 || b.x > 1.0) return float2(0.0, -1.0);
break;
}
return b;
}
// @@ body_colorize_4
float4 w_Qh4f4f4f2(array<float4, 4> a, float4 b, float2 c) {
if (c.y < 0.0) return float4(0.0f); else if (c.x <= b.x) return float4(a[0]); else if (c.x < b.y) return float4(mix(a[0], a[1], (c.x - b.x) / (b.y - b.x))); else if (c.x < b.z) return float4(mix(a[1], a[2], (c.x - b.y) / (b.z - b.y))); else if (c.x < b.w) return float4(mix(a[2], a[3], (c.x - b.z) / (b.w - b.z))); else return float4(a[3]);
}
// @@ body_colorize_8
float4 x_Qh4f4f4f2(array<float4, 8> a, array<float4, 2> b, float2 c) {
if (c.y < 0.0) return float4(0.0f); else if (c.x < b[1].x) if (c.x < b[0].z) if (c.x <= b[0].x) return float4(a[0]); else if (c.x < b[0].y) return float4(mix(a[0], a[1], (c.x - b[0].x) / (b[0].y - b[0].x))); else return float4(mix(a[1], a[2], (c.x - b[0].y) / (b[0].z - b[0].y))); else if (c.x < b[0].w) return float4(mix(a[2], a[3], (c.x - b[0].z) / (b[0].w - b[0].z))); else return float4(mix(a[3], a[4], (c.x - b[0].w) / (b[1].x - b[0].w))); else if (c.x < b[1].z) if (c.x < b[1].y) return float4(mix(a[4], a[5], (c.x - b[1].x) / (b[1].y - b[1].x))); else return float4(mix(a[5], a[6], (c.x - b[1].y) / (b[1].z - b[1].y))); else if (c.x < b[1].w) return float4(mix(a[6], a[7], (c.x - b[1].z) / (b[1].w - b[1].z))); else return float4(a[7]);
}
// @@ main_linear
float2 _22_g = float2(_in.LocalMatrix_2_Var.x + 1e-05, 1.0);
_22_g = v_Qf2if2(_globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].tilemode_4, _22_g);
float4 _23_h = w_Qh4f4f4f2(_globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].colors_4, _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].offsets_4, _22_g);
float4 _24_sk_csxform_prealpha;
float4 _25_a = interpolated_to_rgb_unpremul_Qh4h4ii(_23_h, _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].colorSpace_4, _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].doUnPremul_4);
float _26_b = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].mode_5;
if (_26_b < 0.0f) {
_24_sk_csxform_prealpha = float4(_25_a.xyz / max(_25_a.w, 0.0001f), _25_a.w);
} else {
float _27_c = max(_25_a.w, _26_b - 1.0f);
float _28_d = max(_27_c, 1.0f - _26_b);
_24_sk_csxform_prealpha = float4(_25_a.xyz * _28_d, _27_c);
}
float _29_e = sample(_globals.ditherLUT_6, float4(_fragCoord.x, _fragCoord.y, 0.0, _fragCoord.w).xy * 0.125).x - 0.5f;
float4 outColor_1 = float4(clamp(_24_sk_csxform_prealpha.xyz + _29_e * _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].range_6, 0.0f, _24_sk_csxform_prealpha.w), _24_sk_csxform_prealpha.w);
// @@ main_radial
float _25_b = length(_in.LocalMatrix_2_Var);
float2 _26_g = float2(_25_b, 1.0);
_26_g = v_Qf2if2(_globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].tilemode_4, _26_g);
float4 _27_h = w_Qh4f4f4f2(_globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].colors_4, _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].offsets_4, _26_g);
float4 _28_sk_csxform_prealpha;
float4 _29_a = interpolated_to_rgb_unpremul_Qh4h4ii(_27_h, _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].colorSpace_4, _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].doUnPremul_4);
float _30_b = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].mode_5;
if (_30_b < 0.0f) {
_28_sk_csxform_prealpha = float4(_29_a.xyz / max(_29_a.w, 0.0001f), _29_a.w);
} else {
float _31_c = max(_29_a.w, _30_b - 1.0f);
float _32_d = max(_31_c, 1.0f - _30_b);
_28_sk_csxform_prealpha = float4(_29_a.xyz * _32_d, _31_c);
}
float _33_e = sample(_globals.ditherLUT_6, float4(_fragCoord.x, _fragCoord.y, 0.0, _fragCoord.w).xy * 0.125).x - 0.5f;
float4 outColor_1 = float4(clamp(_28_sk_csxform_prealpha.xyz + _33_e * _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].range_6, 0.0f, _28_sk_csxform_prealpha.w), _28_sk_csxform_prealpha.w);
