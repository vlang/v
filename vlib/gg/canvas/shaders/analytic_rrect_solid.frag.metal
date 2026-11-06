#include <metal_stdlib>
#include <simd/simd.h>
#ifdef __clang__
#pragma clang diagnostic ignored "-Wall"
#endif
using namespace metal;
struct CombinedUniformData {
float4 color_1;
};
struct Inputs {
    float4 SolidColor_1_Var [[user(locn0) center_no_perspective]];
    float4 jacobian [[user(locn1)]];
    float4 edgeDistances [[user(locn2)]];
    float4 xRadii [[user(locn3)]];
    float4 yRadii [[user(locn4)]];
    float2 strokeParams [[user(locn5)]];
    float2 perPixelControl [[user(locn6)]];
};
struct Outputs {
    float4 sk_FragColor [[color(0)]];
};
struct CombinedUniforms {
CombinedUniformData combinedUniformData[1];
};
struct IntrinsicUniforms {
float4 viewport;
float4 dstReadBounds;
};
struct Globals {
    const device CombinedUniforms* _anonInterface0;
    constant IntrinsicUniforms* _anonInterface1;
};
void K_Qvf2f22f2f2f2f2(thread float2& a, float2x2 b, float2 c, float2 d, float2 e, float2 f);
void L_Qvf2f22f2f4f4f4(thread float2& a, float2x2 b, float2 c, float4 e, float4 f, float4 g);
float2x2 float2x2_from_float4(float4 x0) {
    return float2x2(float2(x0.xy), float2(x0.zw));
}
void K_Qvf2f22f2f2f2f2(thread float2& a, float2x2 b, float2 c, float2 d, float2 e, float2 f) {
float2 g = f - d;
if (all((g > float2(0.0)))) if (all((f > float2(0.0))) || c.x > 0.0 && c.y < 0.0) {
float2 _87_a = g * e;
float2 _88_e = 1.0 / (f * f + c.x * c.x);
float2 _89_g = _88_e * _87_a;
float2 _90_c = _89_g * b;
float _91_h = rsqrt(dot(_90_c, _90_c));
float _92_i = (0.5 * _91_h) * (dot(_87_a, _89_g) - 1.0);
float _93_j = ((f.x * c.x) * _88_e.x) * _91_h;
float2 h = float2(_93_j - _92_i, _93_j + _92_i);
h.y = f.x - c.x <= 0.0 ? 1.0 : -h.y;
a = min(a, h);
} else if (c.y == 0.0) {
float2 _94_c = e * b;
float h = ((c.x - g.x) - g.y) * rsqrt(dot(_94_c, _94_c));
a.x = min(a.x, h);
}
}
void L_Qvf2f22f2f4f4f4(thread float2& a, float2x2 b, float2 c, float4 e, float4 f, float4 g) {
    float2 _skTemp0;
    float2 _skTemp1;
    float2 _skTemp2;
    float2 _skTemp3;
((K_Qvf2f22f2f2f2f2((_skTemp0 = a), b, c, e.xy, float2(-1.0), float2(f.x, g.x))), (a = _skTemp0));
((K_Qvf2f22f2f2f2f2((_skTemp1 = a), b, c, e.zy, float2(1.0, -1.0), float2(f.y, g.y))), (a = _skTemp1));
((K_Qvf2f22f2f2f2f2((_skTemp2 = a), b, c, e.zw, float2(1.0), float2(f.z, g.z))), (a = _skTemp2));
((K_Qvf2f22f2f2f2f2((_skTemp3 = a), b, c, e.xw, float2(-1.0, 1.0), float2(f.w, g.w))), (a = _skTemp3));
}
fragment Outputs fragmentMain(Inputs _in [[stage_in]], const device CombinedUniforms& _anonInterface0 [[buffer(1)]], constant IntrinsicUniforms& _anonInterface1 [[buffer(0)]], bool _frontFacing [[front_facing]], float4 _fragCoord [[position]]) {
Globals _globals{&_anonInterface0, &_anonInterface1};
(void)_globals;
Outputs _out;
(void)_out;
    float2 _skTemp4;
float4 outColor_1 = _in.SolidColor_1_Var;
float4 outputCoverage = float4(1.0f);
float4 _0_analytic_rrect_coverage_fn;
if (_in.perPixelControl.x > 0.0) _0_analytic_rrect_coverage_fn = float4(1.0f); else if (_in.perPixelControl.y > 1.0) {
float2 _1_h = min(_in.edgeDistances.xy, _in.edgeDistances.zw);
float _2_i = min(_1_h.x, _1_h.y) * float4(_fragCoord.x, _fragCoord.y, 0.0, _fragCoord.w).w;
float _3_j = (_in.perPixelControl.y - 1.0) * float4(_fragCoord.x, _fragCoord.y, 0.0, _fragCoord.w).w;
float _4_k = 1.0 - 0.5 * _3_j;
_0_analytic_rrect_coverage_fn = float4(float(saturate(_3_j * (_2_i + _4_k))));
} else {
float2x2 _5_h = float2x2_from_float4(_in.jacobian) * (1.0 / float4(_fragCoord.x, _fragCoord.y, 0.0, _fragCoord.w).w);
float2 _6_c = float2(1.0, 0.0) * _5_h;
float2 _7_c = float2(0.0, 1.0) * _5_h;
float2 _8_i = float2(rsqrt(dot(_6_c, _6_c)), rsqrt(dot(_7_c, _7_c)));
float2 _9_j = _8_i * (_in.strokeParams.x + min(_in.edgeDistances.xy, _in.edgeDistances.zw));
float2 _10_k = float2(min(_9_j.x, _9_j.y), -1.0);
float _11_l;
float _12_m;
if (_in.perPixelControl.x > -0.95) {
float2 _13_n = _8_i * ((_in.edgeDistances.xy + _in.edgeDistances.zw) + 2.0 * _in.strokeParams.xx);
_11_l = min(min(_13_n.x, _13_n.y), 1.0);
_12_m = 1.0 - 0.5 * _11_l;
} else {
float2 _14_n = (2.0 * _in.strokeParams.x) * _8_i;
float2 _15_o = _14_n - _9_j;
_10_k.y = -max(_15_o.x, _15_o.y);
if (_in.strokeParams.x > 0.0) {
float _16_p = min(_14_n.x, _14_n.y);
float2 _17_q = select(float2(_16_p), _14_n, (_15_o >= float2(-0.5)));
_11_l = saturate(max(_17_q.x, _17_q.y));
_12_m = 1.0 - 0.5 * _11_l;
} else _11_l = (_12_m = 1.0);
}
((L_Qvf2f22f2f4f4f4((_skTemp4 = _10_k), _5_h, _in.strokeParams, _in.edgeDistances, _in.xRadii, _in.yRadii)), (_10_k = _skTemp4));
float _18_n = min(_in.perPixelControl.y, 0.0) * float4(_fragCoord.x, _fragCoord.y, 0.0, _fragCoord.w).w;
float _19_o = _11_l * (min(_10_k.x + _18_n, -_10_k.y) + _12_m);
_0_analytic_rrect_coverage_fn = float4(float(saturate(_19_o)));
}
outputCoverage = _0_analytic_rrect_coverage_fn;
_out.sk_FragColor = outColor_1 * outputCoverage;
return _out;
}
