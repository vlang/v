#include <metal_stdlib>
#include <simd/simd.h>
#ifdef __clang__
#pragma clang diagnostic ignored "-Wall"
#endif
using namespace metal;
struct CombinedUniformData {
float4 color_1;
float4x4 localToDevice;
};
struct Inputs {
    float2 resolveLevel_and_idx  [[attribute(0)]];
    float4 p01  [[attribute(1)]];
    float4 p23  [[attribute(2)]];
    float2 fanPointAttrib  [[attribute(3)]];
    float depth  [[attribute(4)]];
    uint ssboIndex  [[attribute(5)]];
};
struct Outputs {
    float4 sk_Position [[position]];
    float4 SolidColor_1_Var [[user(locn0) center_no_perspective]];
    float sk_PointSize [[point_size]];
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
    uint uniformSsboIndex;
    constant IntrinsicUniforms* _anonInterface1;
};
float m_Qff2f2f2f2f22(float2 a, float2 b, float2 c, float2 d, float2x2 e);
float p_Qff2f2f2f(float2 a, float2 b, float2 c, float d);
float m_Qff2f2f2f2f22(float2 a, float2 b, float2 c, float2 d, float2x2 e) {
float2 f = e * (fma(float2(-2.0), b, c) + a);
float2 g = e * (fma(float2(-2.0), c, d) + b);
return max(dot(f, f), dot(g, g));
}
float p_Qff2f2f2f(float2 a, float2 b, float2 c, float d) {
float2 e = (min(min(a, b), c) + max(max(a, b), c)) * 0.5;
a -= e;
b -= e;
c -= e;
float f = sqrt(max(max(dot(a, a), dot(b, b)), dot(c, c)));
float2 g = fma(float2(-2.0 * d), b, a) + c;
float h = abs(fma(-2.0, d, 2.0));
float i = max(0.0, fma(f, 4.0, -1.0));
float j = length(g) * 4.0 + i * h;
float k = 4.0 * min(d, 1.0);
return j / k;
}
vertex Outputs vertexMain(Inputs _in [[stage_in]], const device CombinedUniforms& _anonInterface0 [[buffer(1)]], constant IntrinsicUniforms& _anonInterface1 [[buffer(0)]], uint sk_VertexID [[vertex_id]], uint sk_InstanceID [[instance_id]]) {
Globals _globals{&_anonInterface0, {}, &_anonInterface1};
(void)_globals;
Outputs _out;
(void)_out;
_globals.uniformSsboIndex = _in.ssboIndex;
float4x4 localToDevice = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].localToDevice;
float2 localCoord;
if (_in.resolveLevel_and_idx.x < 0.0) {
localCoord = _in.fanPointAttrib;
} else {
float2x2 vectorXform = float2x2(localToDevice[0].xy, localToDevice[1].xy);
float _0_b = _in.resolveLevel_and_idx.x;
float _1_c = _in.resolveLevel_and_idx.y;
float _2_f = isinf(_in.p23.z) ? 2.0 : float(isinf(_in.p23.w));
float2 _3_g;
if (_2_f == 2.0) _3_g = _0_b != 0.0 ? _in.p01.zw : (_1_c != 0.0 ? _in.p23.xy : _in.p01.xy); else {
float2 _4_h = _in.p01.xy;
float2 _5_i = _in.p01.zw;
float2 _6_j = _in.p23.xy;
float2 _7_k = _in.p23.zw;
float _8_l = -1.0;
float _9_m;
if (_2_f != 0.0) {
_8_l = _7_k.x;
float _10_e = p_Qff2f2f2f(vectorXform * _4_h, vectorXform * _5_i, vectorXform * _6_j, _8_l);
_9_m = ceil(log2(max(_10_e, 1.0)) * 0.5);
_5_i *= _8_l;
_7_k = _6_j;
} else {
float _11_f = m_Qff2f2f2f2f22(_4_h, _5_i, _6_j, _7_k, vectorXform);
_9_m = ceil(log2(max(9.0 * _11_f, 1.0)) * 0.25);
}
if (_0_b > _9_m) {
_1_c = floor(ldexp(_1_c, int(_9_m - _0_b)));
_0_b = _9_m;
}
float _12_n = floor(0.5 + ldexp(_1_c, int(5.0 - _0_b)));
if (0.0 < _12_n && _12_n < 32.0) {
float _13_o = _12_n * 0.03125;
float2 _14_p = mix(_4_h, _5_i, _13_o);
float2 _15_q = mix(_5_i, _6_j, _13_o);
float2 _16_r = mix(_6_j, _7_k, _13_o);
float2 _17_s = mix(_14_p, _15_q, _13_o);
float2 _18_t = mix(_15_q, _16_r, _13_o);
float2 _19_x = mix(_17_s, _18_t, _13_o);
float _20_y = mix(1.0, _8_l, _13_o);
float _21_z = (_8_l + 1.0) - _20_y;
float _22_A = mix(_20_y, _21_z, _13_o);
_3_g = _8_l < 0.0 ? _19_x : _17_s / _22_A;
} else _3_g = _12_n == 0.0 ? _4_h : _7_k;
}
localCoord = _3_g;
}
float4 devPosition = localToDevice * float4(localCoord, 0.0, 1.0);
devPosition.z = _in.depth;
_out.sk_Position = float4(_globals._anonInterface1->viewport.zw * devPosition.xy - sign(_globals._anonInterface1->viewport.zw) * devPosition.ww, devPosition.zw);
_out.SolidColor_1_Var = float4(_globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].color_1);
return _out;
}
