#include <metal_stdlib>
#include <simd/simd.h>
#ifdef __clang__
#pragma clang diagnostic ignored "-Wall"
#endif
using namespace metal;
struct CombinedUniformData {
float4 color_1;
float4 affineMatrix;
float2 translate;
float maxScale;
};
struct Inputs {
    float4 p01  [[attribute(0)]];
    float4 p23  [[attribute(1)]];
    float2 prevPoint  [[attribute(2)]];
    float2 stroke  [[attribute(3)]];
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
float2 s_Qf2f2f2(float2 c, float2 d);
float2 w_Qf2f2f2f(float2 c, float2 d, float e);
template <typename T>matrix<T, 2, 2> mat2_inverse(matrix<T, 2, 2> m) {return matrix<T, 2, 2>(m[1].y, -m[0].y, -m[1].x, m[0].x) * (1/determinant(m));}float m_Qff2f2f2f2f22(float2 a, float2 b, float2 c, float2 d, float2x2 e) {
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
float2 s_Qf2f2f2(float2 c, float2 d) {
float2 e = c - d;
if (all(e == float2(0.0))) return float2(0.0); else {
float f = 1.0 / max(abs(e.x), abs(e.y));
return normalize(f * e);
}
}
float2 w_Qf2f2f2f(float2 c, float2 d, float e) {
return fma(d - c, float2(e), c);
}
vertex Outputs vertexMain(Inputs _in [[stage_in]], const device CombinedUniforms& _anonInterface0 [[buffer(1)]], constant IntrinsicUniforms& _anonInterface1 [[buffer(0)]], uint sk_VertexID [[vertex_id]], uint sk_InstanceID [[instance_id]]) {
Globals _globals{&_anonInterface0, {}, &_anonInterface1};
(void)_globals;
Outputs _out;
(void)_out;
_globals.uniformSsboIndex = _in.ssboIndex;
float4 affineMatrix = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].affineMatrix;
float2 translate = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].translate;
float maxScale = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].maxScale;
float edgeID = float(sk_VertexID >> 1);
if ((sk_VertexID & 1) != 0) {
edgeID = -edgeID;
}
float2x2 affine = float2x2(affineMatrix.xy, affineMatrix.zw);
float4 _0_tessellate_stroked_curve;
float2 _1_h = _in.prevPoint;
float2 _2_k = _in.p01.xy;
float2 _3_l = _in.p01.zw;
float2 _4_m = _in.p23.xy;
float2 _5_n = _in.p23.zw;
float _6_o = -1.0;
if ((isinf(_in.p23.z) ? 2.0 : float(isinf(_in.p23.w))) != 0.0) {
_6_o = _5_n.x;
_5_n = _4_m;
}
float _7_p;
if (_6_o < 0.0) if (all(_2_k == _3_l) && all(_4_m == _5_n)) _7_p = 1.0; else {
float _8_f = m_Qff2f2f2f2f22(_2_k, _3_l, _4_m, _5_n, affine);
_7_p = max(ceil(sqrt(3.0 * sqrt(_8_f))), 1.0);
} else if (all(_2_k == _3_l) || all(_3_l == _4_m)) _7_p = 1.0; else {
float _9_e = p_Qff2f2f2f(affine * _2_k, affine * _3_l, affine * _4_m, _6_o);
_7_p = max(ceil(sqrt(_9_e)), 1.0);
}
float _10_q = _in.stroke.x;
float _11_r = _in.stroke.y;
bool _12_s = _in.stroke.x == 0.0;
float _13_t;
if (_12_s) {
_13_t = 0.477464825;
_10_q = 0.5;
} else {
_13_t = 0.5 / acos(max(1.0 - 0.25 / (maxScale * _in.stroke.x), -1.0));
}
if (_12_s) {
_2_k = affine * _2_k;
_3_l = affine * _3_l;
_4_m = affine * _4_m;
_5_n = affine * _5_n;
_1_h = affine * _1_h;
}
float2 _14_u = s_Qf2f2f2(all(_2_k == _3_l) ? (all(_3_l == _4_m) ? _5_n : _4_m) : _3_l, _2_k);
float2 _15_v = s_Qf2f2f2(_5_n, all(_5_n == _4_m) ? (all(_4_m == _3_l) ? _2_k : _3_l) : _4_m);
float2 _16_x = s_Qf2f2f2(_2_k, _1_h);
if (all(_14_u == float2(0.0))) {
_11_r = 0.0;
if (_6_o < 0.0) {
_14_u = float2(1.0, 0.0);
_15_v = float2(-1.0, 0.0);
} else {
_6_o = -1.0;
_14_u = _16_x;
_15_v = _16_x;
if (all(_16_x == float2(0.0))) {
_4_m = (_5_n = _2_k + _10_q * float2(1.0, 0.0));
_2_k = (_3_l = _2_k - _10_q * float2(1.0, 0.0));
_16_x = (_14_u = (_15_v = float2(1.0, 0.0)));
} else _4_m = (_5_n = _2_k + _10_q * _16_x);
}
}
float _17_y;
if (_11_r >= 0.0) _17_y = sign(_11_r) + 3.0; else {
float _18_z = acos(clamp(dot(_16_x, _14_u), -1.0, 1.0));
float _19_A = max(ceil(_18_z * _13_t), 1.0);
_17_y = _19_A + 2.0;
_17_y = min(_17_y, 16381.0);
}
float _20_z = determinant(float2x2(_4_m - _2_k, _5_n - _3_l));
float _21_A = abs(edgeID) - _17_y;
if (_21_A < 0.0) {
_15_v = _14_u;
if (any(_1_h != _2_k)) _14_u = _16_x;
_20_z = determinant(float2x2(_14_u, _15_v));
}
float _22_B = clamp(dot(_14_u, _15_v), -1.0, 1.0);
float _23_C = acos(_22_B);
if (_20_z < 0.0) _23_C = -_23_C;
float _24_D;
float _25_E = sign(edgeID);
if (_21_A < 0.0) {
_24_D = _17_y - 2.0;
_7_p = 1.0;
_5_n = (_4_m = (_3_l = _2_k));
_21_A += _24_D + 1.0;
if (_21_A < 0.0) _21_A = 0.0; else {
bool _26_G = abs(_20_z) * rsqrt(dot(_14_u, _14_u) * dot(_15_v, _15_v)) < 0.01;
if (!_26_G || dot(_14_u, _15_v) < 0.0) _25_E = _20_z < 0.0 ? min(_25_E, 0.0) : max(_25_E, 0.0);
}
} else {
float _27_F = (16383.0 - _17_y) - 1.0;
_24_D = max(ceil(abs(_23_C) * _13_t), 1.0);
_24_D = min(_24_D, _27_F);
_7_p = min(_7_p, (_27_F - _24_D) + 1.0);
}
float _28_F = _23_C / _24_D;
float _29_G = (_7_p + _24_D) - 1.0;
bool _30_H = _21_A >= _29_G;
if (_21_A > _29_G) _25_E = 0.0;
if (abs(edgeID) == 2.0 && _11_r > 0.0) {
float _31_c = fma(_22_B, 0.5, 0.5);
_25_E *= (_31_c * _11_r) * _11_r >= 1.0 ? rsqrt(_31_c) : sqrt(_31_c);
}
float2 _32_I;
float2 _33_J;
if (_21_A != 0.0 && !_30_H) {
float2 _34_K;
float2 _35_L;
float2 _36_M = _3_l - _2_k;
float2 _37_N = _5_n - _2_k;
if (_6_o >= 0.0) {
_36_M *= _6_o;
_35_L = 0.5 * _37_N - _36_M;
_34_K = (_6_o - 1.0) * _37_N;
_3_l *= _6_o;
} else {
float2 _38_O = _4_m - _3_l;
_35_L = _38_O - _36_M;
_34_K = fma(float2(-3.0), _38_O, _37_N);
}
float2 _39_O = _35_L * (_7_p * 2.0);
float2 _40_P = _36_M * (_7_p * _7_p);
float _41_Q = 0.0;
float _42_R = min(_7_p - 1.0, _21_A);
float _43_S = -abs(_28_F);
float _44_U = (1.0 + _21_A) * abs(_28_F);
for (float _45_V = 32.0;_45_V >= 1.0; _45_V *= 0.5) {
float _46_W = _41_Q + _45_V;
if (_46_W <= _42_R) {
float2 _47_X = fma(float2(_46_W), _34_K, _39_O);
_47_X = fma(float2(_46_W), _47_X, _40_P);
float _48_Y = dot(normalize(_47_X), _14_u);
float _49_Z = fma(_46_W, _43_S, _44_U);
_49_Z = min(_49_Z, 3.14159274);
if (_48_Y >= cos(_49_Z)) _41_Q = _46_W;
}
}
float _50_V = _41_Q / _7_p;
float _51_W = _21_A - _41_Q;
float _52_X = acos(clamp(_14_u.x, -1.0, 1.0));
_52_X = _14_u.y >= 0.0 ? _52_X : -_52_X;
float _53_Y = fma(_51_W, _28_F, _52_X);
_32_I = float2(cos(_53_Y), sin(_53_Y));
float2 _54_Z = float2(-_32_I.y, _32_I.x);
float _55_aa = dot(_54_Z, _34_K);
float _56_ac = dot(_54_Z, _35_L);
float _57_ad = dot(_54_Z, _36_M);
float _58_ae = max(_56_ac * _56_ac - _55_aa * _57_ad, 0.0);
float _59_af = sqrt(_58_ae);
if (_56_ac > 0.0) _59_af = -_59_af;
_59_af -= _56_ac;
float _60_ag = (-0.5 * _59_af) * _55_aa;
float2 _61_ah = abs(fma(_59_af, _59_af, _60_ag)) < abs(fma(_55_aa, _57_ad, _60_ag)) ? float2(_59_af, _55_aa) : float2(_57_ad, _59_af);
float _62_ai = _51_W != 0.0 && _61_ah.y != 0.0 ? saturate(_61_ah.x / _61_ah.y) : 0.0;
float _63_aj = max(_50_V, _62_ai);
float2 _64_ak = w_Qf2f2f2f(_2_k, _3_l, _63_aj);
float2 _65_al = w_Qf2f2f2f(_3_l, _4_m, _63_aj);
float2 _66_am = w_Qf2f2f2f(_4_m, _5_n, _63_aj);
float2 _67_an = w_Qf2f2f2f(_64_ak, _65_al, _63_aj);
float2 _68_ao = w_Qf2f2f2f(_65_al, _66_am, _63_aj);
float2 _69_ap = w_Qf2f2f2f(_67_an, _68_ao, _63_aj);
float _70_aq = fma(_6_o - 1.0, _63_aj, 1.0);
float _71_ar = (_6_o + 1.0) - _70_aq;
float _72_as = fma(_71_ar - _70_aq, _63_aj, _70_aq);
if (_63_aj != _62_ai) _32_I = _6_o >= 0.0 ? s_Qf2f2f2(_65_al * _70_aq, _64_ak * _71_ar) : s_Qf2f2f2(_68_ao, _67_an);
_33_J = _6_o >= 0.0 ? _67_an / _72_as : _69_ap;
} else {
_32_I = _21_A == 0.0 ? _14_u : _15_v;
_33_J = _21_A == 0.0 ? _2_k : _5_n;
}
float2 _73_K = float2(_32_I.y, -_32_I.x);
_33_J += _73_K * (_10_q * _25_E);
if (_12_s) _0_tessellate_stroked_curve = float4(_33_J + translate, mat2_inverse(affine) * _33_J); else _0_tessellate_stroked_curve = float4(affine * _33_J + translate, _33_J);
float4 devAndLocalCoords = _0_tessellate_stroked_curve;
float4 devPosition = float4(devAndLocalCoords.xy, _in.depth, 1.0);
_out.sk_Position = float4(_globals._anonInterface1->viewport.zw * devPosition.xy - sign(_globals._anonInterface1->viewport.zw) * devPosition.ww, devPosition.zw);
_out.SolidColor_1_Var = float4(_globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].color_1);
return _out;
}
