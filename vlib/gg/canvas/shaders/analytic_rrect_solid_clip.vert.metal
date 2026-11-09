#include <metal_stdlib>
#include <simd/simd.h>
#ifdef __clang__
#pragma clang diagnostic ignored "-Wall"
#endif
using namespace metal;
struct CombinedUniformData {
float4 color_1;
float4 rect_5;
float2 radiusPlusHalf_5;
float4 edgeSelect_5;
};
struct Inputs {
    uint cornerID  [[attribute(0)]];
    float2 position  [[attribute(1)]];
    float2 normal  [[attribute(2)]];
    float normalScale  [[attribute(3)]];
    float centerWeight  [[attribute(4)]];
    float4 xRadiiOrFlags  [[attribute(5)]];
    float4 radiiOrQuadXs  [[attribute(6)]];
    float4 ltrbOrQuadYs  [[attribute(7)]];
    float4 center  [[attribute(8)]];
    float depth  [[attribute(9)]];
    uint ssboIndex  [[attribute(10)]];
    float3 mat0  [[attribute(11)]];
    float3 mat1  [[attribute(12)]];
    float3 mat2  [[attribute(13)]];
};
struct Outputs {
    float4 sk_Position [[position]];
    uint ssboIndexVar [[user(locn0) flat]];
    float4 SolidColor_1_Var [[user(locn1) center_no_perspective]];
    float4 jacobian [[user(locn2)]];
    float4 edgeDistances [[user(locn3)]];
    float4 xRadii [[user(locn4)]];
    float4 yRadii [[user(locn5)]];
    float2 strokeParams [[user(locn6)]];
    float2 perPixelControl [[user(locn7)]];
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
float2 perp_f2f2(float2 a);
float4 analytic_rrect_vertex_fn_f4If2f2fff4f4f4f4ff33f4f4f4f4f2f2f2(uint a, float2 b, float2 c, float d, float e, float4 f, float4 g, float4 h, float4 i, float j, float3x3 k, thread float4& l, thread float4& m, thread float4& n, thread float4& o, thread float2& p, thread float2& q, thread float2& r);
template <typename T>matrix<T, 3, 3> mat3_inverse(matrix<T, 3, 3> m) {T a00 = m[0].x, a01 = m[0].y, a02 = m[0].z,a10 = m[1].x, a11 = m[1].y, a12 = m[1].z,a20 = m[2].x, a21 = m[2].y, a22 = m[2].z,b01 =  a22*a11 - a12*a21,b11 = -a22*a10 + a12*a20,b21 =  a21*a10 - a11*a20,det = a00*b01 + a01*b11 + a02*b21;return matrix<T, 3, 3>(b01, (-a22*a01 + a02*a21), ( a12*a01 - a02*a11),b11, ( a22*a00 - a02*a20), (-a12*a00 + a02*a10),b21, (-a21*a00 + a01*a20), ( a11*a00 - a01*a10)) * (1/det);}float2x2 float2x2_from_float4(float4 x0) {
    return float2x2(float2(x0.xy), float2(x0.zw));
}
float4 float4_from_float2x2(float2x2 x) {return float4(x[0].xy, x[1].xy);}float2 perp_f2f2(float2 a) {
return float2(-a.y, a.x);
}
float4 analytic_rrect_vertex_fn_f4If2f2fff4f4f4f4ff33f4f4f4f4f2f2f2(uint a, float2 b, float2 c, float d, float e, float4 f, float4 g, float4 h, float4 i, float j, float3x3 k, thread float4& l, thread float4& m, thread float4& n, thread float4& o, thread float2& p, thread float2& q, thread float2& r) {
float w = 1.0;
uint x = (a + 1u) % 4u;
bool y = i.z <= 0.0;
bool z = false;
float4 A;
float4 B;
float4 C = float4(1.0);
bool D = false;
if (f.x < -1.0) {
D = f.y > 0.0;
A = D ? h.xxzz : h.xzzx;
B = h.yyww;
if (f.y < 0.0) {
n = -f - 2.0;
o = g;
p = float2(0.0, 1.0);
} else {
n = g;
o = n;
p = f.zw;
w = p.y < 0.0 ? 0.414213568 : sign(p.y);
}
} else if (any((f > float4(0.0)))) {
A = h.xzzx;
B = h.yyww;
n = f;
o = g;
p = float2(0.0, -1.0);
} else {
A = g;
B = h;
C = -f;
n = float4(0.0);
o = float4(0.0);
p = float2(0.0, 1.0);
z = true;
}
float2 E = float2(n[a], o[a]);
if (a % 2u != 0u) E = E.yx;
float2 F = float2(1.0);
if (all((E > float2(0.0)))) {
w = 0.414213568;
F = E.yx;
}
float4 G = A - A.wxyz;
float4 H = B - B.wxyz;
float4 I = 1.0 / max(abs(G), max(abs(H), float4(1.0)));
G *= I;
H *= I;
float4 J = G * G + H * H;
float4 K = sign(J);
float4 L = float4(0.0);
float2 M = float2(p.x);
if (any((K == float4(0.0)))) if (all((K == float4(0.0)))) {
G = float4(0.0, 1.0, 0.0, -1.0);
H = float4(-1.0, 0.0, 1.0, 0.0);
J = float4(1.0);
} else {
bool N = ((K.x + K.y) + K.z) + K.w > 2.5;
float4 O = N ? G.yzwx : H.yzwx;
float4 P = N ? H.yzwx : -G.yzwx;
G = mix(O, G, K);
H = mix(P, H, K);
J = mix(J.yzwx, J, K);
C = mix(C.yzwx, C, K);
if (!N && w == 0.0) {
M *= float2(K[a], K[x]);
L = (K - 1.0) * p.x;
p.y = 1.0;
w = 1.0;
}
}
float4 N = rsqrt(J);
G *= N;
H *= N;
float2 O = -float2(G[x], H[x]);
float2 P = float2(G[a], H[a]);
float2 Q;
bool R = false;
if (d < 0.0) if (i.w < 0.0 || e * i.z != 0.0) R = true; else {
float S = i.w;
float2 T = E + (y ? -M : M);
if (w == 1.0 || any((T <= float2(S)))) Q = T - S; else Q = T * b - S * c;
} else Q = (E + M) * (b + w * b.yx);
if (R) Q = i.xy; else {
Q -= E;
Q = (float2(A[a], B[a]) + O * Q.x) + P * Q.y;
}
m = (H * (A - Q.x) - G * (B - Q.y)) + L;
float3x3 S = mat3_inverse(k);
float3 T = k * float3(Q, 1.0);
l = float4(S[0].xy - S[0].z * Q, S[1].xy - S[1].z * Q);
if (z) {
float4 U = -H * (S[0].x - S[0].z * A) + G * (S[0].y - S[0].z * B);
float4 V = -H * (S[1].x - S[1].z * A) + G * (S[1].y - S[1].z * B);
m *= rsqrt(U * U + V * V);
m += (1.0 - C) * abs(T.z);
bool W = all(C == float4(1.0)) && dot(abs(G * G.yzwx + H * H.yzwx), float4(1.0)) < 0.00024;
if (W) {
float2 X = m.xy + m.zw;
q.y = 1.0 + min(min(X.x, X.y), abs(T.z));
} else q.y = 1.0 + abs(T.z);
}
if (d > 0.0 && T.z > 0.0) {
float2x2 U = float2x2_from_float4(l);
float2 V = float2(C[a], C[x]) * c;
float2 W = ((F.x * V.x) * perp_f2f2(-P)) * U;
float2 X = ((F.y * V.y) * perp_f2f2(O)) * U;
bool Y = all((V != float2(0.0)));
if (w == 1.0 && Y) {
W = normalize(W);
X = normalize(X);
if (dot(W, X) < -0.8) {
float Z = sign(determinant(float2x2(W, X)));
W = Z * perp_f2f2(W);
X = -Z * perp_f2f2(X);
}
}
T.xy = T.xy + T.z * normalize(W + X);
if (z) m -= T.z; else q.y = -T.z;
} else if (!z) q.y = 0.0;
q.x = float(e != 0.0 ? 1.0 : (y ? -1.0 : 0.0));
if (D) l = float4_from_float2x2(float2x2(float2(H.x, -H.y), float2(-G.x, G.y)) * float2x2_from_float4(l));
r = Q;
return float4(T.xy, T.z * j, T.z);
}
vertex Outputs vertexMain(Inputs _in [[stage_in]], const device CombinedUniforms& _anonInterface0 [[buffer(1)]], constant IntrinsicUniforms& _anonInterface1 [[buffer(0)]], uint sk_VertexID [[vertex_id]], uint sk_InstanceID [[instance_id]]) {
Globals _globals{&_anonInterface0, {}, &_anonInterface1};
(void)_globals;
Outputs _out;
(void)_out;
    float4 _skTemp0;
    float4 _skTemp1;
    float4 _skTemp2;
    float4 _skTemp3;
    float2 _skTemp4;
    float2 _skTemp5;
    float2 _skTemp6;
    float4 _skTemp7;
float2 stepLocalCoords = float2(0.0);
_globals.uniformSsboIndex = _in.ssboIndex;
float4 devPosition = ((_skTemp7 = analytic_rrect_vertex_fn_f4If2f2fff4f4f4f4ff33f4f4f4f4f2f2f2(_in.cornerID, _in.position, _in.normal, _in.normalScale, _in.centerWeight, _in.xRadiiOrFlags, _in.radiiOrQuadXs, _in.ltrbOrQuadYs, _in.center, _in.depth, float3x3(_in.mat0, _in.mat1, _in.mat2), _skTemp0, _skTemp1, _skTemp2, _skTemp3, _skTemp4, _skTemp5, _skTemp6)), (_out.jacobian = _skTemp0), (_out.edgeDistances = _skTemp1), (_out.xRadii = _skTemp2), (_out.yRadii = _skTemp3), (_out.strokeParams = _skTemp4), (_out.perPixelControl = _skTemp5), (stepLocalCoords = _skTemp6), _skTemp7);
_out.sk_Position = float4(_globals._anonInterface1->viewport.zw * devPosition.xy - sign(_globals._anonInterface1->viewport.zw) * devPosition.ww, devPosition.zw);
_out.SolidColor_1_Var = float4(_globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].color_1);
_out.ssboIndexVar = _globals.uniformSsboIndex;
return _out;
}
