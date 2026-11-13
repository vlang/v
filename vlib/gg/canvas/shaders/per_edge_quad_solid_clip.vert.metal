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
    float2 normal  [[attribute(1)]];
    float4 edgeFlags  [[attribute(2)]];
    float4 quadXs  [[attribute(3)]];
    float4 quadYs  [[attribute(4)]];
    float depth  [[attribute(5)]];
    uint ssboIndex  [[attribute(6)]];
    float3 mat0  [[attribute(7)]];
    float3 mat1  [[attribute(8)]];
    float3 mat2  [[attribute(9)]];
};
struct Outputs {
    float4 sk_Position [[position]];
    uint ssboIndexVar [[user(locn0) flat]];
    float4 SolidColor_1_Var [[user(locn1) center_no_perspective]];
    float4 edgeDistances [[user(locn2)]];
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
float4 per_edge_aa_quad_vertex_fn_f4If2f4f4f4ff33f4f2(uint a, float2 b, float4 c, float4 d, float4 e, float f, float3x3 g, thread float4& h, thread float2& i);
template <typename T>matrix<T, 3, 3> mat3_inverse(matrix<T, 3, 3> m) {T a00 = m[0].x, a01 = m[0].y, a02 = m[0].z,a10 = m[1].x, a11 = m[1].y, a12 = m[1].z,a20 = m[2].x, a21 = m[2].y, a22 = m[2].z,b01 =  a22*a11 - a12*a21,b11 = -a22*a10 + a12*a20,b21 =  a21*a10 - a11*a20,det = a00*b01 + a01*b11 + a02*b21;return matrix<T, 3, 3>(b01, (-a22*a01 + a02*a21), ( a12*a01 - a02*a11),b11, ( a22*a00 - a02*a20), (-a12*a00 + a02*a10),b21, (-a21*a00 + a01*a20), ( a11*a00 - a01*a10)) * (1/det);}float2 perp_f2f2(float2 a) {
return float2(-a.y, a.x);
}
float4 per_edge_aa_quad_vertex_fn_f4If2f4f4f4ff33f4f2(uint a, float2 b, float4 c, float4 d, float4 e, float f, float3x3 g, thread float4& h, thread float2& i) {
uint k = (a + 1u) % 4u;
float4 l = d - d.wxyz;
float4 m = e - e.wxyz;
float4 n = 1.0 / max(abs(l), max(abs(m), float4(1.0)));
l *= n;
m *= n;
float4 o = l * l + m * m;
float4 p = sign(o);
if (any((p == float4(0.0)))) if (all((p == float4(0.0)))) {
l = float4(0.0, 1.0, 0.0, -1.0);
m = float4(-1.0, 0.0, 1.0, 0.0);
o = float4(1.0);
} else {
bool q = ((p.x + p.y) + p.z) + p.w > 2.5;
float4 r = q ? l.yzwx : m.yzwx;
float4 s = q ? m.yzwx : -l.yzwx;
l = mix(r, l, p);
m = mix(s, m, p);
o = mix(o.yzwx, o, p);
c = mix(c.yzwx, c, p);
}
float4 q = rsqrt(o);
l *= q;
m *= q;
float2 r = -float2(l[k], m[k]);
float2 s = float2(l[a], m[a]);
float2 t = float2(d[a], e[a]);
h = m * (d - t.x) - l * (e - t.y);
float3x3 u = mat3_inverse(g);
float3 v = g * float3(t, 1.0);
float4 w = -m * (u[0].x - u[0].z * d) + l * (u[0].y - u[0].z * e);
float4 x = -m * (u[1].x - u[1].z * d) + l * (u[1].y - u[1].z * e);
h *= rsqrt(w * w + x * x);
h += (1.5 - c) * abs(v.z);
if (any((b != float2(0.0))) && v.z > 0.0) {
float2x2 y = float2x2(u[0].xy - u[0].z * t, u[1].xy - u[1].z * t);
float2 z = float2(c[a], c[k]) * b;
float2 A = (z.x * perp_f2f2(-s)) * y;
float2 B = (z.y * perp_f2f2(r)) * y;
bool C = all((z != float2(0.0)));
if (C) {
A = normalize(A);
B = normalize(B);
if (dot(A, B) < -0.8) {
float D = sign(determinant(float2x2(A, B)));
A = D * perp_f2f2(A);
B = -D * perp_f2f2(B);
}
}
v.xy = v.xy + v.z * normalize(A + B);
h -= v.z;
}
i = t;
return float4(v.xy, v.z * f, v.z);
}
vertex Outputs vertexMain(Inputs _in [[stage_in]], const device CombinedUniforms& _anonInterface0 [[buffer(1)]], constant IntrinsicUniforms& _anonInterface1 [[buffer(0)]], uint sk_VertexID [[vertex_id]], uint sk_InstanceID [[instance_id]]) {
Globals _globals{&_anonInterface0, {}, &_anonInterface1};
(void)_globals;
Outputs _out;
(void)_out;
    float4 _skTemp0;
    float2 _skTemp1;
    float4 _skTemp2;
float2 stepLocalCoords = float2(0.0);
_globals.uniformSsboIndex = _in.ssboIndex;
float4 devPosition = ((_skTemp2 = per_edge_aa_quad_vertex_fn_f4If2f4f4f4ff33f4f2(_in.cornerID, _in.normal, _in.edgeFlags, _in.quadXs, _in.quadYs, _in.depth, float3x3(_in.mat0, _in.mat1, _in.mat2), _skTemp0, _skTemp1)), (_out.edgeDistances = _skTemp0), (stepLocalCoords = _skTemp1), _skTemp2);
_out.sk_Position = float4(_globals._anonInterface1->viewport.zw * devPosition.xy - sign(_globals._anonInterface1->viewport.zw) * devPosition.ww, devPosition.zw);
_out.SolidColor_1_Var = float4(_globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].color_1);
_out.ssboIndexVar = _globals.uniformSsboIndex;
return _out;
}
