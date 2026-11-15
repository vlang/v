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
    uint ssboIndexVar [[user(locn0) flat]];
    float4 SolidColor_1_Var [[user(locn1) center_no_perspective]];
    float4 edgeDistances [[user(locn2)]];
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
    uint uniformSsboIndex;
    constant IntrinsicUniforms* _anonInterface1;
};
fragment Outputs fragmentMain(Inputs _in [[stage_in]], const device CombinedUniforms& _anonInterface0 [[buffer(1)]], constant IntrinsicUniforms& _anonInterface1 [[buffer(0)]], bool _frontFacing [[front_facing]], float4 _fragCoord [[position]]) {
Globals _globals{&_anonInterface0, {}, &_anonInterface1};
(void)_globals;
Outputs _out;
(void)_out;
_globals.uniformSsboIndex = _in.ssboIndexVar;
float4 outColor_1 = _in.SolidColor_1_Var;
float4 outputCoverage = float4(1.0f);
float2 _0_d = min(_in.edgeDistances.xy, _in.edgeDistances.zw);
float _1_e = min(_0_d.x, _0_d.y) * float4(_fragCoord.x, _fragCoord.y, 0.0, _fragCoord.w).w;
outputCoverage = float4(float(saturate(_1_e)));
float2 devCoord = float4(_fragCoord.x, _fragCoord.y, 0.0, _fragCoord.w).xy - _globals._anonInterface1->viewport.xy;
float4 _2_b = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].rect_5;
float2 _3_c = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].radiusPlusHalf_5;
float4 _4_d = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].edgeSelect_5;
float2 _5_e = float2(abs(_3_c.x));
float2 _6_f = float2(_4_d.xy) * ((_2_b.xy + _5_e) - devCoord);
float2 _7_g = float2(_4_d.zw) * (devCoord - (_2_b.zw - _5_e));
float2 _8_h = max(max(_6_f, _7_g), 0.0);
float _9_i = float(saturate(_5_e.x * (1.0 - length(_8_h * _3_c.y))));
float4 _10_j = saturate(float4(float2(devCoord - _2_b.xy), float2(_2_b.zw - devCoord)));
_10_j = mix(_10_j, float4(1.0f), _4_d);
float _11_k = (((_9_i * _10_j.x) * _10_j.y) * _10_j.z) * _10_j.w;
_11_k = _3_c.x < 0.0 ? 1.0f - _11_k : _11_k;
float4 outColor_5 = float4(_11_k);
outputCoverage *= outColor_5.w;
_out.sk_FragColor = outColor_1 * outputCoverage;
return _out;
}
