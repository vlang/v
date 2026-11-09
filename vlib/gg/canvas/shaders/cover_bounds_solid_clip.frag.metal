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
float2 devCoord = float4(_fragCoord.x, _fragCoord.y, 0.0, _fragCoord.w).xy - _globals._anonInterface1->viewport.xy;
float4 _0_b = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].rect_5;
float2 _1_c = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].radiusPlusHalf_5;
float4 _2_d = _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].edgeSelect_5;
float2 _3_e = float2(abs(_1_c.x));
float2 _4_f = float2(_2_d.xy) * ((_0_b.xy + _3_e) - devCoord);
float2 _5_g = float2(_2_d.zw) * (devCoord - (_0_b.zw - _3_e));
float2 _6_h = max(max(_4_f, _5_g), 0.0);
float _7_i = float(saturate(_3_e.x * (1.0 - length(_6_h * _1_c.y))));
float4 _8_j = saturate(float4(float2(devCoord - _0_b.xy), float2(_0_b.zw - devCoord)));
_8_j = mix(_8_j, float4(1.0f), _2_d);
float _9_k = (((_7_i * _8_j.x) * _8_j.y) * _8_j.z) * _8_j.w;
_9_k = _1_c.x < 0.0 ? 1.0f - _9_k : _9_k;
float4 outColor_5 = float4(_9_k);
outputCoverage *= outColor_5.w;
_out.sk_FragColor = outColor_1 * outputCoverage;
return _out;
}
