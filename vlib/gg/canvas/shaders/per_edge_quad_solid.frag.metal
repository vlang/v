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
    float4 edgeDistances [[user(locn1)]];
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
fragment Outputs fragmentMain(Inputs _in [[stage_in]], const device CombinedUniforms& _anonInterface0 [[buffer(1)]], constant IntrinsicUniforms& _anonInterface1 [[buffer(0)]], bool _frontFacing [[front_facing]], float4 _fragCoord [[position]]) {
Globals _globals{&_anonInterface0, &_anonInterface1};
(void)_globals;
Outputs _out;
(void)_out;
float4 outColor_1 = _in.SolidColor_1_Var;
float4 outputCoverage = float4(1.0f);
float2 _0_d = min(_in.edgeDistances.xy, _in.edgeDistances.zw);
float _1_e = min(_0_d.x, _0_d.y) * float4(_fragCoord.x, _fragCoord.y, 0.0, _fragCoord.w).w;
outputCoverage = float4(float(saturate(_1_e)));
_out.sk_FragColor = outColor_1 * outputCoverage;
return _out;
}
