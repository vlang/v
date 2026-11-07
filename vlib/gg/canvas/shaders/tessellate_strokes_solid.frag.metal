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
    float4 SolidColor_1_Var [[user(locn0) center_no_perspective]];
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
_out.sk_FragColor = outColor_1;
return _out;
}
