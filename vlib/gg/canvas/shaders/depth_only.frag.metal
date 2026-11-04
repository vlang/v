#include <metal_stdlib>
using namespace metal;
// Graphite's depth-only draws have no fragment function. sokol pipelines
// need one; this one writes nothing (the color attachment is masked).
fragment void fragmentMain() {}
