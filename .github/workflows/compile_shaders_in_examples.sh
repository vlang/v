#!/bin/bash

for f in examples/sokol/sdf \
      examples/sokol/simple_shader_glsl \
      examples/sokol/02_cubes_glsl/cube_glsl \
      examples/sokol/03_march_tracing_glsl/rt_glsl \
      examples/sokol/04_multi_shader_glsl/rt_glsl_puppy \
      examples/sokol/04_multi_shader_glsl/rt_glsl_march \
      examples/sokol/05_instancing_glsl/rt_glsl_instancing \
      examples/sokol/06_obj_viewer/gouraud \
      ; do 
      echo "compiling shaders for $f ...";
      ./v shader $f;
done;
