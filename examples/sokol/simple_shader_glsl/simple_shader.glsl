// The following defines a vertex shader main function
@vs vs
in vec4 position;
in vec4 color0;

// You can add more functions here

void main() {
    gl_Position = position;
    color = color0;
}
@end

// The following defines a fragment shader main function
@fs fs
in vec4 color;
out vec4 frag_color;

// You can add more functions here

void main() {
    frag_color = color;
}
@end

// The value after `@program` decide the name of the function you
// need to define in V the value is suffixed `_shader_desc`.
// Thus the name for this becomes: `simple_shader_desc` and it's signature in V is:
// `fn C.simple_shader_desc(gfx.Backend) &gfx.ShaderDesc`.
// Running `v shader -v .` in this dir will also show you this information.
@program simple vs fs
