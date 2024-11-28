//------------------------------------------------------------------------------
//  Signed-distance-field raymarching shaders, see:
//  https://iquilezles.org/articles/mandelbulb
//  https://www.shadertoy.com/view/ltfSWn
//------------------------------------------------------------------------------

//--- vertex shader
@vs vs
uniform vs_params {
    float aspect;
    float time;
};
in vec4 position;

out vec2 pos;
out vec3 eye;
out vec3 up;
out vec3 right;
out vec3 fwd;

// compute eye position (orbit around center)
vec3 eye_pos(float time, vec3 center) {
    return center + vec3(sin(time * 0.05) * 3.0, sin(time * 0.1) * 2.0, cos(time * 0.05) * 3.0);
}

// a lookat function
void lookat(vec3 eye, vec3 center, vec3 up, out vec3 out_fwd, out vec3 out_right, out vec3 out_up) {
    out_fwd = normalize(center - eye);
    out_right = normalize(cross(out_fwd, up));
    out_up = cross(out_right, out_fwd);
}

void main() {
    gl_Position = position;
    pos.x = position.x * aspect;
    pos.y = position.y;
    const vec3 center = vec3(0.0, 0.0, 0.0);
    const vec3 up_vec = vec3(0.0, 1.0, 0.0);
    eye = eye_pos(time * 5, center);
    lookat(eye, center, up_vec, fwd, right, up);
}
@end

//--- fragment shader
@fs fs
in vec2 pos;
in vec3 eye;
in vec3 up;
in vec3 right;
in vec3 fwd;

out vec4 frag_color;

float sd_sphere(vec3 p, float s) {
    return length(p) - s;
}

float sd_mandelbulb(vec3 p, out vec4 res_color) {
    vec3 w = p;
    float m = dot(w,w);

    vec4 trap = vec4(abs(w),m);
    float dz = 1.0;

    for( int i=0; i<4; i++ ) {
        float m2 = m*m;
        float m4 = m2*m2;
        dz = 8.0*sqrt(m4*m2*m)*dz + 1.0;

        float x = w.x; float x2 = x*x; float x4 = x2*x2;
        float y = w.y; float y2 = y*y; float y4 = y2*y2;
        float z = w.z; float z2 = z*z; float z4 = z2*z2;

        float k3 = x2 + z2;
        float k2 = inversesqrt( k3*k3*k3*k3*k3*k3*k3 );
        float k1 = x4 + y4 + z4 - 6.0*y2*z2 - 6.0*x2*y2 + 2.0*z2*x2;
        float k4 = x2 - y2 + z2;

        w.x = p.x +  64.0*x*y*z*(x2-z2)*k4*(x4-6.0*x2*z2+z4)*k1*k2;
        w.y = p.y + -16.0*y2*k3*k4*k4 + k1*k1;
        w.z = p.z +  -8.0*y*k4*(x4*x4 - 28.0*x4*x2*z2 + 70.0*x4*z4 - 28.0*x2*z2*z4 + z4*z4)*k1*k2;

        trap = min( trap, vec4(abs(w),m) );

        m = dot(w,w);
        if( m > 256.0 ) {
            break;
        }
    }
    res_color = vec4(m,trap.yzw);
    return 0.25*log(m)*sqrt(m)/dz;
}

float d_scene(vec3 p, out vec4 res_color) {
    float d = sd_sphere(p, 1.1);
    if (d < 0.1) {
        d = sd_mandelbulb(p, res_color);
    }
    else {
        res_color = vec4(0.0);
    }
    return d;
}

// surface normal estimation
vec3 surface_normal(vec3 p, float dp) {
    const float eps = 0.001;
    const vec2 d = vec2(eps, 0);
    vec4 tra;
    float x = d_scene(p + d.xyy, tra) - dp;
    float y = d_scene(p + d.yxy, tra) - dp;
    float z = d_scene(p + d.yyx, tra) - dp;
    return normalize(vec3(x, y, z));
}

vec3 calc_color(vec3 ro, vec3 rd, float t, vec4 tra) {
    const vec3 light1 = vec3( 0.577, 0.577, -0.577);
    const vec3 light2 = vec3(-0.707, 0.000,  0.707);

    vec3 pos = ro + rd * t;
    vec3 nrm = surface_normal(pos, t);
    vec3 hal = normalize(light1 - rd);
    float occ = clamp(0.05 * log(tra.x), 0.0, 1.0);
    float fac = clamp(1.0 + dot(rd, nrm), 0.0, 1.0);

    // sun
    float dif1 = clamp(dot( light1, nrm), 0.0, 1.0);
    float spe1 = pow(clamp(dot(nrm, hal), 0.0, 1.0), 32.0 )*dif1*(0.04+0.96*pow(clamp(1.0-dot(hal,light1),0.0,1.0),5.0));
    // bounce
    float dif2 = clamp( 0.5 + 0.5*dot( light2, nrm ), 0.0, 1.0 )*occ;
    // sky
    float dif3 = (0.7+0.3*nrm.y)*(0.2+0.8*occ);

    vec3 col = vec3(0.01);
    col = mix(col, vec3(0.10,0.20,0.30), clamp(tra.y,0.0,1.0) );
    col = mix(col, vec3(0.02,0.10,0.30), clamp(tra.z*tra.z,0.0,1.0) );
    col = mix(col, vec3(0.30,0.10,0.02), clamp(pow(tra.w,6.0),0.0,1.0) );

    vec3 lin = vec3(0.0);
         lin += 7.0*vec3(1.50,1.10,0.70)*dif1;
         lin += 4.0*vec3(0.25,0.20,0.15)*dif2;
         lin += 1.5*vec3(0.10,0.20,0.30)*dif3;
         lin += 2.5*vec3(0.35,0.30,0.25)*(0.05+0.95*occ); // ambient
         lin += 4.0*fac*occ; // fake SSS
    col *= lin;
    col = pow( col, vec3(0.7,0.9,1.0)); // fake SSS
    col += spe1*15.0;

    // gamma
    col = sqrt(col);

    return col;
}

void main() {
    const float epsilon = 0.001;
    const float focal_length = 1.8;

    vec3 ray_origin = eye + fwd * focal_length + right * pos.x + up * pos.y;
    vec3 ray_direction = normalize(ray_origin - eye);

    vec4 tra;
    vec4 color = vec4(0.10,0.20,0.30,1.0);
    float t = 0.0;
    for (int i = 0; i < 96; i++) {
        vec3 p = ray_origin + ray_direction * t;
        float d = d_scene(p, tra);
        if (d < epsilon) {
            color.xyz = calc_color(p, ray_direction, d, tra);
            break;
        }
        else {
            color.xyz += vec3(0.003, 0.001, 0.0) * i;
        }
        if (t > 3) {
            break;
        }
        t += d;
    }
    frag_color = color;
}
@end

@program sdf vs fs
