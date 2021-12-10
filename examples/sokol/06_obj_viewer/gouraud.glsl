//#pragma sokol @ctype mat4 hmm_mat4

#pragma sokol @vs vs

uniform vs_params {
	mat4 u_MVMatrix;       // A constant representing the combined model/view matrix.
	mat4 u_MVPMatrix;      // A constant representing the combined model/view/projection matrix.
	mat4 u_NMatrix;        // A constant representing the Normal Matrix
};
 
in vec4 a_Position;     // Per-vertex position information we will pass in.
in vec3 a_Normal;       // Per-vertex normal information we will pass in.
in vec4 a_Color;        // Per-vertex color information we will pass in.
in vec2 a_Texcoord0;

out vec3 v_Position;   // This will be passed into the fragment shader.
out vec4 v_Color;      // This will be passed into the fragment shader.
out vec3 v_Normal;     // This will be passed into the fragment shader.
out vec3 v_Normal1;
out vec2 uv;           // This will be passed into the fragment shader.

// The entry point for our vertex shader.
void main()
{
	// Transform the vertex into eye space.
	v_Position = vec3(u_MVMatrix * a_Position);
	// Pass through the color.
	v_Color = a_Color;
	// calc eye space normal
	v_Normal = vec3(u_NMatrix * vec4(a_Normal, 1.0));
	// texture coord
	uv = a_Texcoord0;
	
	v_Normal1 = normalize(vec3(u_MVMatrix * vec4(a_Normal, 1.0)));
	
	// gl_Position is a special variable used to store the final position.
	// Multiply the vertex by the matrix to get the final point in normalized screen coordinates.
	gl_Position = u_MVPMatrix * a_Position;
}

#pragma sokol @end

#pragma sokol @fs fs
//precision mediump float;       // Set the default precision to medium. We don't need as high of a precision in the fragment shader
uniform sampler2D tex;
uniform fs_params {
	vec4 u_LightPos;       // The position of the light in eye space.
	vec4 ambientColor;
	vec4 diffuseColor;
	vec4 specularColor;
	
};
in vec3 v_Position;       // Interpolated position for this fragment.
in vec4 v_Color;          // This is the color from the vertex shader interpolated across the triangle per fragment.
in vec3 v_Normal;         // Interpolated normal for this fragment.
in vec3 v_Normal1;
in vec2 uv;
out vec4 frag_color;

vec3 lightDirection = -u_LightPos.xyz;// vec3(0.0, -0.5, 0.5);
//const vec4 ambientColor = vec4(0.094, 0.0, 0.0, 1.0);
//const vec4 diffuseColor = vec4(0.5, 0.0, 0.0, 1.0);
//const vec4 specularColor = vec4(1.0, 1.0, 1.0, 1.0);
//const float shininess = 10.0;
const vec4 lightColor = vec4(1.0, 1.0, 1.0, 1.0);

vec3 phongBRDF(vec3 lightDir, vec3 viewDir, vec3 normal, vec3 phongDiffuseCol, vec3 phongSpecularCol, float phongShininess) {
  vec3 color = phongDiffuseCol;
  vec3 reflectDir = reflect(-lightDir, normal);
  float specDot = max(dot(reflectDir, viewDir), 0.0);
  color += pow(specDot, phongShininess) * phongSpecularCol;
  return color;
}

vec4 getPhong(in vec4 diffuseColor) {
  vec3 lightDir = normalize(-lightDirection);
  vec3 viewDir = normalize(-v_Position);
  vec3 n = normalize(v_Normal);

  vec3 luminance = ambientColor.rgb * 0.5;
  
  float illuminance = dot(lightDir, n);
  if(illuminance > 0.0) {
		// we save specular shiness in specularColor.a
    vec3 brdf = phongBRDF(lightDir, viewDir, n, diffuseColor.rgb, specularColor.rgb, specularColor.a * 1000);
    luminance += brdf * illuminance * lightColor.rgb;
  }

  vec4 outColor = vec4(luminance,1.0);
  return outColor;
}
          
// The entry point for our fragment shader.
void main()
{
	vec4 txt = texture(tex, uv);
 
	// Directional light
	float directional = dot(normalize(v_Normal1), normalize(vec3(0,0.5,1))) ;
	directional = directional * 0.15;
	 
  // Multiply the color by the diffuse illumination level to get final output color.	
	frag_color = vec4(clamp(directional + txt.rgb * getPhong(diffuseColor).rgb,0,1), txt.a * diffuseColor.a);
	
}
#pragma sokol @end

#pragma sokol @program gouraud vs fs