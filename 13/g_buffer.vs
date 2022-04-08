#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTexCoords;

out     vec3 FragPos;
out     vec3 Normal;
out     vec2 TexCoords;
out     vec4 FragPosLightSpace;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
uniform mat4 lightSpaceMatrix;

void main() {
	Normal = transpose(inverse(mat3(view * model))) * aNormal;
	vec4 tmp = model * vec4(aPos, 1.0);
	FragPos =  vec3(view * tmp);
	TexCoords = aTexCoords;
    FragPosLightSpace = lightSpaceMatrix * tmp;
	gl_Position = projection * vec4(FragPos, 1.0);
}
