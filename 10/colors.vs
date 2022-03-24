attribute vec3 aPos;
attribute vec3 aNormal;
attribute vec3 aTexCoords;
varying   vec3 FragPos;
varying   vec3 Normal;
varying   vec2 TexCoords;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
	Normal = mat3(transpose(inverse(model))) * aNormal;
	FragPos = vec3(model * vec4(aPos, 1.0));
	TexCoords = aTexCoords;
	gl_Position = projection * view * model * vec4(aPos, 1.0);
}
