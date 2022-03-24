attribute vec3 aPos;
attribute vec3 aNormal;
varying   vec3 FragPos;
varying   vec3 Normal;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
	Normal = mat3(transpose(inverse(model))) * aNormal;
	FragPos = vec3(model * vec4(aPos, 1.0));
	gl_Position = projection * view * model * vec4(aPos, 1.0);
}
