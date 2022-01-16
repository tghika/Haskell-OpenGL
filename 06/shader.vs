attribute vec3 aPos;
attribute vec2 aTexCoord;
varying   vec3 outColor;
varying   vec2 TexCoord;
uniform   mat4 transform;

void main() {
   outColor = aColor;
   TexCoord = aTexCoord;
   gl_Position = transform * vec4(aPos, 1.0);
}
