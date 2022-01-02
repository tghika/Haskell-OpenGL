attribute vec3 aPos;
attribute vec3 aColor;
attribute vec2 aTexCoord;
varying   vec3 outColor;
varying   vec2 TexCoord;

void main() {
   outColor = aColor;
   TexCoord = aTexCoord;
   gl_Position = vec4(aPos, 1.0);
}
