attribute vec3 aPos;
attribute vec3 aColor;
varying   vec3 outColor;

void main() {
   outColor = aColor;
   gl_Position = vec4(aPos, 1.0);
}
