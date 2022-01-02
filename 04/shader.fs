varying vec3 outColor;
varying vec2 TexCoord;
uniform sampler2D texture1;

void main() {
   gl_FragColor = texture(texture1, TexCoord);
}
