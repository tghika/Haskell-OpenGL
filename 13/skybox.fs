#version 330 core

layout (location = 2) out     vec4        FragColor;
in      vec3        TexCoords;
uniform samplerCube skybox;

void main() {    
    FragColor = texture(skybox, TexCoords);
}
