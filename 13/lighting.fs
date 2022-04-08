#version 330 core

out vec4 FragColor;

in vec2 TexCoords;
uniform sampler2D gPosition;
uniform sampler2D gNormal;
uniform sampler2D gAlbedo;
uniform sampler2D ssao;

struct Light {
  vec3  position;
  vec3  ambient;
  vec3  diffuse;
  vec3  specular;
};

uniform Light light;
uniform vec3  viewPos;

void main() {
    vec3 FragPos = texture(gPosition, TexCoords).rgb;
    float shadow = texture(gPosition, TexCoords).w;
    vec3 Normal = texture(gNormal, TexCoords).rgb;
    vec3 Diffuse = texture(gAlbedo, TexCoords).rgb;
    float AmbientOcclusion = texture(ssao, TexCoords).r;
    
    vec3 ambient = light.ambient * Diffuse * AmbientOcclusion;
    vec3 viewDir  = normalize(viewPos-FragPos);

    vec3 lightDir = normalize(light.position - FragPos);
    vec3 diffuse = max(dot(Normal, lightDir), 0.0) * Diffuse * light.diffuse;

    vec3 halfwayDir = normalize(lightDir + viewDir);  
    float spec = pow(max(dot(Normal, halfwayDir), 0.0), 8.0);
    vec3 specular = light.specular * spec * texture(gAlbedo, TexCoords).w;

    vec3 lighting = vec3(0);

    if (dot(Normal,Normal)<0.01){
      lighting += Diffuse;
    }else{
      lighting += ambient + (1.0 - shadow) * (diffuse + specular);
    }

    FragColor = vec4(lighting, 1.0);
}
