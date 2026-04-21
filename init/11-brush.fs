#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform sampler2D noiseTex;
uniform vec2 resolution;
uniform vec2 mouse;
uniform float radius;
uniform int useNoise;
uniform vec4 solidColor;

out vec4 finalColor;

void main() {
    vec2 pixel = gl_FragCoord.xy;
    vec2 delta = pixel - mouse;
    float inside = step(dot(delta, delta), radius * radius);
    vec4 baseColor = fragColor;
    vec4 fillColor = solidColor;
    if (useNoise != 0) {
        fillColor = texture(noiseTex, fragTexCoord);
    }
    finalColor = mix(fillColor, baseColor, inside);
}
