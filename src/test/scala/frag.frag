#version 150

in vec2 passTexCoords;

out vec4 fragment;

uniform sampler2D tex;

void main() {
	fragment = texture(tex, passTexCoords);
}