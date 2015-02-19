#version 150

in vec4 inPosition;
in vec2 inTexCoords;

out vec2 passTexCoords;

uniform mat4 projection;
uniform mat4 trans;

void main() {
	gl_Position = (projection * trans) * inPosition;
	passTexCoords = inTexCoords;
}