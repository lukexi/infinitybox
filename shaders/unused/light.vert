#version 330 core

uniform mat4 uModelViewProjection;

in vec3 aPosition;

void main() {

  gl_Position = uModelViewProjection * vec4( aPosition, 1. );

}
