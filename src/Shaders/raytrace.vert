#version 330 core

uniform vec3 uLight1;

uniform mat4 uModelViewProjection;
uniform mat4 uModel;
uniform mat4 uInverseModel;
uniform vec3 uCamera;

in vec3 aPosition;
in vec3 aNormal;
in vec2 aUV;
in vec3 aTangent;

out vec3 vPos;
out vec3 vLight;
out vec3 vNorm;
out vec3 vCam;

out vec2 vUv;


void main(){

  vUv = aUV;

  vPos = aPosition;
  vNorm = aNormal;

  vCam   = ( uInverseModel * vec4( uCamera , 1. ) ).xyz;
  vLight = ( uInverseModel * vec4( uLight1 , 1. ) ).xyz;


  // Use this position to get the final position 
  gl_Position = uModelViewProjection * vec4( aPosition , 1.);

}