#version 330 core

uniform mat4 uModelViewProjection;
uniform mat4 uModel;
uniform vec3 uCamera;

in vec3 aPosition;
in vec3 aNormal;
in vec2 aUV;
in vec3 aTangent;

out vec2 vUV;
out vec3 vNormal;
out vec3 vEye;
out vec3 vPos;


mat3 m3( mat4 mIn ) {

  mat3 mOut;
  
  mOut[ 0 ][ 0 ] = mIn[ 0 ][ 0 ]; 
  mOut[ 0 ][ 1 ] = mIn[ 0 ][ 1 ]; 
  mOut[ 0 ][ 2 ] = mIn[ 0 ][ 2 ]; 
  
  mOut[ 1 ][ 0 ] = mIn[ 1 ][ 0 ]; 
  mOut[ 1 ][ 1 ] = mIn[ 1 ][ 1 ]; 
  mOut[ 1 ][ 2 ] = mIn[ 1 ][ 2 ]; 
  
  mOut[ 2 ][ 0 ] = mIn[ 2 ][ 0 ]; 
  mOut[ 2 ][ 1 ] = mIn[ 2 ][ 1 ]; 
  mOut[ 2 ][ 2 ] = mIn[ 2 ][ 2 ]; 
  
  return mOut;
}

void main() {

  gl_Position = uModelViewProjection * vec4( aPosition, 1. );

  vUV = aUV;

  vec3 n = m3( uModel ) * aNormal; 
  vec3 t = m3( uModel ) * aTangent.xyz;
  vec3 b = m3( uModel ) * cross( n, t );
  
  vec3 eyeVec = ( uModel * vec4( aPosition, 1. ) ).xyz - uCamera;
  //eyeVec = ( modelViewMatrix * vec4( eyeVec, 1. ) ).xyz;
  vec3 v = vec3(
    dot( eyeVec, t ),
    dot( eyeVec, b ),
    dot( eyeVec, n )
  );
  eyeVec = normalize( v );

  vEye = eyeVec;
  vPos = ( uModel * vec4( aPosition, 1. ) ).xyz;
  vNormal = normalize( ( uModel * vec4( aNormal , 0. ) ).xyz );

}

