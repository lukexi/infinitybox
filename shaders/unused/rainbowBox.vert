#version 330 core

uniform mat4 uModelViewProjection;
uniform mat4 uModel;
uniform mat4 uInverseModel;
uniform vec3 uCamera;

in vec3 aPosition;
in vec3 aNormal;
in vec2 aUV;
in vec3 aTangent;

out vec2 vUV;
out vec3 vEye;
out vec3 vPos;
out vec3 vNorm;
out mat3 iTBN;


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

  mat3 TBN = mat3(
    aTangent,
    cross( aNormal , aTangent ),
    aNormal
  );

  iTBN = transpose( TBN );

  vec3 iCamPos = ( uInverseModel * vec4( uCamera , 1. ) ).xyz;
  vEye = iCamPos - aPosition;
  vPos = aPosition;


}
