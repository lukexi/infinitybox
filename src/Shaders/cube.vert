#version 330 core

uniform mat4 uModelViewProjection;
uniform mat4 uModel;
uniform mat4 uInverseModel;
uniform vec3 uCamera;

uniform vec3 uLight1;
uniform vec3 uLight2;
uniform vec3 uLight3;
uniform vec3 uLight4;

in vec3 aPosition;
in vec3 aNormal;
in vec2 aUV;
in vec3 aTangent;

out vec2 vUV;
out vec3 vEye;

out vec3 vLight1;
out vec3 vLight2;
out vec3 vLight3;
out vec3 vLight4;

out vec3 vCamera;

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
  vLight1 = ( ( uInverseModel * vec4( uLight1 , 1. ) ).xyz );
  vLight2 = ( ( uInverseModel * vec4( uLight2 , 1. ) ).xyz );
  vLight3 = ( ( uInverseModel * vec4( uLight3 , 1. ) ).xyz );
  vLight4 = ( ( uInverseModel * vec4( uLight4 , 1. ) ).xyz );
  vEye = iCamPos - aPosition;
  vCamera = iCamPos;
  vPos = aPosition;
  vNorm = aNormal;

}

