#version 330 core

uniform mat4 uModelViewProjection;
uniform mat4 uModel;
uniform mat4 uInverseModel;
uniform vec3 uCamera;

in vec3 aPosition;
in vec3 aNormal;
in vec2 aUV;
in vec3 aTangent;

out mat3 vINormMat;
out vec2 vUV;
out vec3 vNormal;
out vec3 vEye;
out vec3 vPos;


mat3 matInverse( mat3 m ){
    
  vec3 a = vec3(
    
    m[1][1] * m[2][2] - m[2][1] * m[1][2],
    m[0][2] * m[2][1] - m[2][2] * m[0][1],
    m[0][1] * m[1][2] - m[1][1] * m[0][2]
      
  );
  
  vec3 b = vec3(
    
    m[1][2] * m[2][0] - m[2][2] * m[1][0],
    m[0][0] * m[2][2] - m[2][0] * m[0][2],
    m[0][2] * m[1][0] - m[1][2] * m[0][0]
      
  );
  
   vec3 c = vec3(
    
    m[1][0] * m[2][1] - m[2][0] * m[1][1],
    m[0][1] * m[2][0] - m[2][1] * m[0][0],
    m[0][0] * m[1][1] - m[1][0] * m[0][1]
      
  );
  
  
  return mat3( 
      
    a.x , a.y , a.z ,
    b.x , b.y , b.z ,
    c.x , c.y , c.z
      
  );
  
}


void main( void ) { 
  vUV = aUV;

  vec3 norm = aNormal;
  vec3 tang = aTangent;
  vec3 bino = cross( norm , tang );

  mat3 normMat = mat3(
    norm.x , norm.y , norm.z ,
    tang.x , tang.y , tang.z ,
    bino.x , bino.y , bino.z 
  );

  vINormMat = matInverse( normMat );

  vNormal = normalize( ( uModel * vec4( aNormal , 0. ) ).xyz );

  vec3 iCamPos = ( uInverseModel * vec4( uCamera , 1. ) ).xyz;

  vEye = normalize( iCamPos  - aPosition );
  vPos = aPosition;

  gl_Position = uModelViewProjection * vec4( aPosition , 1.0 );

}