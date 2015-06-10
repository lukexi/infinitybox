#version 330 core

uniform vec3 uCamera;

in vec2 vUV;
in mat3 vINormMat;
in vec3 vNormal;
in vec3 vEye;
in vec3 vPos;

in vec3 vCam;
in vec3 vMPos;
in vec3 vTang;
in vec3 vNorm;
in vec3 vMNorm;
in vec3 vBino;

out vec4 color;


const float oscillationSize = 20.;
const float stepDepth = .2;



vec3 hsv(float h, float s, float v){

  return mix( vec3( 1.0 ), clamp( ( abs( fract(
    h + vec3( 3.0, 2.0, 1.0 ) / 3.0 ) * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
}

float getHeight( vec2 uv ){


  float h = pow( ( abs( sin( uv.x  * 10. ) ) + abs( sin( uv.y * 10. ))) / 2. , 4. );

  if( h > .5 && h < .6 ){
    return .1;
  }else if( h > .3 && h < .4 ){
    return 0.5;
  }else{
    return 0.;
  }


}

void main( void ) {

 vec3 eyeVec = vMPos - vCam;
  //eyeVec = ( modelViewMatrix * vec4( eyeVec, 1. ) ).xyz;
  vec3 v = vec3(
    dot( eyeVec, vTang ),
    dot( eyeVec, vBino ),
    dot( eyeVec, vNorm )
  );

  eyeVec = normalize( v );


  vec3 d = normalize( eyeVec );
  vec2 uv2;
  const float layers = 10.;
  float step = .01;

  vec3 col = vec3( 0. );
  float lum = 0.;

  for( float j = 0.; j < layers; j ++ ){
    uv2 = vUV - step * d.xy * j * j / d.z;


    float amount = sin( uv2.x * 4. * 3.14159 ) + sin( uv2.y  * 4. * 3.14159 );///

    if( amount > .5 && amount < .55 ){
      col += hsv( abs( sin( amount ) * 10. )  , 1. , 1. ) ;
    }

  }
 
  col /= layers ;


  vec3 eye = vPos - uCamera;
  eye = normalize( eye );
  float match = abs( dot( eye , vNormal ) );


  mat3 TBNMat = mat3( vTang , vBino , vNorm );

  mat3 worldToTangent = transpose( TBNMat );

  vec3 tanEye   = worldToTangent * normalize( ( vMPos - vCam ) );
  vec3 tanPos   = worldToTangent * ( vMPos );
  vec3 tanNorm  = worldToTangent * vNorm;
  vec3 tanTan   = worldToTangent * vTang;
  vec3 tanBino  = worldToTangent * vBino;

  col = vec3( 0. );
  for( float j = 0.; j < layers; j++ ){
    vec3 p = tanPos + tanEye * step * j;
    vec2 uv2 = p.xy;
    float amount = ( sin( uv2.x * 4. * 3.14159 ) + sin( uv2.y  * 4. * 3.14159 ) ) ;///

    if( amount > .5 && amount < .7 ){
      col += hsv( abs(sin(amount * 10. )), 1. , 1. ) * ( .1 - abs( amount - .6 )) / .1;
    }


  }

col /= layers ;
  /*


  mat3 TBNMat = mat3( vTang , vBino , vNorm );

  vec3 eyeVec = vMPos - vCam;

  eyeVec *= TBNMat;
  vec3 eye = normalize( eyeVec );

  
  //col *= 2.;


  float height = getHeight( vUV );// pow( ( abs( sin( vUV.x  * 10. ) ) + abs( sin( vUV.y * 10. ))) / 2. , 4. );


  float v = height;
  vec2 uv2 = vUV + ( eye.xy * v );
  float finalHeight = getHeight( uv2 ); //pow( ( abs( sin( uv2.x  * 10. ) ) + abs( sin( uv2.y * 10. ))) / 2. , 4. );
 

 vec3 col = hsv( abs(sin( finalHeight  * 200. )) , 1. , 1. );*/
  //col = vBino * .5+.5;
  //col = vec3( match );
  //col = uCamera;
  //col = vec3( nor );
  col = vTang * .5 + .5;
  color = vec4( col , 1. );
  //if( found == 1 ) gl_FragColor = vec4( c, 0., c, 1. );

}