#version 330 core

uniform vec3 uCamera;

in vec2 vUV;
in vec3 vEye;
in vec3 vPos;
in vec3 vNorm;
in mat3 iTBN;


out vec4 color;


const float oscillationSize = 30.;
const float stepDepth = .002;
const float PI2 = 3.14159 * 2.;


vec3 hsv(float h, float s, float v){

  return mix( vec3( 1.0 ), clamp( ( abs( fract(
    h + vec3( 3.0, 2.0, 1.0 ) / 3.0 ) * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
}

void main( void ) {


  const float layers = 10.;
  float step = .01;

  vec3 col = vec3( 0. );
  float lum = 0.;

  vec3 ro = vPos;
  vec3 rd = normalize( vEye );
  for( float j = 0.; j < layers; j ++ ){
     
    vec3 p = ro - rd * j * stepDepth;

    p = iTBN * p;

    p.z = j * stepDepth;

    float amount = length( abs( sin(abs(  p.xy )* oscillationSize )));
                 /*abs( sin( p.y * oscillationSize )   
                      + sin( p.x * oscillationSize ) 
                      );*/

    if( amount > .45 && amount < .55 ){
      col += hsv( abs( sin( amount ) * 10. )  , 1. , 1. ) ;
    }

  }
 
  col /= layers ;

  color = vec4( col , 1. );

}