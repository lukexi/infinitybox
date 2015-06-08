#version 330 core

uniform vec3 uCamera;

in vec2 vUV;
in mat3 vINormMat;
in vec3 vNormal; 
in vec3 vEye;
in vec3 vPos;

out vec4 color;


const float oscillationSize = 20.;
const float stepDepth = .2;



vec3 hsv(float h, float s, float v){

  return mix( vec3( 1.0 ), clamp( ( abs( fract(
    h + vec3( 3.0, 2.0, 1.0 ) / 3.0 ) * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
}

void main( void ) {

  vec3 d = normalize( vEye );
  vec2 uv2;
  const float layers = 4.;
  float step = .05;

  vec3 col = vec3( 0. );
  float lum = 0.;

  for( float j = 0.; j < layers; j ++ ){
    uv2 = vUV - step * d.xy * j * j / d.z;

    if( length( uv2 ) > .5 && length( uv2 ) < .55 ){
      col += hsv( j/10. , 1. , 1. ) ;
    }

  }
  col /= layers;

  vec3 eye = vPos - uCamera;
  eye = normalize( eye );
  float match = abs( dot( eye , vNormal ) );
 
  col = eye * .5 + .5;
  col = vec3( match );
  color = vec4( col , 1. );
  //if( found == 1 ) gl_FragColor = vec4( c, 0., c, 1. );

}