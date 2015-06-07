#version 330 core

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

#define STEPS 10
vec4 volumeColor( vec3 ro , vec3 rd , mat3 iBasis ){

  vec3 col = vec3( 0. );
  float lum = 0.3;
  for( int i = 0; i < STEPS; i++ ){

    vec3 p = ro - rd * float( i ) * stepDepth;
   
    p = iBasis * p;
    //float lu = abs(sin( p.y * oscillationSize ) +sin( p.z * oscillationSize ))/2.; 

    lum += abs(sin( p.y * 10. ) + sin( p.z * 10. ));///
    //col += hsv( p.x * 3. + lum / 20., 1. , 1. );
    col += hsv( lum / 10. , 1. , 1. ) / lum;

  }

  return vec4( col , lum ) / float( STEPS );

}


void main(void) {

  vec4 volCol = volumeColor( vPos , normalize(vEye) , vINormMat );

  color = vec4( (normalize( vNormal ) * .5 + .5 )  * volCol.xyz , 1. );
  // color = vec4(1.);

}