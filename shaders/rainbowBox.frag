#version 330 core

uniform vec3 uCamera;

in vec2 vUV;
in vec3 vEye;
in vec3 vPos;
in vec3 vNorm;
in mat3 iTBN;


out vec4 color;


const float oscillationSize = .04;
const float stepDepth = 5.;
const float layers = 10.;
const float PI2 = 3.14159 * 2.;


// Taken from https://www.shadertoy.com/view/4ts3z2
float tri(in float x){return abs(fract(x)-.5);}
vec3 tri3(in vec3 p){return vec3( tri(p.z+tri(p.y*1.)), tri(p.z+tri(p.x*1.)), tri(p.y+tri(p.x*1.)));}
                                 

// Taken from https://www.shadertoy.com/view/4ts3z2
float triNoise3D(in vec3 p, in float spd)
{
    float z=1.4;
  float rz = 0.;
    vec3 bp = p;
  for (float i=0.; i<=3.; i++ )
  {
        vec3 dg = tri3(bp*2.);
        p += (dg+0.*.1*spd);

        bp *= 1.8;
    z *= 1.5;
    p *= 1.2;
        //p.xz*= m2;
        
        rz+= (tri(p.z+tri(p.x+tri(p.y))))/z;
        bp += 0.14;
  }
  return rz;
}

vec3 hsv(float h, float s, float v){

  return mix( vec3( 1.0 ), clamp( ( abs( fract(
    h + vec3( 3.0, 2.0, 1.0 ) / 3.0 ) * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
}

float rand(vec2 co){
  return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}



float posToFloat( vec3 p ){
 
    float f = triNoise3D( p * .02, .1 );
    return f;
    
}

void main( void ) {

  float step = .01;

  vec3 col = vec3( 0. );
  float lum = 0.;

  vec3 ro = vPos;
  vec3 rd = normalize( vEye );
  for( float j = 0.; j < layers; j ++ ){
     
    vec3 p = ro - rd * j * stepDepth;

    p = iTBN * p;

    p.z = j * stepDepth;
    vec3 offset =   p + vec3( sin(cos(p.z * 100.) + sin( p.x * 20.) + sin( p.z * 50 )) );

    lum += posToFloat( p );

    col += hsv( lum , 1. , 1. ) / ( 1. + j );
  }
 

  col /= layers;
  col *= 3.;

  if( vUV.x > .95 || vUV.x < .05 || vUV.y > .95 || vUV.y < .05  ){
    col = vec3( 1. ) - normalize( col );//vNorm * .5 + .5;
  }

  color = vec4( col , 1. );

}