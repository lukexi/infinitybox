#version 330 core

uniform vec3 uCamera;

in vec2 vUV;
in vec3 vEye;
in vec3 vLight1;
in vec3 vLight2;
in vec3 vLight3;
in vec3 vLight4;
in vec3 vPos;
in vec3 vNorm;
in mat3 iTBN;


out vec4 color;


const float oscillationSize = .04;
const float stepDepth = .04;
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
  for (float i=0.; i<=3.; i++ ){
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
 
    float f = triNoise3D( p * 1., .1 );
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

    col += hsv( lum , 1. , 1. ) / (( 1. + j ));

  }
  
  vec3 fCol = col;

  col /= layers;
  //col *= 20.;  

  vec3 l1 = normalize( vLight1 );
  vec3 l2 = normalize( vLight2 );
  vec3 l3 = normalize( vLight3 );  
  vec3 l4 = normalize( vLight4 );

  float lamb1 = max( 0. , dot( l1 , vNorm ) );
  float lamb2 = max( 0. , dot( l2 , vNorm ) );
  float lamb3 = max( 0. , dot( l3 , vNorm ) );
  float lamb4 = max( 0. , dot( l4 , vNorm ) );

  vec3 refl1 = reflect( l1 , vNorm );
  vec3 refl2 = reflect( l2 , vNorm );
  vec3 refl3 = reflect( l3 , vNorm );
  vec3 refl4 = reflect( l4 , vNorm );

  float spec1 = pow( max( 0. , dot( refl1 , -normalize( vEye ))), 60.);
  float spec2 = pow( max( 0. , dot( refl2 , -normalize( vEye ))), 60.);
  float spec3 = pow( max( 0. , dot( refl3 , -normalize( vEye ))), 60.);
  float spec4 = pow( max( 0. , dot( refl4 , -normalize( vEye ))), 60.);
  
  vec3 c1 =  vec3( 1. , 0.4 , 0.4 ); 
  vec3 c2 =  vec3( .9 , .7 , 0.2 );
  vec3 c3 =  vec3( 0.4 , 0.2 , .9 );
  vec3 c4 =  vec3( .3 , .7 , 1. ); 

  vec3 col1 = c1 * ( lamb1 + spec1 * .00001 ) * ( 5. / length( vLight1 ));
  vec3 col2 = c2 * ( lamb2 + spec2 * .00001 ) * ( 5. / length( vLight2 ));
  vec3 col3 = c3 * ( lamb3 + spec3 * .00001 ) * ( 5. / length( vLight3 ));
  vec3 col4 = c4 * ( lamb4 + spec4 * .00001 ) * ( 5. / length( vLight4 ));

  col =  ( fCol * ( col1 + col2 + col3 + col4 )) /layers;

  if( vUV.x > .95 || vUV.x < .05 || vUV.y > .95 || vUV.y < .05  ){
    col = fCol * ( spec1  + spec2  + spec3  + spec4  + .1 ) * 5. * (lum/layers);
  }

  color = vec4( col , 1. );

}