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
in vec3 vCamera;
in mat3 iTBN;


out vec4 color;



const float stepDepth = .05;
const float layers = 10.;
const float PI2 = 3.14159 * 2.;
const float oscillationSize =  4. * PI2 / 10. ;


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

const float MAX_TRACE_DISTANCE = 1.0;           // max trace distance
const float INTERSECTION_PRECISION = .0001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 10;
  

float sdSphere( vec3 p, float s ){
  return length(p)-s;
}

float repSphere( vec3 p, vec3 c , float r){
  vec3 q = mod( p , c ) - 0.5 * c ;
  return sdSphere( q , r);
}

float opU( float d1, float d2 ){
  return min(d1,d2);
}


vec2 map( vec3 pos  , vec3 ro ){  
    
    vec2 res = vec2( 10000. , 0.);// vec2( sdPlane( pos - vec3( 0. , -1. , 0. )), 0.0 );
  
    vec2 res2 = vec2( repSphere( pos - vec3( 0. , 0. , ro.z) , vec3( .1 , .1 , .01 ) ,  abs( pos.z - ro.z )), 1.);
    
    res.x = opU( res.x , res2.x );
    
    return res;
    
}

// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos , in vec3 ro ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy , ro ).x - map(pos-eps.xyy, ro).x,
      map(pos+eps.yxy , ro ).x - map(pos-eps.yxy, ro).x,
      map(pos+eps.yyx , ro ).x - map(pos-eps.yyx, ro).x );
  return normalize(nor);
}


vec2 calcIntersection( in vec3 ro, in vec3 rd ){

    
  float h =  INTERSECTION_PRECISION*2.0;
  float t = 0.0;
  float res = -1.0;
  float id = -1.;
    
  for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
      
    if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
    vec2 m = map( ro+rd*t , ro);
    h = m.x;
    t += h;
    id = m.y;
      
  }

  if( t < MAX_TRACE_DISTANCE ) res = t;
  if( t > MAX_TRACE_DISTANCE ) id =-1.0;
  
  return vec2( res , id );
    
}

void main( void ) {

  vec3 ro = iTBN * vPos;
  //ro.z = 0.;
  vec3 rd = iTBN * -normalize( vEye );

  vec3 col = vec3( 0. );
  vec2 res = calcIntersection( ro , rd );


    // If we have hit something lets get real!
  if( res.y > -.5 ){
  //col *= 20.;  

    vec3 pos = ro + rd * res.x;
    vec3 nor = calcNormal( pos , ro );
    vec3 eye = ( iTBN * vCamera ) - pos;

    
    vec3 l1 = normalize( iTBN *  vLight1 - pos );
    vec3 l2 = normalize( iTBN *  vLight2 - pos );
    vec3 l3 = normalize( iTBN *  vLight3 - pos );  
    vec3 l4 = normalize( iTBN *  vLight4 - pos );

    float lamb1 = max( 0. , dot( l1 , nor ) );
    float lamb2 = max( 0. , dot( l2 , nor ) );
    float lamb3 = max( 0. , dot( l3 , nor ) );
    float lamb4 = max( 0. , dot( l4 , nor ) );

    vec3 refl1 = reflect( l1 , nor );
    vec3 refl2 = reflect( l2 , nor );
    vec3 refl3 = reflect( l3 , nor );
    vec3 refl4 = reflect( l4 , nor );

    float spec1 = pow( max( 0. , dot( normalize( refl1 ) , -normalize( eye ))), 60.);
    float spec2 = pow( max( 0. , dot( normalize( refl2 ) , -normalize( eye ))), 60.);
    float spec3 = pow( max( 0. , dot( normalize( refl3 ) , -normalize( eye ))), 60.);
    float spec4 = pow( max( 0. , dot( normalize( refl4 ) , -normalize( eye ))), 60.);

    vec3 c1 =  vec3( 1. , 0.4 , 0.4 ); 
    vec3 c2 =  vec3( .9 , .7 , 0.2 );
    vec3 c3 =  vec3( 0.4 , 0.2 , .9 );
    vec3 c4 =  vec3( .3 , .7 , 1. ); 

    vec3 col1 = c1 * ( lamb1* .1+ spec1 * 1.1 ); //* ( 5. / length( vLight1 ));
    vec3 col2 = c2 * ( lamb2* .1+ spec2 * 1.1 ); //* ( 5. / length( vLight2 ));
    vec3 col3 = c3 * ( lamb3* .1+ spec3 * 1.1 ); //* ( 5. / length( vLight3 ));
    vec3 col4 = c4 * ( lamb4* .1+ spec4 * 1.1 ); //* ( 5. / length( vLight4 ));

    col = col1 + col2 + col3 + col4;

  }

  color = vec4( col , 1. );

}