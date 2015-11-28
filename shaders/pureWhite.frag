#version 330 core

uniform float uTime;
uniform float uStarted;
uniform float uTrigger;
uniform float uThumb;




in vec3 vPos;
in vec3 vCam;
in vec3 vNorm;

in vec3 vLight1;
in vec3 vLight2;

in vec2 vUv;

out vec4 color;

const float uTrigger = 1.;



const float MAX_TRACE_DISTANCE = 1.;           // max trace distance
const float INTERSECTION_PRECISION = 0.0000001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float PI  = 3.14159;


vec3 light1;
vec3 light2;


float sdSphere( vec3 p, float s ){
  return length(p)-s;
}

float opRepSphere( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    vec3 re = (q-p)/c;
    return sdSphere( q  , r  );
}

float opRepSphereFall( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    vec3 re = (q-p)/c;
    return sdSphere( q  , r * 2. - length( re ) * .001 );
}
vec2 opU( vec2 d1, vec2 d2 )
{
    return  d1.x < d2.x ? d1 : d2 ;
}



vec2 opS( vec2 d1, vec2 d2 )
{
    return  -d1.x > d2.x ? vec2(-d1.x , d1.y) : d2;
}


float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}


vec2 subCube( vec3 pos ){

  vec2 spheres = vec2( opRepSphereFall( pos - vec3( 0. , .0 , +.04), vec3( .009  )  , .003 ) , 2.);

  vec2 cubeO = vec2( sdBox( pos , vec3( 0.038 , 0.038 , 0.2651 ) * .5 + vec3( .005) ) , 1.);
  vec2 spheresO = vec2( opRepSphere( pos , vec3( .03 + .02 * uTrigger )  , .01 ) , 2.);

  cubeO = opS( cubeO , spheresO );

  vec2 cubeI = vec2( sdBox( pos , vec3( 0.038 , 0.038 , 1.1651 ) * .5 - vec3( .005)) , 2.);
  cubeI = opS( spheres , cubeI );

  vec2 thumb = vec2( sdSphere( pos - vec3( 0. , .08 , -.04) , .07 ) , 3. );
  cubeI = opS(  thumb , cubeI );

  vec2 trigger = vec2( sdBox( pos - vec3( 0. , -.015 , -.035) , vec3( .005 , .005 , .01) ) , 4. );
  if( uTrigger != 1. ){
    cubeI = opU(  trigger , cubeI );
  }else{
    cubeI = opS(  trigger , cubeI );
  }



  vec2 r = opU( cubeO , cubeI );

  return r;

}


// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
    
    
    vec2 res = subCube( pos );

    return res;
    
}


// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.00001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy).x - map(pos-eps.xyy).x,
      map(pos+eps.yxy).x - map(pos-eps.yxy).x,
      map(pos+eps.yyx).x - map(pos-eps.yyx).x );
  return normalize(nor);
}

float calcAO( in vec3 pos, in vec3 nor )
{
  float occ = 0.0;
    float sca = 2.1;
    for( int i=0; i<5; i++ )
    {
        float hr = 0.002 + 0.1212*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).x;
        occ += -(dd-hr)*sca;
        sca *= 0.5;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}


vec2 calcIntersection( in vec3 ro, in vec3 rd ){

    
    float h =  INTERSECTION_PRECISION*2.0;
    float t = 0.0;
    float res = -1.0;
    float id = -1.;
    
    for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
        
      if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
      
      vec2 m = map( ro+rd*t );

      h = m.x;
      t += h;
      id = m.y;
        
    }

    if( t < MAX_TRACE_DISTANCE ) res = t;
    if( t > MAX_TRACE_DISTANCE ) id =-1.0;
    
    return vec2( res , id );
     
}

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
        p += (dg+uTime*.1*spd);

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
        return mix( vec3( 1.0 ), clamp(( abs( fract(h + vec3( 3.0, 2.0, 1.0 ) / 3.0 )
                   * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
      }



vec3 doCrazyThumbColor( vec3 ro , vec3 rd){
  

  vec3 col = vec3( 0. );

  for( int i  = 0; i < 20; i++ ){

    p = ro + rd * float( i ) * .003;

    float val = triNoise3D( 2. * p   , .3 ); //length( vec3( sin( p.x * 100. ) , sin( p.y * 100. ) , sin( p.z  * 100. )) );


    if( val > .3 ){
      col = hsv( val * 7. + float( i ) * .4 , 1. , 1.);
      break;
    }
  }

  return col;
  
}


void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vCam );

  vec2 res = calcIntersection( ro , rd );

  vec3 col = vec3( 1. , 0. , 0. );




  // If we have hit something lets get real!
  if( res.y > -.5 ){

    vec3 pos = ro + rd * res.x;
    vec3 nor = calcNormal( pos );

    float AO = calcAO( pos , nor );


    col =nor * .5 + .5;
    if( res.y > 1. ){
      //col *= nor * .5 + .5;
    }

    // thumb
    if( res.y == 3. ){
      vec3 crazyCol = doCrazyThumbColor( pos , rd );
      col  = mix( col , crazyCol , uThumb );

    // trigger
    }else if( res.y == 4. ){
      col *= vec3( 0.5 , 1. , 0.5 );
      col  = mix( col , vec3( .4 , .9 , .8) , uTrigger );

    }

   
  }

  if( uStarted < 1. ){
    col =  vec3( 1. );
  }else{
    float mixVal = min( 1. , uTime * .5 );
    col = mix( vec3( 1. ) , col , mixVal );
  }



  color = vec4( col , 1. );



}
