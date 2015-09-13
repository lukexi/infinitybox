#version 330 core

uniform float uTime;

uniform float uParameter1;
uniform float uParameter2;
uniform float uParameter3;
uniform float uParameter4;
uniform float uParameter5;
uniform float uParameter6;
uniform float uTick;


uniform float uFilledness;
uniform float uComplete;

in vec3 vPos;
in vec3 vCam;
in vec3 vNorm;

in vec3 vLight1;
in vec3 vLight2;

in vec2 vUv;

out vec4 color;

const float traceBoxSize = 10.;


const float MAX_TRACE_DISTANCE = 20.;           // max trace distance
const float INTERSECTION_PRECISION = 0.1;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 5;





vec3 hsv(float h, float s, float v){
        return mix( vec3( 1.0 ), clamp(( abs( fract(h + vec3( 3.0, 2.0, 1.0 ) / 3.0 )
                   * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
      }



vec2 opU( vec2 d1, vec2 d2 )
{
    return  d1.x < d2.x ? d1 : d2 ;
}

float opS( float d1, float d2 )
{
    return max(-d1,d2);
}

float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}

float udBox( vec3 p, vec3 b )
{
  return length(max(abs(p)-b,0.0));
}


float udRoundBox( vec3 p, vec3 b, float r )
{
  return length(max(abs(p)-b,0.0))-r;
}


float sdSphere( vec3 p, float s )
{
  return length(p)-s;
}

float opRepSphere( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return sdSphere( q  , r );
}


float opRepBox( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return sdBox( q  ,vec3( r ));
}


// Using SDF from IQ's two tweet shadertoy : 
// https://www.shadertoy.com/view/MsfGzM
float sdBlob( vec3 p ){
  p = p * 1.;
  return length(
    .05 * cos( 9. * (sin( uParameter1 )+ 1.) * p.y * p.x )
    + cos(p) * (sin( uParameter2 ) * .01 + 1.) 
    -.1 * cos( 9. * ( p.z + .3 * (sin(uParameter3) + 1.)   * p.x - p.y * (sin( uParameter4 )+ 1.)   ) ) )
    -1.; 

}


float sphereField( vec3 p ){

  //float fieldSize = 2.  + abs( sin( uParameter5) ) * .1;
  float fieldSize = (1. -  uFilledness ) *6. + 2.;
    //float fieldSize = .1;
  return opRepSphere( p , vec3(  fieldSize ), .05 + uParameter4 * .05 );

}


float cubeField( vec3 p ){

  float fieldSize = 1.  + abs( sin( uParameter5) ) * 1.;
  return opRepBox( p , vec3(fieldSize ), .3 + uParameter4 * .05  );

}

float sdBlob2( vec3 p ){
 
  vec3 pos = p;

  return length( p ) - .2 + .3 * .2 * sin( uParameter4 )*sin(300.0 * sin(uParameter1 ) *pos.x * sin( length(pos) ))*sin(200.0*sin( uParameter2 ) *pos.y )*sin(50.0 * sin( uParameter3 * 4. )*pos.z);

}

//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
   
    // using super thin cube as plane
    vec3 size = vec3( 1.  , 1. , .01 );
   // vec3 rot = vec3( iGlobalTime * .1 , iGlobalTime * .4 , -iGlobalTime * .3 );
    vec3 rot = vec3( 0.,0.,0. );
   // vec2 res = vec2( rotatedBox( pos , rot , size , .001 ) , 1.0 );
    
    float repSize = ( uParameter1 * .4 + .4) * 2.;
    repSize = 2.;

    float radius = .4 * uParameter2  + .1;

    radius = .01;

   // vec2 res = vec2( opRepSphere( pos , vec3( repSize ) , radius ) , 1. );
    //vec2 res = vec2( sdSphere( pos ,  radius ) , 1. );
    //vec2 res = vec2( sdBlob( pos ) , 1. );
    //res.x = opS( sdBox( pos , vec3(3.5) ) , res.x );

    vec2 res =  vec2( sphereField( pos ) , 2. );
    res.x = opS( sdBox( pos , vec3(traceBoxSize) ) , res.x );
    return res;
    
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




// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy).x - map(pos-eps.xyy).x,
      map(pos+eps.yxy).x - map(pos-eps.yxy).x,
      map(pos+eps.yyx).x - map(pos-eps.yyx).x );

  return normalize(nor);
}


vec3 doCol( float lamb , float spec ){

  float nSpec= pow( spec , abs(sin(uParameter1 * 1.1))* 10. + 2. );
  return
      hsv( lamb * .3 + uParameter2 , abs( sin( uParameter6 )) * .2 + .6 , abs( sin( uParameter2 ) * .4 + .6 )) * lamb 
    + hsv( nSpec * .6 + uParameter3 , abs( sin( uParameter5 )) * .4 + .6 , abs( sin( uParameter1 ) * .3 + .8 )) * nSpec;
}





void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vCam );

  vec3 lightDir1 = normalize( vLight1 - ro);

  float iLamb1 = max( dot( -vNorm , lightDir1), 0.);
  vec3  iReflDir1 = reflect( lightDir1 , -vNorm );
  float iSpec1 = max( dot( iReflDir1 , rd ), 0.);

  vec3 lightDir2 = normalize( vLight2 - ro);

  float iLamb2 = max( dot( -vNorm , lightDir2), 0.);
  vec3  iReflDir2 = reflect( lightDir2 , -vNorm );
  float iSpec2 = max( dot( iReflDir2 , rd ), 0.);

  vec3 col = doCol( iLamb1 , iSpec1 );//-vNorm * .5 + .5;
  col += doCol( iLamb2 , iSpec2 );

  col *= .1;

  vec2 res = calcIntersection( ro , rd );


  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;
    vec3 norm = calcNormal( pos );

    vec3 lightDir1 = normalize( vLight1 - pos);
    vec3 reflDir1 = reflect( lightDir1 , norm );
    float lamb1 = max( dot( norm , lightDir1), 0.);
    float spec1 = max( dot( reflDir1 , rd ), 0.);

    vec3 lightDir2 = normalize( vLight2 - pos);
    vec3 reflDir2 = reflect( lightDir2 , norm );
    float lamb2 = max( dot( norm , lightDir2), 0.);
    float spec2 = max( dot( reflDir2 , rd ), 0.);

    //col += vec3( sin( pos.x ) , sin( pos.y ) , sin( pos.z ));
    float fade = ((pos.y  + (traceBoxSize / 2.) ) /traceBoxSize);
    fade = max( 0. , ( uFilledness - fade ));
    fade *= traceBoxSize;
    fade = min( 1. , fade );

    vec3 tmpCol = ( doCol( lamb1 , spec1 ) + doCol( lamb2 , spec2 ) )*.3;

    
    //col = la9mb * vec3( 1. , 0. , 0. ) + pow( spec , 10.) * vec3( 0. , 0. , 1. );// norm * .5 +.5;
    col += mix( vec3(length(tmpCol))*fade , tmpCol*fade , uComplete );

  }


  float edgeSize = .01;
  if( vUv.x < edgeSize || vUv.x > 1. - edgeSize || vUv.y <  edgeSize  || vUv.y > 1. - edgeSize  ){


    //col += doCol( lamb , spec );
    col = vec3( 1.5 , 1.5 , 1.5 );
  }

  //vec3 col = vec3( 2. - length( texture2D( t_iri , vUv * 4. - vec2( 1.5 ) ) ));

  //vec3 col = vec3( hit );

  //col = vCam * .5 + .5;

  //color = vec4( 1. );
  //color = vec4(vec3( length(col)) , 1. );
  color = vec4( col  , 1. );

}
