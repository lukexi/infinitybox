#version 330 core

uniform float uTime;
uniform float uTick;

uniform vec3 uParameterA;
uniform vec3 uParameterB;

in vec3 vPos;
in vec3 vCam;
in vec3 vNorm;

in vec3 vLight1;
in vec3 vLight2;

in vec2 vUv;

out vec4 color;



const float MAX_TRACE_DISTANCE = 10.;           // max trace distance
const float INTERSECTION_PRECISION = 0.01;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 20;



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



vec2 opU( vec2 d1, vec2 d2 )
{
    return  d1.x < d2.x ? d1 : d2 ;
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
    return udBox( q  ,vec3( r ));
}

float smin( float a, float b )
{
  float k = 0.16;
  float h = clamp( 0.5 + 0.5*(b-a)/k, 0.0, 1.0 );
  return mix( b, a, h ) - k*h*(1.0-h);
}



// Using SDF from IQ's two tweet shadertoy : 
// https://www.shadertoy.com/view/MsfGzM
float sdBlob( vec3 p ){

  return length(
    .05 * cos( 9. * (sin( uParameterA.x )+ 1.) * p.y * p.x )
    + cos(p) * (sin( uParameterA.y ) * .01 + 1.) 
    -.1 * cos( 9. * ( p.z + .3 * (sin(uParameterA.z) + 1.)   * p.x - p.y * (sin( uParameterB.x )+ 1.)   ) ) )
    -1.; 

}

float cubeField( vec3 p ){

  float fieldSize = 1.  + abs( sin( uParameterB.y) ) * 1.;
  return opRepBox( p , vec3(fieldSize ), .01 + uParameterB.x * .05  );

}

float sphereField( vec3 p ){

  float fieldSize = 1.  + abs( sin( uParameterB.y + uTick) ) * 1.;
  return opRepSphere( p , vec3(fieldSize ), .01 + uParameterB.x * .05 );

}

float sdBlob2( vec3 p ){
 
  vec3 pos = p;

  return length( p ) - .2 + .3 * .2 * sin( uParameterB.x + uTick )*sin(300.0 * sin(uParameterA.x ) *pos.x * sin( length(pos) ))*sin(200.0*sin( uParameterA.y + uTick ) *pos.y )*sin(50.0 * sin( uParameterA.z * 4. )*pos.z);

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
    
    float repSize = ( uParameterA.x * .4 + .4 + uTick * .1) * 2.;
    repSize = 2.;

    float radius = .4 * uParameterA.y + uTick  + .1;

    radius = .01;

   // vec2 res = vec2( opRepSphere( pos , vec3( repSize ) , radius ) , 1. );
    //vec2 res = vec2( sdSphere( pos ,  radius ) , 1. );
    vec2 res;
    if( gl_FrontFacing ){
       res = vec2( sdBlob2( pos ) , 1. );
       res.x = smin( res.x , sdSphere(pos - vLight1, .1 ));
       res.x = smin( res.x , sdSphere(pos - vLight2, .1 ));
    }else{
      res = vec2( sphereField( pos ) , 2. );
    }
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
vec3 calcNoiseNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy).x - map(pos-eps.xyy).x,
      map(pos+eps.yxy).x - map(pos-eps.yxy).x,
      map(pos+eps.yyx).x - map(pos-eps.yyx).x );

  float noiseS = sin( uParameterA.z * 1. ) * 3. + 4.;
  vec3 noiseNorm = vec3(
      triNoise3D(pos*noiseS+eps.xyy, .4 ) - triNoise3D(pos*noiseS-eps.xyy, .4 ),
      triNoise3D(pos*noiseS+eps.yxy, .4 ) - triNoise3D(pos*noiseS-eps.yxy, .4 ),
      triNoise3D(pos*noiseS+eps.yyx, .4 ) - triNoise3D(pos*noiseS-eps.yyx, .4 ) );

  nor = nor + noiseNorm * .2;


  return normalize(nor);
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




/*vec3 doCol( float lamb , float spec ){

  float nSpec= pow( spec , abs(sin(uParameterA.x * 1.1))* 10. + 2. );
  return
      vec3( 1. , .6 , .2 ) * hsv( uParameterB.z , 1. , 1. ) *  lamb //hsv( lamb , abs( sin( uParameterB.z )) * .2 + .6 , abs( sin( uParameterA.y ) * .4 + .6 )) * lamb 
    + vec3( .3 , .6 , 1. ) *  hsv( uParameterB.y , 1. , 1. ) * nSpec;// hsv( nSpec , abs( sin( uParameterB.y )) * .4 + .6 , abs( sin( uParameterB.x ) * .3 + .8 )) * nSpec;
}*/


vec3 doCol( float lamb1 , float spec1  , float lamb2 , float spec2){

  float nSpec1= pow( spec1 , abs(sin(uParameterA.x * 1.1))* 10. + 2. );
  float nSpec2= pow( spec2 , abs(sin(uParameterA.x * 1.1))* 10. + 2. );
  return
      .5 *  hsv( lamb1  * .3 + uParameterA.y + uTick , abs( sin( uParameterB.z )) * .2 + .6 , abs( sin( uParameterA.y ) * .4 + .6 )) * lamb1 
    +  hsv( nSpec1 * .6 + uParameterA.z , abs( sin( uParameterB.y )) * .4 + .6 , abs( sin( uParameterB.x ) * .3 + .8 )) * nSpec1
    + .5 *  hsv( lamb2  * .3 + uParameterA.y , abs( cos( uParameterB.z )) * .2 + .6 , abs( cos( uParameterA.y ) * .4 + .6 )) * lamb2
    +  hsv( nSpec2 * .6 + uParameterA.z , abs( cos( uParameterB.y )) * .4 + .6 , abs( cos( uParameterB.x ) * .3 + .8 )) * nSpec2;
}





void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vCam );

  vec3 lightDir1 = normalize( vLight1 - ro);
  vec3 lightDir2 = normalize( vLight2 - ro);

  vec2 res = calcIntersection( ro , rd );

  vec3 reflDir1 = reflect( lightDir1 , vNorm );
  vec3 reflDir2 = reflect( lightDir2 , vNorm );

  float lamb1 = max( dot( vNorm , lightDir1), 0.);
  float spec1 = max( dot( reflDir1 , rd ), 0.);

  float lamb2 = max( dot( vNorm , lightDir2), 0.);
  float spec2 = max( dot( reflDir2 , rd ), 0.);



  float iLamb1 = max( dot( -vNorm , lightDir1), 0.);
  vec3  iReflDir1 = reflect( lightDir1 , -vNorm );
  float iSpec1 = max( dot( iReflDir1 , rd ), 0.);

  float iLamb2 = max( dot( -vNorm , lightDir2), 0.);
  vec3  iReflDir2 = reflect( lightDir2 , -vNorm );
  float iSpec2 = max( dot( iReflDir2 , rd ), 0.);


  vec3 col = doCol( iLamb1 , iSpec1 , iLamb2 , iSpec2 );//-vNorm * .5 + .5;
  
  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;

    vec3 lightDir1 = normalize( vLight1 - pos);
    vec3 lightDir2 = normalize( vLight2 - pos);
    vec3 norm;

    if( res.y == 3.){
      norm = calcNoiseNormal( pos );
    }else{
      norm = calcNormal( pos );
    }
    vec3 reflDir1 = reflect( lightDir1 , norm );
    vec3 reflDir2 = reflect( lightDir2 , norm );

    float lamb1 = max( dot( norm , lightDir1 ), 0.);
    float spec1 = max( dot( reflDir1 , rd ), 0.);

    float lamb2 = max( dot( norm , lightDir2 ), 0.);
    float spec2 = max( dot( reflDir2 , rd ), 0.);


    //col = lamb * vec3( 1. , 0. , 0. ) + pow( spec , 10.) * vec3( 0. , 0. , 1. );// norm * .5 +.5;
    col = doCol( lamb1 , spec1 , lamb2 , spec2 );
  }

  if( vUv.x < .05 || vUv.x > .95 || vUv.y < .05 || vUv.y > .95 ){


        col = doCol( lamb1 , spec1 , lamb2 , spec2 );
    col += vec3( .3 , .3 , .3 );
  }

  //vec3 col = vec3( 2. - length( texture2D( t_iri , vUv * 4. - vec2( 1.5 ) ) ));

  //vec3 col = vec3( hit );

  //col = vCam * .5 + .5;

  //col = vec3( 1. , 1. , 1. );
  //gl_FragColor = vec4(vec3(length( col)) , 1. );
  color = vec4( col , 1. );


}
