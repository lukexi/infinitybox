#version 330 core

uniform float uTime;

uniform float uParameter1;
uniform float uParameter2;
uniform float uParameter3;
uniform float uParameter4;
uniform float uParameter5;
uniform float uParameter6;
uniform float uTick;
uniform float uDayNight;
uniform float uDayLength;

uniform float uFilledness;
uniform float uCompleted;
uniform float uStarted;

in vec3 vPos;
in vec3 vCam;
in vec3 vNorm;

in vec3 vLight1;
in vec3 vLight2;

in vec2 vUv;

out vec4 color;

const float MAX_TRACE_DISTANCE = 20.;           // max trace distance
const float INTERSECTION_PRECISION = 0.1;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 10;

const float PI  = 3.14159;

vec3 sunPos;



float sdSphere( vec3 p, float s )
{
  return length(p)-s;
}

float opRepSphere( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return sdSphere( q  , r );
}



vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}




float sphereField( vec3 p ){

  float fieldSize = 2.; //abs( sin( pa) ) * 1.;
  return opRepSphere( p , vec3(fieldSize ), .04 );

}



vec3 palette( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d )
{
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 doPalette( in float val , in mat4 pType ){
  return palette( val ,  pType[0].xyz , pType[1].xyz , pType[2].xyz , pType[3].xyz );
}


//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
   
   float starAmount =  smoothstep(  0. , .5 , -uDayNight );
    vec2 res = vec2( sdSphere( pos - sunPos, 3.) , 1. );

    vec3 disform =  vec3( 0. , sin( pos.x * 2. + uTime )  + sin( length( pos.xyz ) * 1. - uTime  * .1) + sin( pos.z  * 1.4   * 1. + uTime  * 1.3) , 0. ) * .4 * (1.-starAmount);
    vec2 res2 = vec2( sdSphere(  pos + disform + vec3( 0., 24. , 0. ) , 18. ) , 2. );

    res = smoothU( res , res2 , .2 );

    vec2 res3 = smoothU( res , vec2( sphereField( pos ) , 3.) , .2 );

    

    return res3 * starAmount + res * (1. - starAmount);
    
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


float calcAO( in vec3 pos, in vec3 nor ){
  float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float hr = 0.01 + 0.612*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).x;
        occ += -(dd-hr)*sca;
        sca *= 0.5;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}


vec2 doLight( vec3 lightPos , vec3 pos , vec3 norm , vec3 eyeDir ){

  vec3 lightDir = normalize( lightPos - pos );
  vec3 reflDir  = reflect( lightDir , norm );


  float lamb = max( 0. , dot( lightDir , norm   ) );
  float spec = max( 0. , dot( reflDir  , eyeDir ) );

  return vec2( lamb , spec );

}


vec3 doBoxShading( vec2 l1 , vec2 l2 , vec3 n , vec3 ro ){

  vec3 col = vec3( 0. );
  vec3 nor = n * .5 + .5;

  float spec = pow( l1.y , 40. );
  col +=  nor * ( spec ) * .5;

  spec = pow( l2.y , 40. );
  col +=   nor * ( spec ) * .5;

  float edgeSize = .01;// * (1.) + .01;
  if( vUv.x < edgeSize || vUv.x > 1. - edgeSize || vUv.y < edgeSize || vUv.y > 1. - edgeSize ){
    col += vec3( .6 , .6 , .6 );
  }

  return col;

}



/*vec3 doRayShading( vec2 l1 , vec2 l2  , vec3 norm , vec3 ro ){

  vec3 col = vec3( 0. );

  float spec = pow( l1.y , 10. );
  col +=  doPalette( l1.x , palette1 ) * ( l1.x  + spec );

  spec = pow( l2.y , 10. );
  col +=  doPalette( l2.x , palette2 ) * ( l2.x  + spec );

  //col += doBackgroundShading( l1 , l2 , ro ); //}

  return col;
}*/


vec3 doRayShading( vec2 l1 , vec2 l2  , vec3 norm , vec3 ro ){

  vec3 col = vec3( 0. );
  vec3 n = norm * .5 + .5;

  float spec = pow( l1.y , 10. );
  col +=  n * spec; 

  spec = pow( l2.y , 10. );
  col +=  n * spec; //doPalette( l2.x , palette2 ) * ( l2.x  + spec );

  //col += doBackgroundShading( l1 , l2 , ro ); //}

  return col;
}


// Gets background color if nothing is hit
// also used for refraction
vec3 bgCol( in vec3 p , in vec3 rd , in float nite){
 
 vec3 disPos = vec3( p +  .5 *  ( sin( p.x * 10. ) + sin( p.y  * 10. ) + sin( p.z * 10.)));
 vec3 n = -normalize( disPos );
 
 vec2 l1 = doLight( vLight1 , disPos , n , rd );
 vec2 l2 = doLight( vLight2 , disPos , n , rd );

 
 vec3 baseCol = vec3( .3 , .3 , .3 );
    
 return ( n * .5 + .5) * (1. -  nite);

}


void main(){

  float nite = clamp(max( -uDayNight *4. , 0. ) , 0. , 1. );
  float speedTime = uTime / uDayLength;
  float rad = speedTime * 2. * PI;

  sunPos = vec3( 0. ,  sin( rad ) * 10.,  cos( rad ) * 10. );

 // sunPos = vec3( 0. , filledness * 2. - 3. + completed * 5. , -3.6 );


  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vCam );

  vec2 res = calcIntersection( ro , rd );


  vec2 light1 = doLight( vLight1 , ro , vNorm , rd );
  vec2 light2 = doLight( vLight2 , ro , vNorm , rd );

  vec3 col = vec3( 0. , 0. , 0. );

  col += doBoxShading( light1 , light2, vNorm , ro );
 
 
  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;
    vec3 norm = calcNormal( pos );

    vec3 refr = refract( rd , norm , 1. / 1.2 );

    light1 = doLight( vLight1 , pos , norm , rd );
    light2 = doLight( vLight2 , pos , norm , rd );

   // col += norm * .5 + .5;

   
    col += nite * doRayShading( light1 , light2 , norm , ro );

    vec3 refrCol = bgCol( refr * ( MAX_TRACE_DISTANCE - res.x ) + pos , rd  , nite );
    
    //col = vec3( 3. / res.y );
    col += (1. - nite ) * refrCol;// * ( 1. - nite);
    /*if( res.y > 99.0 ){
      col += max( 0. , -uDayNight )*( norm * .5 + .5);
    }*/

  }else{

    col += bgCol( ro + rd * MAX_TRACE_DISTANCE , rd  , nite )* min( 1.  , ( uTime * 1. ) );// doBackgroundShading( light1 , light2 , ro );

  }


  color = vec4( col , 1. ) * uStarted * min( 1. , uTime * .5 );
  //color = vec4(1. );


}
