#version 330 core

uniform float uTime;

uniform float uParameter1;
uniform float uParameter2;
uniform float uParameter3;
uniform float uParameter4;
uniform float uParameter5;
uniform float uParameter6;
uniform float uTick;

2
in vec3 vPos;
in vec3 vCam;
in vec3 vNorm;

in vec3 vLight1;
in vec3 vLight2;

in vec2 vUv;

out vec4 color;

mat4 palette1;
mat4 palette2;
mat4 palette3;


const float MAX_TRACE_DISTANCE = 5.;           // max trace distance
const float INTERSECTION_PRECISION = 0.1;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 10;

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

  float fieldSize = 2. - illedness; //abs( sin( pa) ) * 1.;
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
   
    vec2 res = vec2( sdSphere( pos - sunPos, .6) , 1. );
    vec2 res2 = vec2( sdSphere( pos + vec3( 0., 20. , 0. ), 18. ) , 2. );

    res = smoothU( res , res2 , .2 );

    vec2 res3 =  vec2( sphereField( pos ) , 2. );


    return res3 * uCompleted + res * (1. - uCompleted);
    
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


float calcAO( in vec3 pos, in vec3 nor )
{
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


vec3 doBoxShading( vec2 l1 , vec2 l2 , vec3 ro ){

  vec3 col = vec3( 0. );

  float fillednessVal = (( ro.y + 1.5 )  / 3. ) * uFilledness;

  float spec = pow( l1.y , 40. );
  col +=  doPalette( l1.x , palette1 ) * ( spec ) * .5;

  spec = pow( l2.y , 40. );
  col +=  doPalette( l2.x , palette2 ) * ( spec ) * .5;


  float edgeSize = .05  * (1. - uComplete ) + .01;
  if( vUv.x < edgeSize || vUv.x > 1. - edgeSize || vUv.y < edgeSize || vUv.y > 1. - edgeSize ){
    col += vec3( .3 , .3 , .3 );
  }

  return col;

}

vec3 doBackgroundShading( vec2 l1 , vec2 l2 , vec3 ro ){

  float fillednessVal = ( ro.y + 1.5 ) / 3. * uFilledness * ( 1. + uCompleted );

  vec3 p =  doPalette( fillednessVal / 2. , palette3 ); 

  vec3 col = p  * uFilledness* ( 1. - uCompleted * 1. ) * .5; //* ( 1. - completed );

  return col;

}


vec3 doRayShading( vec2 l1 , vec2 l2  , vec3 norm , vec3 ro ){

  vec3 col = vec3( 0. );

  float spec = pow( l1.y , 10. );
  col +=  doPalette( l1.x , palette1 ) * ( l1.x  + spec );

  spec = pow( l2.y , 10. );
  col +=  doPalette( l2.x , palette2 ) * ( l2.x  + spec );

  col += doBackgroundShading( l1 , l2 , ro ); //}

  return col;
}




void main(){

  sunPos = vec3( 0. , filledness * 2. - 3. + completed * 5. , -3.6 );


  /*palette1 = mat4( .5 , .5 , .5 , 0. 
                 , .5 , .5 , .5 , 0.
                 , 1. , 1. , 1. , 0.
                 , .3 , .2 , .2 , 0.
                 );

  palette2 = mat4( .5 , .5 , .5 , 0. 
                 , .5 , .5 , .5 , 0.
                 , 1. , 1. , 0. , 0.
                 , .8 , .9 , .3 , 0.
                 );


  palette3 = mat4( .5 , .5 , .5 , 0. 
                 , .5 , .5 , .5 , 0.
                 , 2. , 1. , 0. , 0.
                 , .5 , .2 , .25 , 0.
                 );*/


  palette1 = mat4( .5  * ( 1. + sin( time * .5 ) * .3 ) , .5 * ( 1. + sin( time * .5 ) * .3 )  , .5 * ( 1. + sin( time * .5 ) * .3 )  , 0. 
                 , .5  * ( 1. + sin( time * .8 ) * .3 ) , .5 * ( 1. + sin( time * .3 ) * .3 )  , .5 * ( 1. + sin( time * .19 ) * .3 )  , 0.
                 , 1.  * ( 1. + sin( time * .2 ) * .3 ) , 1. * ( 1. + sin( time * .7 ) * .3 )  , 1. * ( 1. + sin( time * .4 ) * .3 )  , 0.
                 , .3  * ( 1. + sin( time * .1 ) * .3 ) , .2 * ( 1. + sin( time * .9 ) * .3 )  , .2 * ( 1. + sin( time * 1.5 ) * .3 )  , 0.
                 );

  palette2 = mat4( .5 * ( 1. + sin( time * .56 ) * .3 )  , .5 * ( 1. + sin( time * .225 ) * .3 )  , .5  * ( 1. + sin( time * .111 ) * .3 ) , 0. 
                 , .5 * ( 1. + sin( time * 1.5 ) * .3 )  , .5 * ( 1. + sin( time * .2 ) * .3 )  , .5  * ( 1. + sin( time * .3 ) * .3 ) , 0.
                 , 1. * ( 1. + sin( time * .73 ) * .3 )  , 1. * ( 1. + sin( time * .15 ) * .3 )  , 0.  * ( 1. + sin( time * .74 ) * .3 ) , 0.
                 , .8 * ( 1. + sin( time * 1.5 ) * .3 )  , .9 * ( 1. + sin( time * .35 ) * .3 )  , .3  * ( 1. + sin( time * .9 ) * .3 ) , 0.
                 );


  palette3 = mat4( .5  * ( 1. + sin( time * .86 ) * .3 )  , .5 * ( 1. + sin( time * .51 ) * .3 )  , .5  * ( 1. + sin( time * .2 ) * .3 ) , 0. 
                 , .5  * ( 1. + sin( time * 1. ) * .3 )  , .5 * ( 1. + sin( time * .76 ) * .3 )  , .5  * ( 1. + sin( time * 1.5 ) * .3 ) , 0.
                 , 2.  * ( 1. + sin( time * .72 ) * .3 )  , 1. * ( 1. + sin( time * .21 ) * .3 )  , 0.  * ( 1. + sin( time * .632 ) * .3 ) , 0.
                 , .5  * ( 1. + sin( time * .11 ) * .3 )  , .2 * ( 1. + sin( time * .06 ) * .3 )  , .25 * ( 1. + sin( time * .755 ) * .3 )  , 0.
                 );


  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vCam );

  vec2 res = calcIntersection( ro , rd );


  vec2 light1 = doLight( vLight1 , ro , vNorm , rd );
  vec2 light2 = doLight( vLight2 , ro , vNorm , rd );

  vec3 col = vec3( 0. , 0. , 0. );

  col += doBoxShading( light1 , light2 , ro );

  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;
    vec3 norm = calcNormal( pos );


    light1 = doLight( vLight1 , pos , norm , rd );
    light2 = doLight( vLight2 , pos , norm , rd );

   // col += norm * .5 + .5;
    col += doRayShading( light1 , light2 , norm , ro );

  }else{

    col += doBackgroundShading( light1 , light2 , ro );

  }


  color = vec4( col , 1. );


}