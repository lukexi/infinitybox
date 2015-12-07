#version 330 core


layout (std140) uniform uboData {
  // Manually grouping things into 4 'til I integrate a more principled approach
  uniform float uStarted;
  uniform float uTime;
  uniform float uDayNight;
  uniform float uDayLength;
  
  uniform vec3 uLight1;
  uniform float uFilledness;

  uniform vec3 uLight2;
  uniform float uTick;
};

uniform vec3 uParameterA;
uniform vec3 uParameterB;

uniform vec3 uCollisionPosition;
uniform float uCollisionTime;


in vec3 vPos;
in vec3 vCam;
in vec3 vNorm;

in vec3 vLight1;
in vec3 vLight2;

in vec2 vUv;

out vec4 color;


vec3 sunPos;



const float MAX_TRACE_DISTANCE = 5.;           // max trace distance
const float INTERSECTION_PRECISION = 0.01;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 10;
const float PI  = 3.14159;


vec3 light1;
vec3 light2;

mat4 rotateX(float angle){
    float c = cos(angle);
    float s = sin(angle);
  return mat4(1.0, 0.0, 0.0, 0.0, 0.0, c, -s, 0.0, 0.0, s, c, 0.0, 0.0, 0.0, 0.0, 1.0);
}

mat4 rotateY(float angle){
    float c = cos(angle);
    float s = sin(angle);
  return mat4(c, 0.0, s, 0.0, 0.0, 1.0, 0.0, 0.0, -s, 0.0, c, 0.0, 0.0, 0.0, 0.0, 1.0); 
}

mat4 rotateZ(float angle){
    float c = cos(angle);
    float s = sin(angle);
  return mat4(c, -s, 0.0, 0.0, s, c, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0);
    
}
mat4 translate(vec3 t){
    
  return mat4(1.0, 0.0, 0.0, -t.x, 0.0, 1.0, 0.0, -t.y, 0.0, 0.0, 1.0, -t.z, 0.0, 0.0, 0.0, 1.0);
    
}

float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}

float sdSphere( vec3 p, float s )
{
  return length(p)-s;
}



vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}





const int numSteps =4;
//-------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
    
    
    float branchSize;

    if( gl_FrontFacing ){
      branchSize = .2;
    }else{
      branchSize = 3.;
    }
    float reductionFactor = .5 + .01 * uParameterA.y;
    float bs = branchSize;

    vec4 p = vec4( pos , 1. );
    mat4 m;
    
  
    vec2 res = vec2( 10000. , 1. );
    
    vec3 t = vec3( uParameterA.x , 1. , uParameterA.x );

    for( int i = 0; i <numSteps; i ++ ){

        bs *= reductionFactor;
        
        float id =  float( i ) / float( numSteps );

        mat4 rot = rotateX( uParameterB.x ) * rotateY( uParameterB.y ) * rotateZ( uParameterB.z );
        
        m = translate( abs(t) * bs  * 2.) * rot;
        
        p.x = abs(p.x);
        p.z = abs(p.z);
        p.y = abs(p.y);

        p = p * m; 

        float smoothA = .02 - .01 * id;
        
        float radius =  bs;
        
        res = smoothU( res , vec2( sdBox( p.xyz , vec3( radius  ) ) , id ) , smoothA );


    }

    return res;
    
}


// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.01, 0.0, 0.0 );
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


  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vCam );

  vec2 res = calcIntersection( ro , rd );


  vec2 light1 = doLight( vLight1 , ro , vNorm , rd );
  vec2 light2 = doLight( vLight2 , ro , vNorm , rd );

  vec3 col = vec3( 0. , 0. , 0. );

  col += doBoxShading( light1 , light2 , vNorm , ro );



  // If we have hit something lets get real!
  if( res.y > -.5 ){

    vec3 pos = ro + rd * res.x;
    vec3 nor = calcNormal( pos );
    float AO = calcAO( pos , nor );

    light1 = doLight( vLight1 , pos , nor , rd );
    light2 = doLight( vLight2 , pos , nor , rd );

    vec3 refr = refract( rd , nor , 1. / 1.1 );

    col += nite * doRayShading( light1 , light2 , nor , ro );

    vec3 refrCol = bgCol( refr * ( MAX_TRACE_DISTANCE - res.x ) + pos , rd  , nite );

    col += (1. - nite ) * refrCol;

  }else{

    col += bgCol( ro + rd * MAX_TRACE_DISTANCE , rd , nite  ) * min( 1.  , ( uTime * 1. ) );

  }

  vec3 initialFade =  vec3( min( vPos.z * 2. + min( uTime * .8 , 3.) * .6  - 1. , 1. ) ) ;
  

  if( uStarted < 1. ){
    col = initialFade; // = vec3( min( vPos.z * 2. + min( uTime * .8 , 3.) * .6  - 1. , 1. ) ) ;
  }else{

    float mixVal = min( 1. , uTime * .5 );
    //float mixVal = min( 1. , (uStarted - .9 ) * 10.));
    col = mix( vec3( 1. ) , col , mixVal );
  }
 
 //col = vCollision;

  col += vec3( max( 0. , .5 - length( vPos - uCollisionPosition ))) * max(0.,(.5 + ( uCollisionTime - uTime )));
  //col = vec3( 1. );
  color = vec4( col , 1. );



}
