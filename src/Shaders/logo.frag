#version 330 core

uniform float uTime;
uniform float uTick;

uniform float uFilledness;
uniform float uComplete;

uniform float uParameter1;
uniform float uParameter2;
uniform float uParameter3;
uniform float uParameter4;
uniform float uParameter5;
uniform float uParameter6;


in vec3 vPos;
in vec3 vCam;
in vec3 vNorm;

in vec3 vLight1;
in vec3 vLight2;

in vec2 vUv;

out vec4 color;



const float MAX_TRACE_DISTANCE = 5.;           // max trace distance
const float INTERSECTION_PRECISION = 0.001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 50;
const float PI  = 3.14159;

float breath;
float branchFade;

const int numSteps =4;

float fadeOutSpeed = .03;
float fadeInSpeed = .1;

float city2Tree = .6;
float treeBreath = .2;
float tree2City = 1. - city2Tree - treeBreath;

float speed = .1;

float branchSize =.3;
float reductionFactor = .5;
//float bs = branchSize;
float rot = -20.;

float gTime;


float smin( float a, float b, float k )
{
    float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}


float smax( float a, float b, float k )
{
    float h = clamp( 0.5+0.5*(a-b)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}

float getBreath(float  time ){
    
    // Gettin our time normalized for 
    // the course of the breath
    float t = max( 0. , time - fadeInSpeed);
    t /= 1. - fadeInSpeed;
    t = min( 1. , t );
    t /= (1.- fadeOutSpeed);
    t = min( 1. , t );

    
    //Get city 2 tree transisition
    float c2t = clamp( t / city2Tree , 0. , 1. );
    
    float tb = clamp(  ( t - city2Tree ) / treeBreath , 0. , 1. ); 
    float t2c = clamp( ( t - city2Tree - treeBreath ) / tree2City , 0. , 1. );
      

    float breath = 0.;// t * t * t;
    breath += pow( c2t , 6. ) * .8 + c2t * c2t *.2;
    breath += pow( tb , .6 ) * .6;
    
    breath -= pow( t2c , 5. ) * 1.6;
    breath = max( breath , 0. );
   
    return breath;
    
}

float getFade(float time ){
    
    //Fade in Value
    float inT = min( 1. , time / fadeInSpeed );
    
    
    float outT = min( 1. , ((1. - time ) / fadeOutSpeed));
   
    inT = pow( inT , .8 );

    return min( inT , outT);

}



mat4 rotateX(float angle){
    
  angle = -angle/180.0*3.1415926536;
    float c = cos(angle);
    float s = sin(angle);
  return mat4(1.0, 0.0, 0.0, 0.0, 0.0, c, -s, 0.0, 0.0, s, c, 0.0, 0.0, 0.0, 0.0, 1.0);
    
}

mat4 rotateY(float angle){
    
  angle = -angle/180.0*3.1415926536;
    float c = cos(angle);
    float s = sin(angle);
  return mat4(c, 0.0, s, 0.0, 0.0, 1.0, 0.0, 0.0, -s, 0.0, c, 0.0, 0.0, 0.0, 0.0, 1.0);
    
}

mat4 rotateZ(float angle){
    
  angle = -angle/180.0*3.1415926536;
    float c = cos(angle);
    float s = sin(angle);
  return mat4(c, -s, 0.0, 0.0, s, c, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0);
    
}
mat4 translate(vec3 t){
    
  return mat4(1.0, 0.0, 0.0, -t.x, 0.0, 1.0, 0.0, -t.y, 0.0, 0.0, 1.0, -t.z, 0.0, 0.0, 0.0, 1.0);
    
}



vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}


float udBox( vec3 p, vec3 b )
{
  return length(max(abs(p)-b,0.0));
}

float sdSphere( vec3 p, float s )
{
  return length(p)-s;
}

//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
    

    mat4 m;
    float bs = branchSize;

    vec4 p = vec4( pos , 1. );
    

    p = p * rotateY( (gTime ) * 360.);
    p = p * translate( vec3( 0. , -breath * bs * .25 , 0. ) );
    
    vec2 res = vec2(udBox( p.xyz , vec3(  bs / 10. ,  bs *  branchFade * 1.4- bs * .4  , bs / 10. )) ,1.);

     
    for( int i = 0; i <numSteps; i ++ ){
        
        float id = float( i ) / float( numSteps);
        
        bs *= reductionFactor;

        vec3 tVec = vec3( 0.  , bs * ( breath  * 2.  )* ( 4. + float( i ))/8.   , 0. );
        m = translate( tVec  ) * rotateZ( rot * breath) * rotateX( -rot * breath );    
   

        p.x = abs(p.x)-bs/2.;
        p.z = abs(p.z)-bs/2.;  

        p = p * m; 
        
        float box = udBox( p.xyz , vec3(  bs / 10. , bs *  branchFade * 1.4- bs * .4, bs / 10. ));  
        
        float smoothA = .2 * (1. - id * .5) * ( breath * 1.5 + .2 ) * bs;
        res = smoothU( res , vec2( box , id ) ,  smoothA );


    }

    res = smoothU( res , vec2( sdSphere(pos - vLight1, .1 ) , 1. ) , .5);
    res = smoothU( res , vec2( sdSphere(pos - vLight2, .1 ) , 1. ) , .5);

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

vec3 palette( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d )
{
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 doPalette( in float val , in mat4 pType ){
  return palette( val ,  pType[0].xyz , pType[1].xyz , pType[2].xyz , pType[3].xyz );
}




void main(){
   
    vec3 ro = vPos;
    vec3 rd = normalize( vPos - vCam );

    float time = max( 0. ,uTime * speed);
    
    gTime = time;
    breath = getBreath(time);
    branchFade =getFade(time );
    
    vec2 res = calcIntersection( ro , rd  );
    
    vec3 col = vec3(0. ); 
    
        // If we have hit something lets get real!
    if( res.y > -.5 ){
   
        vec3 pos = ro + rd * res.x;
        vec3 nor = calcNormal( pos );
        float AO = calcAO( pos , nor );
        
      
        col = nor * .5 + .5;
        col = vec3( AO );
    }else{
        discard;
    }
    
    // apply gamma correction
    col = pow( col, vec3(0.4545) );

    if( .5 - abs(vUv.x - .5) < .01 ){ col = vec3( 1. ); }
    if( .5 - abs(vUv.y - .5) < .01 ){ col = vec3( 1. ); }


    color = vec4( col , 1. );



}