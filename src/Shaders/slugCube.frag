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
const int NUM_OF_TRACE_STEPS = 20;
const float PI  = 3.14159;


mat4 palette1;
mat4 palette2;
mat4 palette3;

vec3 light1;
vec3 light2;

mat4 rotateX(float angle){
    
  //angle = -angle/180.0*3.1415926536;
    float c = cos(angle);
    float s = sin(angle);
  return mat4(1.0, 0.0, 0.0, 0.0, 0.0, c, -s, 0.0, 0.0, s, c, 0.0, 0.0, 0.0, 0.0, 1.0);
    
}

mat4 rotateY(float angle){
    
  //angle = -angle/180.0*3.1415926536;
    float c = cos(angle);
    float s = sin(angle);
  return mat4(c, 0.0, s, 0.0, 0.0, 1.0, 0.0, 0.0, -s, 0.0, c, 0.0, 0.0, 0.0, 0.0, 1.0);
    
}

mat4 rotateZ(float angle){
    
  //angle = -angle/180.0*3.1415926536;
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


vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}





const int numSteps =3;
//-------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
    
    
    float branchSize = .04;
    float reductionFactor = .5 + .01 * uTick;
    float bs = branchSize;

    vec4 p = vec4( pos , 1. );
    mat4 m;
    
  
    vec2 res = vec2( 10000. , 1. );
    
    vec3 t = vec3( uParameter1 , uParameter2 , uParameter3 );

    for( int i = 0; i <numSteps; i ++ ){

        bs *= reductionFactor;
        
        float id =  float( i ) / float( numSteps );

        mat4 rot = rotateX( uParameter4 ) * rotateY( uParameter5 ) * rotateZ( uParameter6 );
        
        m = translate( abs(t) * bs * 4.) * rot;
        
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


// From Inigos Simple Color palettes
//http://www.iquilezles.org/www/articles/palettes/palettes.htm
vec3 palette( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d )
{
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 doPalette( in float val , in mat4 pType ){
  return palette( val ,  pType[0].xyz , pType[1].xyz , pType[2].xyz , pType[3].xyz );
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


  float edgeSize = .05 ;// * (1. - uComplete ) + .01;
  if( vUv.x < edgeSize || vUv.x > 1. - edgeSize || vUv.y < edgeSize || vUv.y > 1. - edgeSize ){
    col += vec3( .3 , .3 , .3 );
  }

  return col;

}



// Gets background color if nothing is hit
// also used for refraction
vec3 bgCol( in vec3 p , in vec3 rd ){
 
 vec3 disPos = vec3( p +  sin( p.x * 10. ) + sin( p.y  * 10. ) + sin( p.z * 10.));
 vec3 n = normalize( disPos );
 
 vec2 l1 = doLight( vLight1 , disPos , n , rd );
 vec2 l2 = doLight( vLight1 , disPos , n , rd );

 vec3 col1 = doPalette( .3 + .3 * l1.x , palette1 ) * l1.x;
 vec3 col2 = doPalette( .6 + .3 * l2.x , palette2 ) * l2.x;
 
 vec3 baseCol = vec3( .3 , .3 , .3 );
    
 return col1 + col2;

}

void main(){

  palette1 = mat4( .5  * ( 1. + sin( uTime * .5 ) * .3 ) , .5 * ( 1. + sin( uTime * .5 ) * .3 )  , .5 * ( 1. + sin( uTime * .5 ) * .3 )  , 0. 
                 , .5  * ( 1. + sin( uTime * .8 ) * .3 ) , .5 * ( 1. + sin( uTime * .3 ) * .3 )  , .5 * ( 1. + sin( uTime * .19 ) * .3 )  , 0.
                 , 1.  * ( 1. + sin( uTime * .2 ) * .3 ) , 1. * ( 1. + sin( uTime * .7 ) * .3 )  , 1. * ( 1. + sin( uTime * .4 ) * .3 )  , 0.
                 , .3  * ( 1. + sin( uTime * .1 ) * .3 ) , .2 * ( 1. + sin( uTime * .9 ) * .3 )  , .2 * ( 1. + sin( uTime * 1.5 ) * .3 )  , 0.
                 );

  palette2 = mat4( .5 * ( 1. + sin( uTime * .56 ) * .3 )  , .5 * ( 1. + sin( uTime * .225 ) * .3 )  , .5  * ( 1. + sin( uTime * .111 ) * .3 ) , 0. 
                 , .5 * ( 1. + sin( uTime * 1.5 ) * .3 )  , .5 * ( 1. + sin( uTime * .2 ) * .3 )  , .5  * ( 1. + sin( uTime * .3 ) * .3 ) , 0.
                 , 1. * ( 1. + sin( uTime * .73 ) * .3 )  , 1. * ( 1. + sin( uTime * .15 ) * .3 )  , 0.  * ( 1. + sin( uTime * .74 ) * .3 ) , 0.
                 , .8 * ( 1. + sin( uTime * 1.5 ) * .3 )  , .9 * ( 1. + sin( uTime * .35 ) * .3 )  , .3  * ( 1. + sin( uTime * .9 ) * .3 ) , 0.
                 );


  palette3 = mat4( .5  * ( 1. + sin( uTime * .86 ) * .3 )  , .5 * ( 1. + sin( uTime * .51 ) * .3 )  , .5  * ( 1. + sin( uTime * .2 ) * .3 ) , 0. 
                 , .5  * ( 1. + sin( uTime * 1. ) * .3 )  , .5 * ( 1. + sin( uTime * .76 ) * .3 )  , .5  * ( 1. + sin( uTime * 1.5 ) * .3 ) , 0.
                 , 2.  * ( 1. + sin( uTime * .72 ) * .3 )  , 1. * ( 1. + sin( uTime * .21 ) * .3 )  , 0.  * ( 1. + sin( uTime * .632 ) * .3 ) , 0.
                 , .5  * ( 1. + sin( uTime * .11 ) * .3 )  , .2 * ( 1. + sin( uTime * .06 ) * .3 )  , .25 * ( 1. + sin( uTime * .755 ) * .3 )  , 0.
                 );


  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vCam );

  vec2 res = calcIntersection( ro , rd );


  vec2 light1 = doLight( vLight1 , ro , vNorm , rd );
  vec2 light2 = doLight( vLight2 , ro , vNorm , rd );

  vec3 col = vec3( 0. , 0. , 0. );

  col += doBoxShading( light1 , light2 , ro );



  // If we have hit something lets get real!
  if( res.y > -.5 ){

    vec3 pos = ro + rd * res.x;
    vec3 nor = calcNormal( pos );
    float AO = calcAO( pos , nor );

    light1 = doLight( vLight1 , pos , nor , rd );
    light2 = doLight( vLight2 , pos , nor , rd );

    vec3 refr = refract( rd , nor , 1. / 1.2 );

    vec3 reflCol1= doPalette( .3 + .3 * light1.x, palette1 ) * light1.x;
    vec3 reflCol2= doPalette( .6 + .3 * light2.y, palette2 ) * light2.x;

    vec3 refrCol = bgCol( refr * ( MAX_TRACE_DISTANCE - res.x ) + pos , rd );

    col += reflCol1 + reflCol2 +  refrCol;

  }else{

    col += bgCol( ro + rd * MAX_TRACE_DISTANCE , rd );

  }



  color = vec4( col , 1. );



}
