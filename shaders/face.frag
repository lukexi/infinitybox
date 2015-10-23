#version 330 core

uniform float uTime;

uniform vec3 uParameterA;
uniform vec3 uParameterB;

in vec3 vPos;
in vec3 vCam;
in vec3 vNorm;

in vec3 vLight1;
in vec3 vLight2;


in vec2 vUv;

out vec4 color;



const float MAX_TRACE_DISTANCE = 2.;           // max trace distance
const float INTERSECTION_PRECISION = 0.001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 20;




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

float sdCappedCone( in vec3 p, in vec3 c )
{
    vec2 q = vec2( length(p.xy), -p.z - c.z );
    vec2 v = vec2( c.z*c.y/c.x, -c.z );

    vec2 w = v - q;

    vec2 vv = vec2( dot(v,v), v.x*v.x );
    vec2 qv = vec2( dot(v,w), v.x*w.x );

    vec2 d = max(qv,0.0)*qv/vv;

    return sqrt( dot(w,w) - max(d.x,d.y) )* sign(max(q.y*v.x-q.x*v.y,w.y));
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


float smax(float a, float b, float k)
{
    return log(exp(k*a)+exp(k*b))/k;
}

vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}


float opS( float d1, float d2 )
{
    return max(-d1,d2);
}


vec2 opS( vec2 d1, vec2 d2 )
{
   return (-d1.x>d2.x) ? vec2( -d1.x , d1.y ) : d2;
}




vec2 smoothSub( vec2 d1, vec2 d2, float k)
{
    return  vec2( smax( -d1.x , d2.x , k ) , d2.y );
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

  float fieldSize = 1.  + abs( sin( uParameterB.y) ) * 1.;
  return opRepSphere( p , vec3(fieldSize ), .01 + uParameterB.x * .05 );

}

float sdBlob2( vec3 p ){
 
  vec3 pos = p;

  return length( p ) - .2 + .3 * .2 * sin( uParameterB.x )*sin(300.0 * sin(uParameterA.x ) *pos.x * sin( length(pos) ))*sin(200.0*sin( uParameterA.y ) *pos.y )*sin(50.0 * sin( uParameterA.z * 4. )*pos.z);

}

float smin_2_3(float a, float b, float k) {
  float h = clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0);
  return mix(b, a, h) - k * h * (1.0 - h);
}
float sdTorus( vec3 p, vec2 t )
{
  vec2 q = vec2(length(p.xy)-t.x,p.z);
  return length(q)-t.y;
}

float sdCapsule( vec3 p, vec3 a, vec3 b, float r )
{
    vec3 pa = p - a, ba = b - a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return length( pa - ba*h ) - r;
}


float face( vec3 pos , vec3 p ){
    
    vec3 headPos  = ( pos - p );
    vec3 nosePos  = ( pos - p );
    vec3 lePos    = ( pos - p + vec3( -.1 , -.1 , 0.));
    vec3 rePos    = ( pos - p + vec3( .1 , -.1 , 0.));
    vec3 mouthPos = ( pos - p + vec3( 0. , .12 , 0.));

    
    float nose = sdCappedCone( nosePos , vec3( .1 , .04 , .1 ) );
    float head = udBox( headPos , vec3( .25 , .35 , .01 ));
    
    float re = sdSphere( rePos , .03 );
    float le = sdSphere( lePos , .03 );
    
    float mouth = sdTorus( mouthPos , vec2( .04 , .02 ));

    float f = head;
    
    f = smin_2_3( f , nose  , .04 );
    f = smin_2_3( f , le    , .04 );
    f = smin_2_3( f , re    , .04 );
    f = smin_2_3( f , mouth , .04 );
    f = f + .005 * uParameterA.x * abs( sin( pos.x * 300. * uParameterA.y ) * sin( pos.y * 300. * uParameterA.z ) );
                       
    return f;
    
}

float doRing( vec3 p  , float size ){
  
    vec3 pos = p;
    
     
    float lor = sign( pos.x );
    
    
    pos.x = abs( pos.x );
    
    //pos.x = mod( pos.x , 1.5 );
    float degree = atan( pos.y , pos.z );
    
    float ogD = degree;
    
    degree += uTime;// * (1. + lor * .2);
    float l = length( pos.yz );
    

    
    degree = mod( degree - 3.14159  / 8. , 3.14159  / 4. );
 
    
    pos.y = l * sin( degree );
    pos.z = l * cos( degree );

    
    return sdSphere( pos - vec3( 2.4 ,  1. , 2.39 )  * .2 * size , .04 *( 1. + abs(ogD) ) * size) ;
    
}


//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){

vec3 og = pos;  
  pos.z = -pos.z;
  pos -= vec3( 0. , -.015 , -.025 );
  //pos.z -= .5;
   
    float size = .1;
   //pos *= 2.5;
  // pos -= vec3( 0. , 0., .5 );
    
    vec2 ring;


    vec2 res = vec2( sdSphere( pos , 1. * size  ) , 1. );
    
   
    vec2 mouth = vec2( sdSphere( pos - vec3( 0. , -.6 , 1. ) *size , .3 * size ) , 2. );
    
    //res.x = smax( res.x , -mouth.x  , 10.1 );//
      
 
    
    res = opS( mouth , res );
    
    
        
    ring = vec2( doRing( pos , size  ) , 2. );
   
    
    res.x = smax( res.x , -ring.x  ,  4. / size );//
    res = opU( ring , res );
    
    float lor = sign( pos.x );
    
    pos.x = abs( pos.x );

    ring =  vec2( sdCapsule( pos , vec3( 0.) , vec3( .3 , .5 , .6 ) * 2. * size ,  .1 * size ) , 2. );
    res = smoothU( res, ring , .2 * size);
    
    ring =  vec2( sdSphere( pos - vec3( 0.2  , .2,  .7 ) * 1.4 * size , .1 * size ) , 2. );
    res = opU( ring , res);

    res.x = smin( res.x , sdSphere(og - vLight1, .1 ));
    res.x = smin( res.x , sdSphere(og - vLight2, .1 ));
    
      
    return res;
    
}
/*


//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
   
    // using super thin cube as plane
    vec3 size = vec3( 1.  , 1. , .01 );
   // vec3 rot = vec3( iGlobalTime * .1 , iGlobalTime * .4 , -iGlobalTime * .3 );
    vec3 rot = vec3( 0.,0.,0. );
   // vec2 res = vec2( rotatedBox( pos , rot , size , .001 ) , 1.0 );
    
    float repSize = ( uParameterA.x * .4 + .4) * 2.;
    repSize = 2.;

    float radius = .4 * uParameterA.y  + .1;

    radius = .01;

   // vec2 res = vec2( opRepSphere( pos , vec3( repSize ) , radius ) , 1. );
    //vec2 res = vec2( sdSphere( pos ,  radius ) , 1. );
    vec2 res;
    if( gl_FrontFacing ){
       res = vec2( face( pos , vec3( 0. , 0. , 0.06 ) ) , 1. );
       res.x = smin( res.x , sdSphere(pos - vLight1, .1 ));
       res.x = smin( res.x , sdSphere(pos - vLight2, .1 ));
    }else{
      res = vec2( sphereField( pos ) , 2. );
    }
    return res;
    
}
*/

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




/*vec3 doCol( float lamb , float spec ){

  float nSpec= pow( spec , abs(sin(uParameterA.x * 1.1))* 10. + 2. );
  return
      vec3( 1. , .6 , .2 ) * hsv( uParameterB.z , 1. , 1. ) *  lamb //hsv( lamb , abs( sin( uParameterB.z )) * .2 + .6 , abs( sin( uParameterA.y ) * .4 + .6 )) * lamb 
    + vec3( .3 , .6 , 1. ) *  hsv( uParameterB.y , 1. , 1. ) * nSpec;// hsv( nSpec , abs( sin( uParameterB.y )) * .4 + .6 , abs( sin( uParameterB.x ) * .3 + .8 )) * nSpec;
}*/






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


  vec3 col = rd * .5 + .5;
  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;

    vec3 lightDir1 = normalize( vLight1 - pos);
    vec3 lightDir2 = normalize( vLight2 - pos);
    vec3 norm;

    
    norm = calcNormal( pos );
    

    vec3 reflDir1 = reflect( lightDir1 , norm );
    vec3 reflDir2 = reflect( lightDir2 , norm );

    float lamb1 = max( dot( norm , lightDir1 ), 0.);
    float spec1 = max( dot( reflDir1 , rd ), 0.);

    float lamb2 = max( dot( norm , lightDir2 ), 0.);
    float spec2 = max( dot( reflDir2 , rd ), 0.);


    //col = lamb * vec3( 1. , 0. , 0. ) + pow( spec , 10.) * vec3( 0. , 0. , 1. );// norm * .5 +.5;
    //col = doCol( lamb1 , spec1 , lamb2 , spec2 );

    col = norm * .5 +  .5;
  }

  if( vUv.x < .05 || vUv.x > .95 || vUv.y < .05 || vUv.y > .95 ){
    //col = doCol( lamb1 , spec1 , lamb2 , spec2 );
    col += vec3( .3 , .3 , .3 );
  }

  //if( length( col ) < .001 ){ discard; }

  color = vec4( col , 1. );


}
