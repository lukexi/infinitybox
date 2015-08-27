#version 330 core

uniform float uTime;

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
        p += (dg+time*.1*spd);

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



// Using SDF from IQ's two tweet shadertoy : 
// https://www.shadertoy.com/view/MsfGzM
float sdBlob( vec3 p ){

  return length(
    .05 * cos( 9. * (sin( uParameter1 )+ 1.) * p.y * p.x )
    + cos(p) * (sin( uParameter2 ) * .01 + 1.) 
    -.1 * cos( 9. * ( p.z + .3 * (sin(uParameter3) + 1.)   * p.x - p.y * (sin( uParameter4 )+ 1.)   ) ) )
    -1.; 

}

float cubeField( vec3 p ){

  float fieldSize = 1.  + abs( sin( uParameter5) ) * 1.;
  return opRepBox( p , vec3(fieldSize ), .01 + uParameter4 * .05  );

}

float sphereField( vec3 p ){

  float fieldSize = 1.  + abs( sin( uParameter5) ) * 1.;
  return opRepSphere( p , vec3(fieldSize ), .01 + uParameter4 * .05 );

}

float sdBlob2( vec3 p ){
 
  vec3 pos = p;

  return length( p ) - .2 + .3 * .2 * sin( uParameter4 )*sin(300.0 * sin(uParameter1 ) *pos.x * sin( length(pos) ))*sin(200.0*sin( uParameter2 ) *pos.y )*sin(50.0 * sin( uParameter3 * 4. )*pos.z);

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
    f = f + .005 * uParameter1 * abs( sin( pos.x * 300. * uParameter2 ) * sin( pos.y * 300. * uParameter3 ) );
                       
    return f;
    
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

  float noiseS = sin( uParameter3 * 1. ) * 3. + 4.;
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

  float nSpec= pow( spec , abs(sin(uParameter1 * 1.1))* 10. + 2. );
  return
      vec3( 1. , .6 , .2 ) * hsv( uParameter6 , 1. , 1. ) *  lamb //hsv( lamb , abs( sin( uParameter6 )) * .2 + .6 , abs( sin( uParameter2 ) * .4 + .6 )) * lamb 
    + vec3( .3 , .6 , 1. ) *  hsv( uParameter5 , 1. , 1. ) * nSpec;// hsv( nSpec , abs( sin( uParameter5 )) * .4 + .6 , abs( sin( uParameter4 ) * .3 + .8 )) * nSpec;
}*/


vec3 doCol( float lamb1 , float spec1  , float lamb2 , float spec2){

  float nSpec1= pow( spec1 , abs(sin(uParameter1 * 1.1))* 10. + 2. );
  float nSpec2= pow( spec2 , abs(sin(uParameter1 * 1.1))* 10. + 2. );
  return
      .5 *  hsv( lamb1  * .3 + uParameter2 , abs( sin( uParameter6 )) * .2 + .6 , abs( sin( uParameter2 ) * .4 + .6 )) * lamb1 
    +  hsv( nSpec1 * .6 + uParameter3 , abs( sin( uParameter5 )) * .4 + .6 , abs( sin( uParameter4 ) * .3 + .8 )) * nSpec1
    + .5 *  hsv( lamb2  * .3 + uParameter2 , abs( cos( uParameter6 )) * .2 + .6 , abs( cos( uParameter2 ) * .4 + .6 )) * lamb2
    +  hsv( nSpec2 * .6 + uParameter3 , abs( cos( uParameter5 )) * .4 + .6 , abs( cos( uParameter4 ) * .3 + .8 )) * nSpec2;
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

  color = vec4( col , 1. );


}
