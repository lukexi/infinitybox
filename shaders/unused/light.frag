#version 330 core

uniform float uID;

out vec4 color;

void main( void ){

  vec3 c1 = vec3( 1. , 0.4 , 0.4 ); 
  vec3 c2 = vec3( .9 , .7 , 0.2 );
  vec3 c3 = vec3( 0.4 , 0.2 , .9 );
  vec3 c4 = vec3( .3 , .7 , 1. ); 

  vec3 col = vec3( 0. );
  if( uID == 0. ){
    col = c1;
  }else if( uID == 1. ){
    col = c2;
  }else if( uID == 2. ){
    col = c3;
  }else{
    col = c4;
  }
  
  color = vec4( col , 1. );

}
