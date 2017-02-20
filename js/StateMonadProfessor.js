------------------------------------------------------------------------------------------------
function initRecogn(v){
return new State( v, "s0");  
 
}
function State(value, state){
this.data = [value,state];  
 
}
/*
 * The monad relates a value to a state according to its own  
 * state function (sfn) and machine function (mfn)
 */
  State.prototype.bind = function( func ) {
  return func( this.data[1] );
  };
State.prototype.fmap = function( func ) {
  var newVal      =  func( this.data[0] ); 
  var newState
=  this.sfn( newVal, this.data[1]  );
var ootput      =  this.mfn( newVal, newState ); 
console.log("newVal=" + newVal + " newState=" + newState + " ootput=" + ootput); 
  return new State( ootput, newState ); 
};
State.prototype.toString = function() {
return '' + this.data[0]  + ' |  state=' + this.data[1];
};
  State.prototype.sfn = function(v,state){ //recognizer: 1 1 0
var newState;
switch( state ){ 
case "s0" : newState = v == 0 ? "s0" : "s1";break;
case "s1" : newState = v == 0 ? "s0" : "s2";break;
case "s2" : newState = v == 0 ? "s3" : "s0";break;
case "s3" : newState = v == 0 ? "s0" : "s0";break;
default: newState = "s0";
}
//console.log( "sfn newState=" + newState  ); 
return newState; 
}
State.prototype.mfn = function(v,state){
var output;
switch( state ){ //recognizer of 1 1 0
case "s0" : output = 0;break;
case "s1" : output = 0;break;
case "s2" : output = 0;break;
case "s3" : output = 1;break;
default: output = "0";
}
return  output; 
}

 
/*
 * Using Recognizer with fmap
 */ 
function zero(){ return 0;}
function uno(){ return 1; }
var s0 = initRecogn( 0 );
console.log("s0=" ,  s0 );
console.log("01110=" ,  s0.fmap(zero).fmap(uno).fmap(uno).fmap(uno).fmap(zero) );  //01110= State { data: [ 0, 's0' ] }
console.log("1111=" ,   s0.fmap(uno).fmap(uno).fmap(zero)  );                      //1111= State { data: [ 1, 's3' ] }
console.log("1111=" +  s0.fmap(uno).fmap(uno).fmap(zero)  );                       //1111= State { data: [ 1, 's3' ] }

console.log("===================================");
/*
 * Using Recognizer with bind
 */ 
 var applsfn = function(v,state){ //recognizer: 1 1 0
var newState;
switch( state ){ 
case "s0" : newState = v == 0 ? "s0" : "s1";break;
case "s1" : newState = v == 0 ? "s0" : "s2";break;
case "s2" : newState = v == 0 ? "s3" : "s0";break;
case "s3" : newState = v == 0 ? "s0" : "s0";break;
default: newState = "s0";
}
//console.log( "applsfn state=" + state + " newState="+newState  ); 
return newState; 
}
var applmfn = function(v,state){
var output;
switch( state ){ //recognizer of 1 1 0
case "s0" : output = 0;break;
case "s1" : output = 0;break;
case "s2" : output = 0;break;
case "s3" : output = 1;break;
default: output = "0";
}
return  output; 
}
/*
 * Application functions that return a monad
 */
function input( v, state ){ 
var newState =  applsfn(v,state);
return new State( applmfn(v,newState), newState ); 
} 
function input0(state) { return input(0,state); }
function input1(state) { return input(1,state); }
 /*
 * Computations
 */ 
var ss0 = initRecogn( 0 );
var res1 = ss0.bind( input1 ).bind( input1 ).bind( input0 );  
console.log("110= ",  res1);
//110=  State { data: [ 1, 's3' ] }  
console.log("110= " + res1);
//110= 1 |  state=s3
 
console.log("100= " + ss0.bind( input1 ).bind( input0 ).bind( input0 ));  //110= 0 |  state=s0
