/*
  class Functor f where
    fmap :: (a -> b) -> f a -> f b

  fmap Allow to execute the pure function inside the functor keeping the same functor structure.
  eg. a list of 3 value will be always be a list of 3 value using fmap.

  Often the idea of functor is skipped because the languanges hide it providing directly the fmap
  operation, like in the example below for the array.map
*/
function Functor () {
    this.fmap  = function(purefunc){};
};


"use strict";

/* Functions For Proof Purposes */
function id (a){ return a; };
function functionApplication(a) { return function(f){ return f(a) }; };
function functionComposition(f) { return function(g){ return function(a){ return f(g(a)); }; }; }; //considering f and g of one argument

/*  Pure Functions */
function addOne (a){ return a + 1; };
function minusOne(a) { return a - 1; };
function multByTwo(a) { return a * 2; };
function square (a){ return a * a; };
function squareAndAddOne (a){ return addOne(square(a)); };

/******************** LIST FUNCTOR EXAMPLE ******************************/

var arrayExample = [2,4];

Array.prototype.fmap = function (purefunc) {
    if (this.length == 0) return [];
    var result = this.slice(1);
    result = result.fmap(purefunc);
    result.unshift(purefunc(this[0]));
    return result;
};

/******************* FUNCTOR LAW PROOF ************************************/

assert(isEquivalent(arrayExample.fmap(id), id(arrayExample)), "law 1 array");

assert(isEquivalent(arrayExample.fmap(squareAndAddOne), arrayExample.fmap(square).fmap(addOne)), "law 2 array");

/********************* FUNCTOR EXAMPLE ***********************************/

console.log("---------Functor Example Array-------------")

console.log("Initial Array value: " + arrayExample);

console.log("Functor value after addOne and Square functions: " + arrayExample.fmap(addOne).fmap(square));

console.log("Same previous result using directly the map operation: " + arrayExample.map(addOne).map(square));

/******************* STATE FUNCTOR ***************************************/

var stack = new State(function(initialStack){ return [1, initialStack]; } );

function State(stateFunc){
    this.stateFunc = stateFunc;
    this.fmap = function(func){ //with fmap there not effects on the state, it's only passed along the computation
        return (new State(function(state){
            var [a, newState]   = stateFunc(state);
            var newValue = func(a);
            return [newValue, newState]
        }))
    };
    this.execState = function(state) {
        return this.stateFunc(state);
    };
};

/******************** FUNCTOR LAW PROOF ********************************/

/*

  don't work because it's difficult to compare two states.

assert(isEquivalent(stack.fmap(id), id(stack)), "law 1 state");

assert(isEquivalent(stack.fmap(squareAndAddOne), stack.fmap(square).fmap(addOne)), "law 2 state");
*/

/******************* STATE FUNCTOR EXAMPLE *******************************/

console.log("---------Functor Example State-------------")

console.log("Initial State value: " + JSON.stringify(stack.execState([0])));

var stackAfterFmap = stack.fmap(square).fmap(addOne);

console.log("state functor applyied to two functions and executed: " + JSON.stringify(stackAfterFmap.execState([0])) );
