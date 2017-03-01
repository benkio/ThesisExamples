/*

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
*/

function Monad(){
    this.bind = function(monadFunc){};
    this.then = function(monadValue){}; //optional but useful, can be created using the bind.
}

/************************ Array Monad Example *******************************************/

Array.prototype.ret = Array.prototype.pure;
Array.prototype.bind = function(monadFunc){
    return Array.prototype.concat.apply([],this.fmap(monadFunc)); //the flatmap https://gist.github.com/samgiles/762ee337dff48623e729
}
Array.prototype.then = function(monadValue){
    // In this situation we have to also iterate trought the element of the
    // Current array, but all the element would be discarted anyway
    //  xs >> ys  = [y | _ <- xs, y <- ys]
    return monadValue; 
};

/************************* Monad Laws **************************************************/

var monadicFunc = function(value) {
    var N = 10;
    var oneToTenArray = Array.apply(null, {length: N}).map(Number.call, Number);
    return  oneToTenArray.fmap(function(a){ return value + a; });
}; //this function must have the signature a -> m b

var monadicFunc2 = function(x) {
    return [x, x+1];
}

assert(isEquivalent([].ret(2).bind(monadicFunc), monadicFunc(2)), "Monad Law 1");
assert(isEquivalent([1,2,3].bind([].ret), [1,2,3]), "Monad Law 2");
assert(isEquivalent( ([1,2,3].bind(monadicFunc)).bind(monadicFunc2),
                     [1,2,3].bind(function(x){ return monadicFunc(x).bind(monadicFunc2); } )), "Monad Law 3");


/************************** Array Monad Example ***********************************/

       console.log("-----------------Monad Example Array-------------------");

       console.log("Monadic computation applying two function from a->m a to a value: " + [].ret(2).bind(monadicFunc).bind(monadicFunc));

       console.log("The `then` operation will perform the previous calculation and then throw away the result leaving the then argument: " + [].ret(2).bind(monadicFunc).bind(monadicFunc).then([].ret(3)));


/**************************** State Monad Example  *****************************************/

State.prototype.ret  = State.prototype.pure;
State.prototype.bind = function(monadFunc) { 
    var OutStateObject = this;
    return new State(function(state) {
        var [v, s] = OutStateObject.execState(state);
        return monadFunc(v).execState(s);
    });
}; 
State.prototype.then = function(monadValue) {
    var OutStateObject = this;
    return new State(function(state){
        var[v, s] = OutStateObject.execState(state);
        return monadValue.execState(s); //the previous value v is not used to compute the new state, check out the signature of then.
    });
};

var stateFunc1 = function(value){
    var result = addOne(value);
    return new State(function(stack) {
        stack.push(2);
        stack.push(4);
        return [result, stack];
    });
};

var stateFunc2 = function(value){
    var result = square(value);
    return new State( function(stack) {
        stack.push(2);
        stack.push(4);
        return [result, stack];
    });
};

var stateThen = new State(function(stack) {
    var v1 = stack.pop();
    var v2 = stack.pop();
    return [v1+v2, stack];
});

var stateThen2 = new State(function (stack) {
    var stack2 = stack.fmap(addOne);
    return [0, stack2];
} );

var stateThen3 = new State(function(stack) {
    stack.push(6);
    stack.push(8);
    return [1, stack];
});

var incCounter = new State(function(counter) {
    return [counter +1, counter+1];
});

var decCounter = new State(function(counter) {
    return [counter -1, counter-1];
});

console.log("------------------------- Monad State(stack) Example --------------------------");

console.log(" Perform a monad computation using the stack: " + JSON.stringify(stack.bind(stateFunc1).bind(stateFunc1).bind(stateFunc2).then(stateThen).then(stateThen2).execState([1,2,3]) ) );

console.log(" Perform another monad computation using the stack: " + JSON.stringify(stack.then(stateThen3).then(stateThen).bind(stateFunc1).execState([5])));

console.log(" We can still apply some pure function inside the monad using its fmap function" + JSON.stringify(stack.then(stateThen3).fmap(addOne).fmap(square).bind(stateFunc2).execState([1,2,3])) );

console.log("----------------------- Counter example (using the state monad) ------------------");

console.log("inc two times: " + JSON.stringify( stack.then(incCounter).then(incCounter).execState(0)  ));

console.log("inc two times and dec one: " + JSON.stringify( stack.then(incCounter).then(incCounter).then(decCounter).execState(0)  ));

// At this point we can build some infrastructure to hide the binding and then and make that automatic based on type...ops there are no types in js :(
// The do notation in Haskell knows if a function perform some effects based on the return type, so if it's a pure function(not returning a monad) it use the fmap to combine
// Then the bind or the then is used. 

/*************************  State Monad 110 Recognitor  *************************************/

console.log("----------------------- 110 recognizer example (using the state monad transformer) ------------------");

// Simulate the input from console, the return type would be IO Int
var i = 0;
var inputSimulator = function() {
    i = i+1;
    if (i < 100){
        var x = Math.random() <= 0.5 ? "0" : "1";
        console.log("input value: " + x);
        return x;
    } else {
        console.log("exit");
        return "x";
    };
};

// HERE I HAVE TO INSERT THE MONAD TRANSFORMER IN ORDER TO PERFORM SOME OUTPUT

// The innerMonadicFunc is a function from another monad. in our case IO
State.prototype.lift = function(innerMonadicFunc) {
    return new State(function(state) {
        var value = innerMonadicFunc();
//        console.log("lift: " + value + ", " + state);
        return [value, state];
    });
};

// Take the function that operate only on the state and apply it.
// The inner value is discarded
var modify = function(functionStateModifier) {
    return new State(function(state){
//        console.log("modify: "+ " state " + state + " " + functionStateModifier(state));
        return [undefined, functionStateModifier(state)];
    });
};

//Throw away the inner value and replace it with the state.
var get = new State(function(x){ return [x,x]; } );

// State -> IO ()
var machineFunction = function(state){
    switch( state ){ //recognizer of 1 1 0
    case "s1" : console.log(" state s1 output from MachineFunction: 0 ");break;
    case "s2" : console.log(" state s2 output from MachineFunction: 0 ");break;
    case "s3" : console.log(" state s3 output from MachineFunction: 0 ");break;
    case "s4" : console.log(" state s4 output from MachineFunction: 1 ");break;
    }
};

// a -> State -> State
var stateFunction = function(v,state){ //recognizer: 1 1 0
    var newState;
    switch( state ){
    case "s1" : newState = v == 0 ? "s1" : "s2";break;
    case "s2" : newState = v == 0 ? "s1" : "s3";break;
    case "s3" : newState = v == 0 ? "s4" : "s1";break;
    case "s4" : newState = v == 0 ? "s1" : "s2";break;
    };
    return newState;
};

var recognizer = new State(function(state) {
//    console.log("recognizer input state: " + state);
    return get.lift(inputSimulator).bind(function(value){
//        console.log("recognizer input value: "+ value);
        if (value == 'x') return new State(function(s) { return [0, s];});
        else {
            //console.log(stateFunction);
            return modify(function(state2) { return stateFunction(parseInt(value), state2); })
                      .then(get)
                      .bind(function(value2){
//                          console.log("lift: " + value2 );
                          return get.lift(function() { machineFunction(value2); });
                      })
                      .then(recognizer);
        }
    }).execState(state);
});

//recognizer.execState("s1");


/************************ RECOGNIZER 110 with input provided at function call  **********************************/


var mtnFunc = function(state){
    switch( state ){ //recognizer of 1 1 0
    case "s1" : return 0;
    case "s2" : return 0;
    case "s3" : return 0;
    case "s4" : return 1;
    }
};

var stFunc = function(v,state){ //recognizer: 1 1 0
    var newState;
    switch( state ){
    case "s1" : newState = v == 0 ? "s1" : "s2";break;
    case "s2" : newState = v == 0 ? "s1" : "s3";break;
    case "s3" : newState = v == 0 ? "s4" : "s1";break;
    case "s4" : newState = v == 0 ? "s1" : "s2";break;
    };
    return newState;
};

var outsideInputFunc = function(v) {
    return new State(function(state) {
        return [mtnFunc(stFunc(v, state)), stFunc(v, state)];
    });
};

var one  = outsideInputFunc(1);
var zero = outsideInputFunc(0);
var initialState = new State(function(initialStack){ return [0, initialStack]; } );

var recognizer2 = new State(function(state){
    return initialState.then(one).then(one).then(zero).execState(state); // not executed yet
});

console.log("exec recognizer with state s1");
console.log(recognizer2.execState("s1")); // executed => [1, s4]

