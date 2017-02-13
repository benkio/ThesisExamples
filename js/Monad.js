/*

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
*/

function Monad(){
    this.bind = function(monadFunc){};
    this.then = function(monadValue){}; //optional but useful
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

console.log("------------------------- Monad State(stack) Example --------------------------");

console.log(" Perform a monad computation using the stack: " + JSON.stringify(stack.bind(stateFunc1).bind(stateFunc1).bind(stateFunc2).then(stateThen).then(stateThen2).execState([1,2,3]) ) );

console.log(" Perform another monad computation using the stack: " + JSON.stringify(stack.then(stateThen3).then(stateThen).bind(stateFunc1).execState([5])));
