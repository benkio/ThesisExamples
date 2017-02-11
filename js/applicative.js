
/*

  class Functor f => Applicative f where
    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b

    the purpose of applicative is to provide a way to lift a value to the computational context with pure
    perform sequential computation in the context. Often it's only a way to hide the lifting and the application
    of the fmap, but it's a necessary step through monad concept.

 */


function Applicative() {
    this.pure  = function(a){};
    this.apply = function(liftedFunc){};
}

Array.prototype.pure  = function(a){ return [a]; }; // To be explicit, we can simply use [a] instead of [].pure(a).
Array.prototype.apply = function(arrayOfFunc) {
    if (arrayOfFunc.length == 0) return [];
    var arrayFmapped = this.fmap(arrayOfFunc[0]);
    var arrayOfFuncTail = arrayOfFunc.slice(1);
    var result = arrayFmapped.concat(this.apply(arrayOfFuncTail));
    return result;
};

/******************** APPLICATIVE LAW PROOF  **********************/

var arrayOfFunc1 = [addOne, square];
var arrayOfFunc2 = [multByTwo, minusOne];

assert(isEquivalent(arrayExample.apply([].pure(id)), id(arrayExample)), "Applicative Identity Law");
assert(isEquivalent([].pure(2).apply([].pure(addOne)), [].pure(addOne(2))), "Homomorphysm Law");
assert(isEquivalent([].pure(2).apply(arrayOfFunc1), arrayOfFunc1.apply([].pure(functionApplication(2)))), "Interchange Law" );
assert(isEquivalent([].pure(2).apply(arrayOfFunc1).apply(arrayOfFunc2),
                    [].pure(2).apply(arrayOfFunc1.apply(arrayOfFunc2.apply([].pure(functionComposition))))), "Associativity Law");

/********************* APPLICATIVE EXAMPLE ***********************************/

console.log("---------Applicative example-------------");

console.log("The type applicative must have a way to lift a sigle value, eg in lists from 2 to: " + [].pure(2));

console.log("With applicative we can easily apply a list of function to a list of value(same list context): " + arrayExample.apply(arrayOfFunc1) );

console.log("Finally we can chain computations, applying other list of functions: " + arrayExample.apply(arrayOfFunc1).apply(arrayOfFunc2) );
