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
}; //this functino must have the signature a -> m b

assert(isEquivalent([].ret(2).bind(monadicFunc), monadicFunc(2)), "Monad Law 1");
assert(isEquivalent([1,2,3].bind([].ret), [1,2,3]), "Monad Law 2");
assert(isEquivalent( ([1,2,3].bind(monadicFunc)).bind(function(x){ return [x,x+1]; }),
                     [1,2,3].bind(function(x){ return monadicFunc(x).bind(function(x){ return [x,x+1]; }); }) ), "Monad Law 3");
