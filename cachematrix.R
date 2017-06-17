## This function simply cache the inverse matrix object of a given matrix 


makeCacheMatrix <- function(b = matrix()) {
    cnt <- NULL
    set <- function(a){
        b <<- a
        cnt <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Desolve the matrix cached by makeCacheMatrix function

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)){
        message("retrieving cached matrix")
        return(inv)
    }
    cached <- x$get()
    inv <- solve (cached, ...)
    x$setInverse(inv)
    inv
}
