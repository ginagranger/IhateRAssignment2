## It takes the argument of the matrix and returns its inverse. First it sets
## the value of the matrix, with the use of double operators. Than it get the  
## value of the matrix,invert it and get the value o the inverse.

## 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function () {inv}
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


## It ruturns a matrix that is the inverse of 'x' and assigns to 'inv'. It gets
## the cached data or computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}


##the end.