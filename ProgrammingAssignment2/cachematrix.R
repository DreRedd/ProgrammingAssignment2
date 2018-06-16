## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse.
## See the benefits of caching the inverse of a matrix
##caching the inverse of a matrix rather than computing it repeatedly
## This functions below creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invt<- NULL
    set<- function(y){
        x<<- y
        invt<<- NULL
    }
    get<- function() x
    setInverse<- function(inverse) invt<<- inverse.gaussian() 
    getInverse<- function() invt
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed);
## The cacheSolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invt <- x$getInverse()
    if(!is.null(invt)){ 
        message("getting cached data")
        return(invt)
    }
    data<- x$get()
    invt<- solve(data, ...)
    x$setInverse(invt)
    invt
}
