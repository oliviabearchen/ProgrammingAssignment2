##This function uses cache as a way to store the previous calculated
#inverse matrix and therefore returns the inverse of a matrix more quickly.

## This function create a matrix and have setters and getters to help cache the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)
}


## This function retrieve the result of the computation.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x $setinverse(i)
    i
}