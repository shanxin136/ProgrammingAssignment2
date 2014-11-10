# this code is to cache the inverse of a matrix if its inverse has been computed so that we only need to 
# compute the inverse of a matrx once.  

# this function create a list for a matrix to save necessary information about itself and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y)  {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inver) inverse <<- inver
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# this function is to check if the inverse of a matrix has been saved. If so, cache the value; if not, 
# compute the inverse and save it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    mat <- x$get()
    inverse <- solve(mat)
    x$setinverse(inverse)
    inverse
}
