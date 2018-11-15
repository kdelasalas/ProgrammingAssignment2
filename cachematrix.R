## As per the assignment, the task is to generate 2 functions that in combination
## will compute the inverse of a matrix from an input then cache the computed values
## to memory. Succeeding computations will first refer to the cache if value is
## already in memory, if so the stored value will be fed as output else a new
## computation will be done and new value will be cached.

## First part is the makeCacheMatrix function which first create a matrix from the
## input and initializes the cache to NULL. It then setup the get, set, getinverse,
## setinverse functions which basically get and set the input values and computed
## inverse values. These will be called later on the cacheSolve part.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function is where the inverse of the matrix is computed and fed
## back to the output as well as the cache for storage. This checks the 
## cache for existing solution before proceeding with the computation. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

