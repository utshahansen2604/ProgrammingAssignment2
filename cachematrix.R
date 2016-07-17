## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
    
	# holds the cached value or NULL if nothing is cached
    # initially nothing is cached so set it to NULL

    i <- NULL
    # store a matrix
    set <- function(y) {
        x <<- y
        # since the matrix is assigned a new value, flush the cache
        i <<- NULL
        
    }
    # returns the stored matrix
    get <- function() x
    # cache the given argument 
    setinverse <- function(inv) i <<- inv
    # get the cached value
    getinverse <- function() i
    
    # return a list. Each named element of the list is a function
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available
# makeCacheMatrix
cacheSolve <- function(x, ...) {
    # get the cached value

    i <- x$getinverse()
    # if a cached value exists return it
    if(!is.null(i)) {
        message("Cached Data Recieved")
        return(i)
    }
    # otherwise get the matrix, caclulate the inverse and store it in the cache
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    
    # return the inverse
    i
}
