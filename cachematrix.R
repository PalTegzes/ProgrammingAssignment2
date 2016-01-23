## This code demonstrates the usefulness of lexical scoping
## In R all functions have access to the environment where they were created,
## this is their enclosing environment.
## When a function is called repetitively, it can always access this 
## same enclosing environment. This can be used to cache results 
## at a given execution and reuse it during later executions.
## In this example we use this property to cache the inverse of a matrix 
## rather than computing it repeatedly


## This function creates a special "CacheMatrix" object
## that can cache its inverse.
## In reality this is a list of 4 functions to
##   1.  set the values of the matrix
##   2.  get the values of the matrix
##   3.  set the value of the inverse matrix
##   4.  get the value of the inverse matrix
## This can be used with cacheSolve to calculate inverse and cache the result
##
## Usage: cx <- makeCacheMatrix(x)
## where x is a simple numeric matrix
## return value is a CacheMatrix object (i.e. a list of 4 functions defined above)

makeCacheMatrix <- function(x = matrix()) {
    # We are now in the execution environment of makeCacheMatrix
    # This will be enclosing environment for set, get, setinverse, getinverse
    # In this environment we can store the cached matrix and inverse matrix
    inv.x <- NULL  # Initialize inverse: not yet calculated 
    set <- function(y) {
        x <<- y        # store the received argument to the enclosing environment
        inv.x <<- NULL # reset the inverse because matrix changed
    }
    get <- function() x # Get the matrix from the enclosing environment
    setinverse <- function(inv.matrix) inv.x <<- inv.matrix # Store the received inverse in the enclosing environment
    getinverse <- function() inv.x # Get the inverse from the enclosing environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) # Return a list compiled from the 4 functions
    
}


## This function computes the inverse of the special
## "CacheMatrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.
##
## Usage: x.inverted <- cacheSolve(cx, ...)
## Where cx is a CacheMatrix object created by makeCacheMatrix, 
## ... can be additional arguments to be provided to solve()
## return value is the inverted matrix as a simple numerical matrix

cacheSolve <- function(cx, ...) {
    stored.inverse <- cx$getinverse() # See if the inverse has been cached yet
    if(!is.null(stored.inverse)) {
        message("getting cached data")
        return(stored.inverse)
    }
    data <- cx$get()    # get matrix values
    inv.data <- solve(data, ...) # calculate inverse 
    cx$setinverse(inv.data) # cache inverse
    inv.data # return the inverse
}
