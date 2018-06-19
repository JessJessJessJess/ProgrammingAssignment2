## makeCacheMatrix() is that it builds a set of functions
## and returns the functions within a list to the parent environment.
## makeCacheMatrix() creates an R object that stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,						# gives the name 'set' to the set() function defined above
        get = get,							# gives the name 'get' to the get() function defined above
        setInverse = setInverse,			# gives the name 'setInverse' to the setInverse() function defined above
        getInverse = getInverse)	        # gives the name 'getInverse' to the getInverse() function defined above        						
         			 						

             
}


## A cache is a way to store objects in memory to accelerate subsequent access to the same object.
## cacheSolve() requires an argument that is returned by makeCacheMatrix() 
## in order to retrieve the inverse of matrix from the cached value that is stored in the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getInverse()
        if (!is.null(inv)) {
                message("here is your cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
