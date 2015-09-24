# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
# the next function only has one parameter. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  #avoid a corrupt value
    set <- function(y) {
        x <<- y  #transfer value of y to x
        inv <<- NULL  # assign NULL  to inv
    }
    get <- function() x   #call the function 
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
# the next function has a particular parameters

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {   #it validates if there is not null
        message("getting cached data.") # only a message
        return(inv)
    }
    data <- x$get()  #assign the object to variable named data
    inv <- solve(data) #assign the result of the solve the equations
    x$setinverse(inv)  #make the inverse
    inv  #show the value of inv
}
