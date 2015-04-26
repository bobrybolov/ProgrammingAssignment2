makeCacheMatrix <- function(x = matrix()) {
##sets 's' variable to null value in local environment
    s <- NULL
    ##'set' function substitutes x value for the new y value and sets s back to
    ##null in the parent environment (which is 'makeCacheMatrix')
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    ##simply returns the value of x
    get <- function() x
    ##'setsolve' stores the value of input in the main function 'makeCacheMatrix'
    ##to 's' variable. 'getsolve' returns the value of s
    setsolve <- function(solve) s <<- solve 
    getsolve <- function() s
    ##this list makes main function 'makeCacheMatrix' capable of execution mul-
    ##tiple function, namely 'set', 'get', 'setsolve', and 'getsolve'
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 ##checks the value of 's' (stored previously with 'getsolve') and it is not NULL;
    ##if this rule is satisfied, simply returns the value of 's'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    ##else, stores the value taken by 'get' function and process it as to achieve
    ##inverse matrix object, that is assigned after by 'setsolve' in 'makeCacheMatrix'
    ##function
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
