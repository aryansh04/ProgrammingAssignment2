## Following two functions calculate the inverse of the matrix and cache its value
## so that it need not be calculated over and over again.

## 'makeCacheMatrix' returns a list of precisely 4 different functions given below:
# 1. Set a matrix
# 2. Get a matrix
# 3. Set an Inverse of matrix
# 4. Get an Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y = matrix()) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)   
}


## 'cacheSolve' calculates the inverse of a matrix. In doing so it first checks
# if the inverse of the same matrix has previously been calculated  (i.e. by 'makeCacheMatrix' function).
# If so it gets it from cache otherwise calculates the inverse using 'solve(a,...)' and returns its value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(as.matrix(data), ...)
    x$setInverse(inv)
    inv
    
}

