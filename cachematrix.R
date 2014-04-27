## makeCacheMatrix creates a matrix object that can cache it's inverse
## cacheSolve computes the inverse of the matrix stored in makeCacheMatrix.
## If the inverse has been calculated, cacheSolve will then take the inverse from the cache

## makeCacheMatrix is an object for caching the inverse of a matrix that has been computed 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               # To ensure matrix starts from NULL
        set <- function(y){                     # Function to set the matrix for solving
                x <<- y                         # Assigns matrix in 'set' function to x
                m <<- NULL                      # Otherwise, if there is no matrix assigned, 'm' remains NULL 
        }
        get <- function() x                     # 'get' function returns cached matrix 'x'
        setInverse <- function(inverse) m <<- inverse   # 'setInverse' function stores computed inverse of matrix
        getInverse <- function() m              # 'getInverse' function returns computed inverse that has been cached
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)  # list of functions created as above

}


## cacheSolve takes the cached inversed matrix from makeCacheMatrix or computes the inverse of the cached matrix if it has not been computed and cached already

cacheSolve <- function(x, ...) {
        m2 <- x$getInverse()                     # fetches the cached inversed matrix from the 'makeCacheMatrix' function and assign to 'm2' object
        if(!is.null(m2)){                        # if 'makeCacheMatrix' already has a cached inversed matrix
                message("getting cached data")   # return the string "getting cached data"
                return(m2)                       # return the cached inverse matrix
        }
        data <- x$get()                          # if 'makeCacheMatrix' has not cached an inversed matrix, fetch the matrix from the 'makeCacheMatrix' function and assign it to object 'data'
        m3 <- solve(data)                        # use the 'solve' function to compute the inverse of the cached matrix and assign it to 'm3'
        x$setInverse(m3)                         # uses 'setInverse' function from 'makeCacheMatrix' function to assign computed inversed matrix to 'm' in 'makeCacheMatrix' environment
        m3                                       ## Return a matrix that is the inverse of 'x'
}
