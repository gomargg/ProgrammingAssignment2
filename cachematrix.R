## Next two functions compute the inverse of the (square, invertible) matrix, 
## if the inverse is already cached, then it's only read from memory.
## Note that both functions are very similar to the example functions.

## First function takes a (sqare, invertible) matrix and similarily as in 
## the example, it returns a special 'matrix' which is then used by the second function.


makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Second function takes a result from the first function and 'checks' if
## the inverse of the given function is cached in which case it 'only' reads
## it from the memory. If the inverse is not cached, it is computed with R
## function 'solve'. In both cases the second function returns the inverse of 
## the given matrix.

cacheSolve <- function(x, ...){
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
