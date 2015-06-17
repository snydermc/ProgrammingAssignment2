## To save on calculation time, these functions store an inverted matrix after
## it has been calculated so that the operation isn't constantly repeated.

## x is a square matrix that can be inverted; this function sets up several 
##other functions to "get" data, "set" data if changed, "get inverse" if it
##already exists or "set inverse" if it's just been created. It's up to the
##subsequent equation to actually call on these functions by subsetting them.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## If matrix has already been inverted, retrieve cached inverse.
## If matrix not already calculated, get the raw data, invert it, 
## then set the inverted data as the new stored inverse
## then return the inverted matrix.

cacheSolve <- function(x, ...) {

        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i

        ## Return a matrix that is the inverse of 'x'
}
