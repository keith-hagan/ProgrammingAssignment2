## The following two functions operate to reduce computing time when a particular matrix inverse
## is needed in multiple computations. 
## The makeCacheMatrix function creates a list which serves as a sort of matrix proto-object
## which the cacheSolve function uses to compute/retrieve matrix inverse attributes.


## This functions creates a list of setter and getter functions which allow the cacheSolve function 
## to retrieve rather than compute the matrix's inverse when necessary.

makeCacheMatrix <- function(X = matrix()) {
        I <- NULL
        set <- function(M) {
                X <<- M
                I <<- NULL
        }
        get <- function() X
        setinv <- function(inv) I <<- inv
        getinv <- function() I
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function either retrieves the cached matrix inverse, or computes and caches the inverse for later.
## In either case the inverse is printed to the console.

cacheSolve <- function(X, ...) {
        I <- X$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- X$get()
        I <- solve(data, ...)
        X$setinv(I)
        I
}