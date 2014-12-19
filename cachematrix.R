## File contains two functions:
## 1) makeCacheMatrix to create a matrix and cache its inverse
## 2) cacheSolve to return the inverse of the matrix to be cached.

## Create a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## starts out as null
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x ## assign get to matrix() function
        setinverse <- function(solve) m <<- solve ## set inverse by calling solve on m and
        ## then assign solve to m in the global environment
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getinverse() ## take the original matrix and call getinverse on it
        if(!is.null(m)) { ## if m isn't null, meaning the inverse has
            ## already been retrieved and cached...
            message("getting cached data") ## show message that cached data is being retrieved
            return(m) ## and return m that was originally created by makeCacheMatrix
        }
        data <- x$get() ## else take x and assign it to data
        m <- solve(data, ...) ## call solve on it
        x$setinverse(m) ## set the inverse as m
        m ## return m
        
}
