## R Programming course - Assignment 2 
## by Leyden Martinez-Fonte
## Caching the inverse of a matrix

## function that calculates the inverse of a matrix and caches it in memory. 
## If inverse was previously calculated, it returns cached values.

makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m<<- solve 
    getinverse <- function() m               
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse) ## returns list of references to functions
}


## function that invokes makeCacheMatrix

cacheSolve <- function(x, ...) { ## Returns a matrix that is the inverse of 'x'
    m <- x$getinverse() 
    if (!is.null(m)) {  ## if inverse is cached (previuosly calculated), retrieve it
        message("getting cached data")
      return(m)
    }
    data <- x$get()    ## if inverse has not been calculated), invoke calculation
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## End of file