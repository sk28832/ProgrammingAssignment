#The set() function sets the value of the matrix
#The get() function gets the value of the matrix
#The setinverse() function sets the value of the inverse
#The getinverse() function gets the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Checks to see if there has been a cached inverse already. If so, get it.
## If not, solve for the inverse and return it

cacheSolve <- function(x, ...) {
        m <- x$getinverse
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
