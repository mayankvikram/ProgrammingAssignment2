## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        invertx<- NULL
        set <- function(y) {
                x <<- y
                invertx <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invertx<<- inverse
        getinverse <- function() invertx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse )

}


## function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
           inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
