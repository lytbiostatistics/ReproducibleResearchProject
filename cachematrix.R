## cache the inverse of a matrix
##  Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(matrix){
                x<<- matrix
                i <<- NULL
        }
        get <- function(){
                x
        }
        setinverse <- function(inverse){
                i <<-inverse
        }
        getinverse <- function(){
                i
        }
        list(set= set, get=get, setinverse= setinverse, getinverse=getinverse)
                }


## Compute the inverse of the special matrix returned by "makeCacheMatrix"above. If the inverse has already been calculated (and the matrix has notchanged), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)%*%data
        x$setinverse(m)
        m## Return a matrix that is the inverse of 'x'
}
