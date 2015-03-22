## The following two functions are used to cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a list containing a function to
## 
##      -- set the value of the matrix
##      -- get the value of the matrix
##      -- set the inverse of the matrix
##      -- get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


##  The following function cacheSolve calculates the inverse of the special "matrix" 
##  created with the above function. It first checks to see if the inverse of the 
##  matrix has already been calculated. If so, it gets the inverse from the cache 
##  and skips the computation. Otherwise, it calculates the inverse of the 
##  matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        initial <- x$get()
        final <- solve(initial, ...)
        x$setinverse(final)
        final
}

##  @Return
## > x <- makeCacheMatrix()
## > x$set(matrix(c(1,4,4,1),2,2))
## > x$get()
##       [,1] [,2]
## [1,]    1    4
## [2,]    4    1
## > cacheSolve(x)
##              [,1]        [,2]
## [1,] -0.06666667  0.26666667
## [2,]  0.26666667 -0.06666667
## > cacheSolve(x)
## getting cached data
##              [,1]        [,2]
## [1,] -0.06666667  0.26666667
## [2,]  0.26666667 -0.06666667
## > x <- makeCacheMatrix()
## > x$set(matrix(c(8,2,2,8),2,2))
## > x$get()
##       [,1] [,2]
## [1,]    8    2
## [2,]    2    8
## > cacheSolve(x)
##              [,1]        [,2]
## [1,]  0.13333333 -0.03333333
## [2,] -0.03333333  0.13333333
## > cacheSolve(x)
## getting cached data
##              [,1]        [,2]
## [1,]  0.13333333 -0.03333333
## [2,] -0.03333333  0.13333333
## > x <- makeCacheMatrix()
## > x$set(matrix(c(0,2,2,0),2,2))
## > x$get()
## [,1] [,2]
## [1,]    0    2
## [2,]    2    0
## > cacheSolve(x)
##      [,1] [,2]
## [1,]  0.0  0.5
## [2,]  0.5  0.0
## > cacheSolve(x)
## getting cached data
##      [,1] [,2]
## [1,]  0.0  0.5
## [2,]  0.5  0.0
