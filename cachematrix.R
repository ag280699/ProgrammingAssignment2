## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL   #Initializing inverse as null
       set <- function(y) {
               x <<- y
               inv <<- NULL
       }
       get <- function() x    ##This function is used to get the value of the matrix
       setInverse <- function(inverse) inv <<- inverse  ##setting value of inverse
       getInverse <- function() inv  ##getting value of inverse
       list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}




## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
##checking if inverse of matrix is in the cache
      if (!is.null(inv)) {
              message("getting cached data")
              return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...) ##computing inverse of matrix
      x$setInverse(inv)   ##setting value of inverse in the cache
      inv
}
