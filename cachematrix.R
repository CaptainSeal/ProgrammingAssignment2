## makeCacheMatrix function creates a special "matrix" object(actually a list of 4 functions) that can cache its inverse.
## cacheSolve function computes the inverse of the special "matrix" created by makeCacheMatrix.
## If it has already been calculated (and the matrix has not changed via set() element of "matrix"), then the cachesolve retrieves the inverse from the cache.

## makeCacheMatrix returns a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) { ## This one sets a new matrix for this "object"
    x <<- y
    inv <<- matrix() ## And sets inverse matrix to empty
  }
  get <- function() x ## This one returns matrix
  setinverse <- function(inverse) inv <<- inverse ## This one sets calculated by cacheSolve inverse
  getinverse <- function() inv ## This one returns inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks cache for previously calculated inverse of matrix. If cache is not "empty", then
## result from cache returned. Otherwise solve() function is called.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!identical(inv,matrix())) { ## Important notice: is.null(matrix()) returns FALSE,
    message("getting cached inverse") ## hence we are comparing inv to empty matrix
    return(inv)
  }
  inv <- solve(x$get(), ...) ## Solving(make sure matrix is invertable)
  x$setinverse(inv)
  inv
}
## You can check solver by running cacheSolve(x)%*%x$get()