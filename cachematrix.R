##makeVector creates a special "vector", which is really a list containing a function to

##set the value of the Matrix
##get the value of the Matrix
##setInverse the value of the Matrix Invers
##getInverse the value of the Matrix Invers

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 

}


#The following function calculates the Invers of the special "Matrix" 
#created with the above function. 
#However, it first checks to see if the Inverse has already been calculated.
#If not then it created the Inverse using solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  p1<-class(mat)
  inv <- solve(mat)
  p2<-class(inv)
  print(p2)
  x$setInverse(inv)
  inv
}
