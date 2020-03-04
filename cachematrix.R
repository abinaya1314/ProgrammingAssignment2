## This makeCacheMatrix function is used to create a special matrix 
## which is going to store its own inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This cacheSolve function is going to take the input from the makeCacheMatrix function
## and calculates the inverse of the input matrix. Note that if the determinant of the
## given matrix is 0, then an error message is shown indicating that taking inverse for
## such matrix is not possible (since 1/0 returns NAN). So, this function checks 
## whether the determinant of the input matrix is non-zero and then proceeds with the
## calculation

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  invfunc<-function(aa)
  {
    if(det(aa)!=0)
      solve(aa)
    else
      stop("determinant of the input matrix is 0.. cannot find the inverse")
  }
  inv <- invfunc(data)
  x$setinverse(inv)
  inv
}


