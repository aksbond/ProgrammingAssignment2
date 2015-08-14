## The two functions collectively create a matrix and calculate the inverse of the matrix
## The second function "cacheSolve" returns the inverse by first checking whether a cached value exists for the matrix
## If it exists it does not re-calculate the inverse. If not it calculates and returns the value.

## This function creates a list containing functions to 
## 1. Set the value of a matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) xinv <<- inverse
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function to return the inverse of the matrix. It will return the cached value of the matrix 
## if it was caclulated previously. Otherwise it will be recalculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("Getting the cached inverse")
    return(xinv)
  }
  
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}



m1 <- makeCacheMatrix(matrix(c(1,2,1,2,3,4,4,3,2),3,3))
cacheSolve(m1)

