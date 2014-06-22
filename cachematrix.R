## makeCacheMatrix defines a special type of matrix that stores
## its inverse in a property, providing also setter and getter
## for the matrix itself and for its inverse

## Description of properties and methods:
## invM is the property that stores the inverse of the matrix
## set is the function to set the original matrix
## get returns the original matrix
## setInv is a function that stores the inverse in the invM property
## getInv returns the stored inverse

## Use:
## To create a square matrix type: Mat <-makeCacheMatrix(m)
## Where Mat is the name of the new matrix and
## m is an existing regular square matrix created with
## m <- matrix(1:4,nrow=2,ncol=2) for example
## If mm is a matrix created with mm <- matrix(1:9,nrow=3,ncol=3)
## and mm[1,2] <- 0 to avoid singularity,
## to change the matrix Mat stores type: Mat$set(mm)
## to set the cached inverse matrix type:
## Mat$setInv(solve(mm))
## to obtain the cached inverse type: Mat$getInv()
makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInv <- function(inv) invM <<- inv
  getInv <- function() invM
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve returns a matrix that is the inverse of x
## if it has not been already calculated and cached,
## it calculates the inverse of the matrix assuming it is
## invertible using the solve() function in R 
## and caches the result for future use

cacheSolve <- function(x, ...) {
  if (nrow(x$get()) == 1 && is.na(x$get())) {
    message("undefined matrix returning null")
    return(NULL)
  }
  if (nrow(x$get()[!complete.cases(x$get()),]) > 0) {
    message("incomplete matrix returning null")
    return(NULL)
  }
  invM <- x$getInv()
  if (!is.null(invM)) {
    message("getting cached inverse")
    return(invM)
  }
  invM <- solve(x$get())
  x$setInv(invM)
  invM
}
