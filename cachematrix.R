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

## Use:
## When a new cached matrix is created with makeCacheMatrix
## it doesn't automatically calculates the inverse, to calculate
## and cache the inverse type:
## Mat <- makeCacheMatrix() ## empty matrix of 1x1
## cacheSolve(Mat) ## returns Null because matrix is undefined
## Mat$set(matrix(nrow=2,ncol=2)) ## set the matrix to a 2x2
## cacheSolve(Mat) ## returns Null because matrix is incomplete
## mm <- matrix(nrow=2,ncol=2) ## new matrix with no values
## mm[1,1] <- 1
## mm[1,2] <- 3
## mm[2,1] <- -2
## Mat$set(mm) ## set the matrix to an incomplete 2x2
## cacheSolve(Mat) ## returns Null because matrix is incomplete
## mm[2,2] <- 2
## Mat$set(mm) ## set the matrix to an complete 2x2
## cacheSolve(Mat) ## returns and caches inverse
cacheSolve <- function(x, ...) {
  ## If inverse in cache then return it
  invM <- x$getInv()
  if (!is.null(invM)) {
    message("getting cached inverse")
    return(invM)
  }
  ## Check if matrix is not undefined
  if (nrow(x$get()) == 1 && is.na(x$get())) {
    message("undefined matrix returning null")
    return(NULL)
  }
  ## Check if matrix contains no NA
  if (is.null(nrow(x$get()[!complete.cases(x$get()),])) ||
      nrow(x$get()[!complete.cases(x$get()),]) > 0) {
    message("incomplete matrix returning null")
    return(NULL)
  }
  ## Check if matrix is square
  if (nrow(x$get()) != ncol(x$get())) {
    message("matrix not square returning null")
    return(NULL)
  }
  invM <- solve(x$get())
  x$setInv(invM)
  invM
}
