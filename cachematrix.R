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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
