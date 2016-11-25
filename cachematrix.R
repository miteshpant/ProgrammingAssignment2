
# makeCacheMatrix creates a special "vector", which is really a list containing a function to

#     set the value of the Matrix
#     get the value of the Matrix
#     set the value of the Inverse of the matrix
#     get the value of the Inverse of the matrix

makeCacheMatrix <- function(myMatrix = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    myMatrix <<- y
    inverse <<- NULL
  }
  
  getMatrix <- function(){ 
    myMatrix
  }
  
  setInverse <- function(invertedMatrix) {
   inverse <<- invertedMatrix
  }
  
  getInverse <- function() {
    inverse
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


# cacheSolve function takes input the vecrot from the previous function and checks if the Inverse is already chached ( returns the same if found), 
# in other case it calculates the inverse using solve function of R
# and saves it in the cache

cacheSolve <- function(cachedMatrix, ...) {

  inverse <- cachedMatrix$getInverse()
  
  
  if(!is.null(inverse)) {
    print("getting cached data")
    return(inverse)
  }
  myMatrix <- cachedMatrix$getMatrix()
  
  inverse <- solve(myMatrix)
  cachedMatrix$setInverse(inverse)
  inverse
}
