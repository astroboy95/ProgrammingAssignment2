# makeCacheMatrix contains a list of functions of functions which help to store
# the matrix given by the user and return the inverse of that matrix

# set_matrix - sets the matrix to the users matrix
# get_matrix - gets the matrix from the user
# setinversematrix - sets the inverse of the matrix (getinversematrix)
# getinversematrix - gets the inverse of the matrix (cached value)

 makeCacheMatrix <- function(x = matrix()) {
  cached <- NULL
  set_matrix <- function(y){
    x <<- y
    cached <<- NULL
  }
  get_matrix <- function(){
    x
  }
  setinversematrix <- function(solve){ m <<- solve }
  getinversematrix <- function() {
    cached
  }
  list(set_matrix = set_matrix, get_matrix = get_matrix, setinversematrix = setinversematrix, getinversematrix = getinversematrix)
  
}

# cacheSolve calculates the inverse of the matrix which is given by the user
# from the function makeCachematrix.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  output <- x$getinversematrix()
  if(!is.null(output)){
    message("getting inverse matrix")
    return(output)
  }
  data <- x$get_matrix()
  output <- solve(data)
  x$setinversematrix(output)
  output
}