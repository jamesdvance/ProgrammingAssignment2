
#Together these functions allow R to cache a matrix object's inverse, and allow retrieval and changes through function calls

## makeCacheMatrix creates a list that stores 2 matrix objects, one is the initial matrix, the second should be either null, or the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
# Setting local variable i to NULL. i will be used to store the inverse
  i <- NULL
# Allowing user to read in a new matrix to the makeCacheMatrix object
# Sets the function environment's x to the new matrix y
# Sets i to NULL so that the inverse of the previous matrix is no longer present
  set <- function(y){
    x <<- y
    i <<- NULL
  }
# Returns object x, the matrix
  get <- function() x
# Sets i to a given parameter. The implication is it would be fed 'solve(x)'
  set_inverse <- function(inverse) i <<- inverse
# Retrieves object i, the inverse of the matrix
  get_inverse <- function() i
# Returns a list containing the 4 functions above, each named with their function name
  list(set=set, get=get, set_inverse = set_inverse, get_inverse = get_inverse)
  
}


## Computes the inverse of the special 'matrix' returned by makeCacheMatrix. If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
# Sets local variable i to get_inverse() attribute of list x
# It does this just to check if the get_inverse attribute is not null
  i <- x$get_inverse()
# If it is not null, it displays a message signifying it was cached and returns i, the get_inverse attribute of the cached data
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
# Since the if statement contains 'return', the function would have ended if the variable i is not null. This next section provides for the case where variable i is null

# Sets the local variable 'data' to x$get, which is the attribute for the list x that contains the matrix
  data <- x$get()
# Sets local variable i to the inverse of the local variable 'data'
  i <- solve(data,...)
# Uses the set_inverse attribute of the list x to to set x's cached inverse to local variable i
  i
# Returns the inverse, store in local variable i
  
}


# Testing
# Creating an invertable matrix
new_matrix <- matrix(data = c(9,1,3,6,13,11,7,0,5,7,4,7,2,6,1,10), nrow=4, ncol =4)
# Create new makeCacheMatrix object
x <- makeCacheMatrix()
# Set new matrix as x's matrix object
x$set(new_matrix)
# Run cacheSolve, should return the inverse without the 'getting cached data' message
cacheSolve(x)
# Set the inverse to the inverse of new_matrix
x$set_inverse(solve(new_matrix))
# Run cacheSolve, should return the inverse with the message 'getting cached data'
cacheSolve(x)

