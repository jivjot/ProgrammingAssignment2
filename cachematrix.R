
## inp: x is numeric matrix
## if inp x is not given then x takes the default value that is the empty matrix
## for given x it will return a list of functions
## set :set matrix x 
## get :return matrix x
## setInverse : will save the inverse of x to be used later
## getInverse : will return the inverse of x , it can be NULL inverse hasnt been cached yet.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## i will store the inverse of x
    set <- function(y) {
      ## upon calling this function we will set the cached value of of x
      ## and reset the value of i to NULL
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## inp : x is a special list of functions descibed in the above function
## any other list will give undefined result 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    ## data is already present so we return the save value  
    message("getting cached data")
    return(i)
  }
  ## get data and compute the inverse and save for the future use
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
