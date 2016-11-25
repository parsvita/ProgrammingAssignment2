## Put comments here that give an overall description of what your
## functions do
## these two function works in pair. First of them creates an environment for storing Cashed matrix (inverted); 
##      and a list of the functions to help to work with this environmnet
##  second function checks if the Cashed value is present. if so it returns cashed value from the parent environment
##      if not function is calculates new inverted matrix, stores it in the Cashe and return it as output.


## This function makes a list of the functions for object of the class 'makeCacheMatrix'.
##      these functions are used in the function cacheSolve to extract inverted Matrix from the cashe (variable of the parent environment) 
##      or evaluate new inverted matrix if the inverted matrix in the cashe is empty
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverted <- function(Inverted) m <<- Inverted
        getInverted <- function() m
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted)
}


##This function is read Inverted matrix from the cashe (parent environment; 
##      parent environment for the function getInverted() is the function makeCacheMatrix)
## if the variable in the cashe is empty function evaluates new inverse matrix,
##      stores in in the Cashe variable and output the result        
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverted()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##If Case value is empty, wee need to calculate new inverted matrix        
        data <- x$get()
        m <- solve(data)
        ##save inverted matrix in the cashe variable
        x$setInverted(m)
        #output the result
        m
}
