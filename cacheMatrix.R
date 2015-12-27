
        ## Creating a function where we input a matrix "x"
        ## And the output is a list of new inputs for the cacheSolve() 
	## First we set and get the matrices
	## Next we set and get the inverses
	## The outputs are used in the next function, cacheSolve()  

makeCacheMatrix <- function(x = matrix()) {
   

        ## We use the operator "<<-" to assign a value to an object in an environment different from the current environment


        inv = NULL
        set = function(y) {
        x <<- y
        inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}




cacheSolve <- function(x, ...) {

        ## Input the output of makeCacheMatrix() to calculate te inverse created with previous function
        ## Unless the inverse is already calculated, in which case we retrieve it from the cache to save time
        ## And brings the value of the inverse to the cache using "setinv"
        ## In the other scenario, the inverse is calculated
        
        inv = x$getinv()
        

        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        

        x$setinv(inv)
        
        return(inv)
}
