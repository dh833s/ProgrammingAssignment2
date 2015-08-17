##MakeCacheMatrix - create a matrix to return a list of functions to set a matrix, get it, set the inverse of that matrix and get that inverse


makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL  ## set inv to null so we can store the cached inverse matrix
        set <- function(y) {  ##set the matrix
                x <<- y  
                inv <<- NULL
        }
        get <- function() x  ## will get the matrix
        setinv <- function(inverse) inv <<- inverse ## will set inverse of the matrix
        getinv <- function() inv  ##will get the inverse of the matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)  ##return the matrix as a list of the new functions
}

cacheSolve <- function(x, ...) {  ##grab the output of the makeCacheMatrix function and return inverse if not already cached, otherwise return cached inverse

        inv <- x$getinv()  
        
        if (!is.null(inv)){ ##check to see if the inverse is already calc'd
                message("getting cached data")  ##if so, return message
                return(inv)  ##then return the cached inverse values in the matrix
        }
        
        mat.data = x$get()  ##if inverse not already there, calculate it
        inv <- solve(mat.data, ...)
        
        x$setinv(inv)  ##use setinv function to set the values of inv
        
        return(inv)  ##return the inverse values just set
}