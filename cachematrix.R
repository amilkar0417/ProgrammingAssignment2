## The following two functionss allow the user to create store the results
## of finding the inverse of a matrix so that the result can be called later
## without having to do the calculation again in the future

## This function creates a vector of functions that allow to manipulate
## a matrix (set and get its value) and its inverse matrix (set and get the inverse)

makeCacheMatrix <- function(m = numeric()){
        
        ## m stands for matrix
        ## i stands for inverse of the matrix m
        
        ## This sets the matrix null
        i <- NULL 
        
        ## This function sets the "value" of the matrix mand sets 
        ## its inverse null as it's yet to be calculated        
        set <- function(y){
                m <<- y
                i <<- NULL
        }
        
        ##This function returns the matrix itself
        get <- function() m  
        
        ##This function sets the inverse of the matrix
        setInverse <- function(inverse) i <<- inverse
        
        ##This function gets the inverse of the matrix
        getInverse <- function() i
        
        ##This line create a list containting the functions avalable and return them
        list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## This function allows the user to calculate the inverse of a matrix if the inverse
## has not been calculated before. It checks if it has been calculated before and if so
## it returns the stored (chached) value instead of doing the calculations again. 
## This saves time and computational power.

cacheSolve <- function(m){
        
        ##m stands for the "matrix" created by the function makeCacheMatrix
        ##Notice that this "matrix" is just a simple list. There was not need to create
        ##an actual matrix as a list suffices
        
        ##This get the "current" value for the inverse of the matrix
        i <- m$getInverse()
        
        
        ##This checks if the "current" value of the inverse of the matrix is null
        ##If it's not null, then it returned the previously stored (cached) data
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        else {
                message("calculating data from scratch")
        }
        
        ##Notice that every time a new "matrix" is created using the function makeCacheMatrix
        ##i is set to NULL which makes sure that a new value for the inverse of the matrix will
        ##be created
        
        
        ##Else, we need to calculate the inverse
        
        ##Here we get the matrix itself and store it in the variable "data". 
        data <- m$get()
        
        ##Here we calculate the inverse of the matrix
        i <- solve(data)
        
        ##Here, we set the value of the inverse of the matrix
        m$setInverse(i)
        
        ##Last, we return the inverse of the matrix
        i
        
}