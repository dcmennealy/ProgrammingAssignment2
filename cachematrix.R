## Put comments here that give an overall description of what your
## functions do

## Goal is to cache a matrix.  There are 5 steps
#1. Start by creating a placeholder for the stored inverse 
#2. Create function that sets the matrix values and clear inverse in main function
#3. Create function that gets or calls the matrix
#4. Create functions that sets and call the value (after solving for inverse).
#5. Store the created functions in a list

makeCacheMatrix <- function(mVal = matrix()) {
        storedInv <- NULL
        fSetMVal <- function(mNewVal) {
                mVal <<- mNewVal
                storedInv <<- NULL
        }
        fGetMVal <- function() mVal
        fSetInv <- function(solve) storedInv <<- solve
        fGetInv <- function() storedInv
        list(fSetMVal = fSetMVal, fGetMVal = fGetMVal,
             fSetInv = fSetInv,
             fGetInv = fGetInv)
}




## Write a short comment describing this function
#Goal here is to store and return inverse matrix solution
#1. cacheSolve takes the matrix set in the main function
#2. Sets the stored inverse value as storedInv
#3. If the stored inverse value is not null then return it
#4. If the cached inverse value is null then it computes it

cacheSolve <- function(mVal, ...) {
                #get cached inverse and set to storedInv
                storedInv <- mVal$fGetInv()
                
                #grab value is stored inverse is not null
                if(!is.null(storedInv)) {
                        message("getting cached data")
                        return(storedInv)
                }
                
                #Compute inverse if null
                dataVal <- mVal$fGetMVal()
                storedInv <- solve(dataVal, ...)
                mVal$fSetInv(storedInv)
                
        ## Return a matrix that is the inverse of 'storedInv'
                storedInv
}
