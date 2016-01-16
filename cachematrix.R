## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
       
	  setinverse <- function(inverse) {
			 m <<- solve }
	  #Note you may write own inverse algorthim without R embeded solve function

        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	  m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}

 # test function to measure time to compute inverse matrix
timeMeasure <- function(mdata){
	
	temp = makeCacheMatrix(mdata)

	start.time = Sys.time()
      cacheSolve(temp)
	duration = Sys.time() - start.time
	print(duration)

	# 2nd time run only look inverse in cache
	start.time = Sys.time()
      cacheSolve(temp)
	duration = Sys.time() - start.time
	print(duration)

}


#test command in R console:
#  source("cachematrix.R")
#  set.seed(100)
#  mdata = matrix(rnorm(1000000), nrow=1000, ncol = 1000)
#  timeMeasure(mdata)


#console printout
#	Time difference of 1.0517 secs
#	getting cached data
#	Time difference of 0.0009999275 secs

