#
## Assignment 2, prepared by Chuck Hill
# 

## Initial vectors to use for tesing. 
# 1. Run cacheMean(x). Return value = 50.5
# 2. From console, x <- makeVector(V2)
# 3. Run cacheMean(x) again. Return value = 250
# 4. Run cacheMean(x) again. You get message "getting cached data", value = 250
#V1 <- 1:100
#V2 <- 200:300
#x <- makeVector(V1)

##  Values below are for matrix inverse operations
# Run cacheSolve(x) from console. Return value =
#       [,1] [,2]
# [1,]  -2  1.5
# [2,]   1 -0.5
#
M <- matrix(1:4, 2, 2)
x <-makeCacheMatrix(M)

makeCacheMatrix <- function(x = matrix()) {
        
        makeVector <- function(x = numeric()) {
                m <- NULL
                set <- function(y) {
                        x <<- y         # y is the value passed when calling x$set
                        m <<- NULL
                }
                get <- function() x
                setmean <- function(mean) m <<- mean
                getmean <- function() m
                list(set = set, get = get,  # list of functions that are passed to cachemean
                     setmean = setmean,
                     getmean = getmean)
                
        } # End of makeVector 
} # End of MakeCacheMatrix

cacheSolve <- function(x, ...) {
        x <- solve(x) # calculates the inverse of a matrix
        
        cacheMean <- function(x, ...) {
                m <- x$getmean()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                        print(m)
                }
                data <- x$get()
                m <- mean(data, ...)
                x$setmean(m)
                m
        }# End of cacheMean 
} # End of cacheSolve
        
