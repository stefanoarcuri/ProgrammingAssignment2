## Put comments here that give an overall description of what your
## functions do
        #####################General Descriptions of two functions do
        #Calculate a inverse, if exists use cache:
        #define a matrix for example A<-matrix(c(1,1,4,0,3,1,4,4,0),nrow=3,ncol=3)
        #>invertita<-makeCacheMatrix(A)
        #cacheSolve(invertita)
        #            [,1]        [,2]    [,3]
        #[1,]  0.08333333 -0.08333333  0.2500
        #[2,] -0.33333333  0.33333333  0.0000
        #[3,]  0.22916667  0.02083333 -0.0625
        #########################################################################
## Write a short comment describing this function
        #     First makeCacheMatrix - I became crazy
        #define a matrix for example A<-matrix(c(1,1,4,0,3,1,4,4,0),nrow=3,ncol=3)
        #Goal:  solve (A) 
        #
        #To use it (for example): 
        #
        #>invertita<-makeCacheMatrix(A)     -> assign instance 
        #      SET FORCE VALUES WITH FORMULA INSIDE
        #> invertita$setsolve(solve(A))     ->make the inverse (it's necessary)
        #>invertita$getsolve()              ->result (A inverse)
        #     SET ACCEPTS NEW VALUES OF THE MATRIX
        #invertita$set(matrix(c(1,1,1,1,2,1,4,4,0),nrow=3,ncol=3)) for example new matrix
        #or 
        #> A<-matrix(c(2,1,4,0,3,1,4,4,0),nrow=3,ncol=3)
        #> invertita$set(A)
        #> A   
        #     GET GIVE NEW OR EXISITING VALUES
        #invertita$get()
        #  [,1] [,2] [,3]
        #[1,]    1    1    4
        #[2,]    1    2    4
        #[3,]    1    1    0
        # 
        # 
        ########################################################################
        

        
        makeCacheMatrix <- function(x = matrix()) {
                m <- NULL                       #initialize cache
                set <- function(y) {
                        x <<- y                 #Force matrix in x
                        m <<- NULL             #reset cache
                }
                get <- function() x            #here 4 functions described above
                setsolve <- function(solve) m <<- solve 
                getsolve <- function() m
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
                
        }
        

## Write a short comment describing this function

        ########################This functions get the cache if exists otherwise runs calculation#####
        # 'getting cached data'
        #otherwise it calculate the Solve.
        #
        #cachesolve(invertita) 
        #you get : 
        #getting cached data <- IF CACHE EXISTS
        #        [,1]        [,2]        [,3]
        #        [1,]  0.07692308 -0.07692308  0.23076923
        #        [2,] -0.30769231  0.30769231  0.07692308
        #        [3,]  0.21153846  0.03846154 -0.11538462
        #

        cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                m <- x$getsolve()             #
                if(!is.null(m)) {                     #CHECK CACHE
                        message("getting cached data")    
                        return(m)                            #
                }                                #
                data <- x$get()                       #mette in vettore in data
                m <- solve(data, ...)                  #calcola inverse
                x$setsolve(m)                          #ASSIGN VALUE
                m                                      #ritorna il risultato
                
        }
        