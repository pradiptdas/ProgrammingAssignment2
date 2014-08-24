
## Creating Function calculate the inverse matrix

inv.mat = function(test.mat){
        
        rows = nrow(test.mat)
        cols = ncol(test.mat)
        
        if(rows !=cols){
                return("Inverse can be calculated only for square matrix")
        }
        
        
        if(det(test.mat) == 0){
                return("Inverse cannot be calculated - determinant = 0")
        }
        
        
        
        row2 = rows - 1
        col2 = cols - 1
        
        ## Calculating Adjoint Matrix
        
        sub.mat = matrix(numeric(),row2,col2)
        cofactor.mat = matrix(numeric(),rows,cols)
        adjoint.mat = matrix(numeric(),rows,cols)
        
        for(i in 1:rows){
                for(j in 1:cols){
                        for(i2 in 1:row2){
                                for(j2 in 1:col2){
                                        nr = if(i+i2 > rows){i+i2-rows}
                                        else {i+i2}
                                        
                                        nc = if(j+j2 > cols){j+j2-cols}
                                        else {j+j2}
                                        
                                        sub.mat[i2,j2] = test.mat[nr,nc]                         
                                }
                        }
                        
                        adjoint.mat[j,i] = det(sub.mat) 
                }
        }

        ## Calculating Inverse matrix
        
        inv = (1/(det(test.mat)))*adjoint.mat
        
        inv

}

## Creating Function to store matrix functions

makeCacheMatrix <- function(x=matrix(numeric)){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinv <- function(inv.mat) inv <<- inv.mat
        getinv <- function() inv
        
        list(set=set,
             get=get,
             getinv=getinv,
             setinv=setinv)
}

## Code to Cache inverse of the matrix

cacheSolve <- function(x){
        inv <- x$getinv() 
        if(!is.null(inv)){
                message("Getting Cached Data")
                return(inv)
        }
        data <- x$get()
        inv <- inv.mat(data)
        x$setinv(inv)
        inv
}


