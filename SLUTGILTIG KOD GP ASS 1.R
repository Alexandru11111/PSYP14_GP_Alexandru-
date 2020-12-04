#https://www.geeksforgeeks.org/solve-linear-algebraic-equation-in-r-programming-solve-function/#:~:text=solve()%20function%20in%20R,is%20going%20to%20be%20calculated.
#https://www.datamentor.io/r-programming/while-loop/  
#https://www.mathsisfun.com/algebra/matrix-inverse.html

womenshealth <- womenshealth[, -5]  

cov_mat <- function(womenshealth) {
  
  #creating matrix 
  n <- ncol(womenshealth)
  mat_new <- cbind(rnorm(ncol(womenshealth), mean = 0, sd=0))
  while (n>1) {
    mat_new <- cbind(mat_new, rnorm(ncol(womenshealth), mean = 0, sd=0))
    n<-n-1  #### write 0000000 runs 7 times 
  }
  row.names(mat_new) <- names(womenshealth)
  colnames(mat_new) <- names(womenshealth) #assigning names 
  
  rows<-1 
  collums<-1 
  for (rows in 1:ncol(womenshealth)) { #Ups the rows for the amount of ncol i have. #filling out the 0 in the matrix this is done with the formula for covariance sample. 
    for (collums in 1:rows) {
      mat_new[rows,collums] <- sum((cbind(womenshealth[,rows]-mean(womenshealth[,rows]))*(cbind(womenshealth[,collums]-mean(womenshealth[,collums])))))/(nrow(womenshealth)-1)
    } 
  }
  for (collums in 1:ncol(womenshealth)) {
    for (rows in 1:collums) {
      mat_new[rows,collums] <- sum((cbind(womenshealth[,rows]-mean(womenshealth[,rows]))*(cbind(womenshealth[,collums]-mean(womenshealth[,collums])))))/(nrow(womenshealth)-1)
    } 
  }
  return(mat_new)
}
cov_mat(womenshealth) #Custom function

cov(womenshealth) # R function 

covariance_mat <- cov_mat(womenshealth)

SAVED_COV <-  cov_mat(womenshealth)
"________________________________________________________________________________"
#put a covariance matrix in not the dataset
cov2cor_new <- function(V) {
  
  n <- ncol(V)
  mat_new <- cbind(rnorm(ncol(V), mean = 0, sd=0))
  while (n>1) {
    mat_new <- cbind(mat_new, rnorm(ncol(V), mean = 0, sd=0))
    n<-n-1 
  }
  row.names(mat_new) <- names(V)
  colnames(mat_new) <- names(V)
  
  D <- c(sqrt(diag(V)))
  
  collums <- 1
  while (collums<=nrow(V)) {
    mat_new[collums,collums] <- D[collums]
    collums <- collums+1
  }
  
  aDinv <- solve(mat_new) #inversion of matrix, from covaraince to correlation #diagonal invert c(sqrt(diag(womenshealth)))
  
  cor_mat = aDinv %*% V %*% aDinv #calculation of correlation matrix
  
  return(cor_mat)
}
cov2cor_new(covariance_mat) #Custom function

cor(womenshealth) #R Function
