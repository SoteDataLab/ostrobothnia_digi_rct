### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###                sharpened_qvals.R                  ###
###    function to calculate sharpened q-values       ###
###  original author: M. Anderson, modified by H, MS  ###
###                                                   ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# The R code to calculate sharpened q-values is made by Michael L Anderson 
# and can be found here: anderson.are.berkeley.edu/Research.html
# We have the modified the code so that it is a function that takes p-values
# as an input and outputs the sharpened:q-values

sharpened_qvals <- function(pval) {
  
  # This line assumes you have your p-values in a vector called "pval)
  totalpvals <- length(pval)
  
  original_sorting_order <- 1:length(pval)
  sorted_indices <- order(pval)
  rank <- rank(pval)
  
  qval <- 1
  
  bky06_qval <- rep(1, length(pval))
  
  while (qval > 0) {
    # First Stage
    qval_adj <- qval / (1 + qval)
    fdr_temp1 <- qval_adj * rank / totalpvals
    reject_temp1 <- ifelse(fdr_temp1 >= pval, 1, 0)
    reject_rank1 <- reject_temp1 * rank
    total_rejected1 <- max(reject_rank1, na.rm = TRUE)
    
    # Second Stage
    qval_2st <- qval_adj * (totalpvals / (totalpvals - total_rejected1))
    fdr_temp2 <- qval_2st * rank / totalpvals
    reject_temp2 <- ifelse(fdr_temp2 >= pval, 1, 0)
    reject_rank2 <- reject_temp2 * rank
    total_rejected2 <- max(reject_rank2, na.rm = TRUE)
    
    bky06_qval[rank <= total_rejected2] <- qval
    
    qval <- qval - 0.001
  }
  
  # Restoring the original sorting order
  sorting_indices_inverse <- order(original_sorting_order)
  bky06_qval <- bky06_qval[sorting_indices_inverse]
  
  # Note: Sharpened FDR q-vals can be LESS than unadjusted p-vals when many hypotheses are rejected, 
  # because if you have many true rejections, then you can tolerate several false rejections too
  # (this effectively just happens for p-vals that are so large that you are not going to reject them regardless).
  
  return(bky06_qval)
  
}


