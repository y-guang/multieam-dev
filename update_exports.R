# Update RcppExports to include the new optimized function
cat("Updating Rcpp exports...\n")
Rcpp::compileAttributes(".")
cat("RcppExports updated successfully!\n")