// Based on Hadley Wickam's Rcpp tutorial:
// http://adv-r.had.co.nz/Rcpp.html

#include <Rcpp.h>

// [[Rcpp::export()]]
Rcpp::NumericVector mat(int n, Rcpp::NumericVector x) {
  // Goal:create a vector without storing the matrix 
  // Args:
  //   n: dimension of the vector
  //   x: a numeric vector to pre-compute the matrix needed for similarity measure 
  // Returns:
  //   v: a binary numeric vector
  // initialize index
  int idx = 0;
  // create 2d numeric vector
  Rcpp::NumericVector v(n*n);
  
  for (int i = 0; i<n; i++){
    for(int j=0;j<n; j++){
      // compare if two elements belong to the same cluster 
      if(x[i]==x[j]){
      // if yes, set to 1 
        v[idx]=1;
        } else {
      // otherwise, set to 0
          v[idx]=0;
        }
      // increase idx
      idx+=1;
    }
   
  }
  //retur v as a numeric vector 
  v.attr("dim") = Rcpp::Dimension(n, n);
  return v;
}



// The line [[Rcpp::export]] before a function tells R to treat it like
  // a native function.
  // [[Rcpp::export]]
  double similarityCPP(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y) {
    //Goal: compute similarity measures
    // Args:
    //   x: a numeric matrix, C1
    //   y: a numeric matrix, C2
    // Returns:
    //  similarity measure 
    double dot = 0.0, denom_a = 0.0, denom_b = 0.0 ;	int n = x.size();
    for(int i = 0; i < n; ++i) {
      // vectorized computation of cosine distance between two matrices
      // more computationally efficient 
      dot += x[i] * y[i] ;
      denom_a += x[i] * x[i] ;
      denom_b += y[i] * y[i] ;
    }
    // return similarity measure 
    return dot / (sqrt(denom_a) * sqrt(denom_b)) ;
  }


