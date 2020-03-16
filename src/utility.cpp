#include <Rcpp.h>
#include <RcppArmadillo.h>
// #include "header.h"
// #include <utility>

// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

void setState(NumericMatrix distMatrix, IntegerVector stateVector, int start, int state, double tolerance){
    for(unsigned int iterator=start+1; iterator<distMatrix.ncol();iterator++){
        if(distMatrix(start,iterator) < tolerance){
            stateVector[iterator]=state;
        }
    }
}
// [[Rcpp::export]]
IntegerVector getState(NumericMatrix distMatrix, double tolerance) {
    unsigned int nGene = distMatrix.nrow();
    IntegerVector stateVector(nGene);
    int stateCounter = 0;
    for(unsigned int iterator=0;iterator<nGene;iterator++){
        if(stateVector[iterator]==0){
            stateCounter++;
            stateVector[iterator]=stateCounter;
            setState( distMatrix, stateVector, iterator, stateCounter, tolerance);
        }
    }
  return stateVector;
}

double euclideanDist(NumericMatrix::Column x, NumericMatrix::Column y){
    double euDist = 0;
    for(auto iterator=0;iterator<y.size();iterator++){
        euDist += pow((x[iterator]-y[iterator]),2);
    }
        return(sqrt(euDist));
}
// [[Rcpp::export]]
Rcpp::List getState2(const arma::mat& expMatrix, double tolerance, int nIC) {
    int n = X.n_rows, k = X.n_cols;
//    IntegerVector getState2(NumericMatrix expMatrix, double tolerance, int nIC, int numModel) {
    Rcout<<euclideanDist(expMatrix(_,i), expMatrix(_,j), 2)<<"\n";
    // unsigned int nGene = expMatrix.nrow();
    // IntegerVector stateVector(nGene);
    // int stateCounter = 0;
    // for(unsigned int iterator=0;iterator<nGene;iterator++){
    //     if(stateVector[iterator]==0){
    //         stateCounter++;
    //         stateVector[iterator]=stateCounter;
    //         setState( expMatrix, stateVector, iterator, stateCounter, tolerance);
    //     }
    // }
    // return stateVector;
    arma::colvec coef = arma::solve(X, y);
    arma::colvec std_err = arma::sqrt(s2 * arma::diagvec(arma::pinv(arma::trans(X)*X)));
    return List::create(Named("coefficients") = coef,
                        Named("stderr")       = std_err,
                        Named("df.residual")  = n - k);
}
