#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
String greet(std::string name) {
  return "Hello, " + name + "!";
}