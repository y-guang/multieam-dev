// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;

//' Simulate evidence accumulation using the Leaky Competing Accumulator (LCA) model
//'
//' The LCA dynamics are defined as a stochastic ODE in vector form:
//'
//' \deqn{
//'   \frac{d\mathbf{x}}{dt} = \mathbf{v}
//'     - k \mathbf{x}
//'     - \beta W \mathbf{x}
//'     + \sigma \boldsymbol{\eta}(t)
//' }{
//'   dx/dt = v - k x - β W x + σ η(t)
//' }
//'
//' where
//' \itemize{
//'   \item \eqn{\mathbf{x}(t) \in \mathbb{R}^N}: state vector (activations of N accumulators)
//'   \item \eqn{\mathbf{v} \in \mathbb{R}^N}: input/drift vector
//'   \item \eqn{k \in \mathbb{R}}: leak constant
//'   \item \eqn{\beta \in \mathbb{R}}: inhibition strength
//'   \item \eqn{W \in \mathbb{R}^{N \times N}}: inhibition matrix.
//'         Convention: \eqn{w_{ij}} is the weight **from unit j to unit i**
//'         (i.e., column j influences row i).
//'         Typically \eqn{w_{ii} = 0}.
//'   \item \eqn{\sigma \in \mathbb{R}}: noise strength
//'   \item \eqn{\boldsymbol{\eta}(t) \in \mathbb{R}^N}: Gaussian white noise
//' }
//'
//' Special cases:
//' \itemize{
//'   \item **Symmetric inhibition:** \eqn{W = J - I}, where \eqn{J} is the all-ones matrix.
//'   \item **Structured inhibition:** general nonnegative matrix \eqn{W}.
//' }
// [[Rcpp::export]]
List accumulate_evidence_lca(
  NumericMatrix W,
  NumericVector A,
  NumericVector V,
  NumericVector ndt,
  double max_t,
  double dt,
  double beta,
  double k,
  int max_reached,
  Function noise_func = R_NilValue
) {
  // Convert R objects to Armadillo objects
  arma::mat W_arma = as<arma::mat>(W);
  arma::vec A_arma = as<arma::vec>(A);
  arma::vec V_arma = as<arma::vec>(V);
  arma::vec ndt_arma = as<arma::vec>(ndt);
  
  int n_items = V_arma.n_elem;
  
  // Basic input validation
  if (max_reached <= 0 || max_reached > n_items) {
    stop("max_reached must be > 0 and <= n_items");
  }
  if (ndt.size() != n_items) {
    stop("Length of ndt must be equal to number of items");
  }
  if (dt <= 0 || max_t <= 0) {
    stop("dt and max_t must be > 0");
  }
  if (Rf_isNull(noise_func)) {
    stop("noise_func parameter is required and cannot be NULL");
  }
  
  // Initialize state vector x (evidence)
  arma::vec x = arma::zeros(n_items);
  
  // Initialize time tracking
  double current_time = 0.0;
  
  // Track which items have reached threshold
  std::vector<int> reached_items;
  std::vector<double> reaction_times;
  std::vector<bool> item_determined(n_items, false);
  int n_reached = 0;
  
  while (n_reached < max_reached && current_time < max_t) {
    current_time += dt;
    
    // Get noise for all items (even determined ones participate in matrix calculations)
    NumericVector noise_vec;
    try {
      SEXP noise_result = noise_func(n_items, dt);
      noise_vec = as<NumericVector>(noise_result);
    } catch (const std::exception& e) {
      stop("Error calling noise function: " + std::string(e.what()));
    }
    arma::vec noise_arma = as<arma::vec>(noise_vec);
    
    // Apply LCA update equation for all items: x_i(t+dt) = x_i(t) + (v_i - k*x_i(t) - beta*(W*x)_i)*dt + noise
    arma::vec Wx = W_arma * x;
    arma::vec dx = (V_arma - k * x - beta * Wx) * dt + noise_arma;
    x = x + dx;
    
    // Ensure non-negative evidence
    x = arma::clamp(x, 0.0, arma::datum::inf);
    
    // Check for threshold crossings (only for items not yet determined)
    for (int i = 0; i < n_items; i++) {
      if (!item_determined[i] && x(i) >= A_arma(n_reached)) {
        reached_items.push_back(i + 1); // R indexing (1-based)
        reaction_times.push_back(current_time + ndt_arma(i)); // Add non-decision time to response time
        item_determined[i] = true;
        n_reached++;
        break; // Only one item can be recalled per time step
      }
    }
  }
  
  // Convert results back to R
  if (n_reached > 0) {
    IntegerVector item_indices(reached_items.begin(), reached_items.end());
    NumericVector rts(reaction_times.begin(), reaction_times.end());
    
    return List::create(
      Named("item_idx") = item_indices,
      Named("rts") = rts
    );
  } else {
    return List::create(
      Named("item_idx") = IntegerVector(0),
      Named("rts") = NumericVector(0)
    );
  }
}