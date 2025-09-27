// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;

// to pop a determined item
inline void swap_erase_at(size_t index, 
                         std::vector<int>& item_idx,
                         std::vector<double>& passed_t) {
  size_t last_idx = item_idx.size() - 1;
  if (index != last_idx) {
    std::swap(item_idx[index], item_idx[last_idx]);
    std::swap(passed_t[index], passed_t[last_idx]);
  }
  item_idx.pop_back();
  passed_t.pop_back();
}


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
  // heuristic batch config
  constexpr double MIN_BATCH_X = 256;
  constexpr double MAX_BATCH_X = 2048;

  int n_items = V.size();
    
  // Basic input validation
  if (max_reached <= 0 || max_reached > n_items) {
    stop("max_reached must be > 0 and <= n_items");
  }
  if (A.size() < max_reached) {
    stop("Length of A must be >= max_reached. Got: A.size()=" + std::to_string(A.size()) + ", max_reached=" + std::to_string(max_reached));
  }
  if (ndt.size() != n_items) {
    stop("Length of ndt must be equal to number of items");
  }
  if (dt <= 0 || max_t <= 0) {
    stop("dt and max_t must be > 0");
  }
  if (W.nrow() != n_items || W.ncol() != n_items) {
    stop("Dimension of W must be n_items x n_items. Got: W.nrow()=" + std::to_string(W.nrow()) + ", W.ncol()=" + std::to_string(W.ncol()) + ", n_items=" + std::to_string(n_items));
  }
  if (Rf_isNull(noise_func)) {
    stop("noise_func parameter is required and cannot be NULL");
  }
  
  // Convert R objects to Armadillo objects
  arma::mat W_arma = as<arma::mat>(W);
  arma::vec A_arma = as<arma::vec>(A);
  arma::vec V_arma = as<arma::vec>(V);

  // status
  arma::vec evidence_arma = arma::zeros(n_items);
  // length equals to undetermined items
  std::vector<double> passed_t(ndt.begin(), ndt.end());
  std::vector<int> item_idx(n_items);
  std::iota(item_idx.begin(), item_idx.end(), 0);
  double t = 0.0;
  int n_recalled = 0;
  int n_undetermined = n_items;

  // Pre-allocate result vectors using STL
  std::vector<int> reached_item_idx;
  std::vector<double> rts;
  reached_item_idx.reserve(max_reached);
  rts.reserve(max_reached);
  
  // Noise batching
  arma::vec noise_batch_arma;
  double heuristic_steps = max_t / dt / 10;
  size_t noise_batch_X = static_cast<size_t>(std::max(MIN_BATCH_X, std::min(MAX_BATCH_X, heuristic_steps)));
  size_t noise_batch_size = noise_batch_X * n_items;
  size_t noise_batch_index = noise_batch_size + 1; // to trigger initial noise generation

  do
  {
    // check timeout
    for (size_t i = 0; i < item_idx.size();) {
      if (passed_t[i] < max_t) {
        i++;
      }
      else{
        // timeout, remove the item
        swap_erase_at(i, item_idx, passed_t);
        n_undetermined--;
        // Don't increment i since we've moved a new element to position i
      }
    }

    // check enough buffer
    if (noise_batch_index + n_items >= noise_batch_size) {
      try {
        SEXP noise_result = noise_func(noise_batch_size, dt);
        NumericVector noise_batch_r = as<NumericVector>(noise_result);
        noise_batch_arma = as<arma::vec>(noise_batch_r);
        noise_batch_index = 0;
      } catch (const std::exception& e) {
        stop("Error calling custom noise function: " + std::string(e.what()));
      }
      // size validation
      if (noise_batch_arma.n_elem != noise_batch_size) {
        stop("Custom noise function signature: function(n, dt) where n is the number of noise values needed and dt is the time step.");
      }
    }

    // check evidence reached threshold
    for (size_t i = 0; i < item_idx.size();) {
      int selected_idx = item_idx[i];
      if (evidence_arma(selected_idx) < A_arma(n_recalled)) {
        i++;
      } else {
        reached_item_idx.push_back(selected_idx + 1);
        rts.push_back(passed_t[i]);
        n_recalled++;
        n_undetermined--;
        swap_erase_at(i, item_idx, passed_t);
        // only allow one item to be recalled
        break;
      }
    }

    // update evidence for all items
    t += dt;
    for (size_t i = 0; i < passed_t.size(); i++) {
      passed_t[i] += dt;
    }
    arma::vec noise_arma = noise_batch_arma.subvec(noise_batch_index, noise_batch_index + n_items - 1);
    noise_batch_index += n_items;
    arma::vec Wx = W_arma * evidence_arma;
    arma::vec dx = (V_arma - k * evidence_arma - beta * Wx) * dt + noise_arma;
    evidence_arma += dx;
    evidence_arma = arma::clamp(evidence_arma, 0.0, arma::datum::inf);
  } while (n_undetermined > 0 && n_recalled < max_reached);

  // Build output using STL vectors and convert to Rcpp types only at the end
  if (n_recalled > 0) {
    // Directly create Rcpp vectors from STL vectors
    IntegerVector output_item_indexes(reached_item_idx.begin(), reached_item_idx.end());
    NumericVector final_rts(rts.begin(), rts.end());
    
    return List::create(
      Named("item_idx") = output_item_indexes,
      Named("rts") = final_rts
    );
  } else {
    return List::create(
      Named("item_idx") = IntegerVector(0),
      Named("rts") = NumericVector(0)
    );
  }
}