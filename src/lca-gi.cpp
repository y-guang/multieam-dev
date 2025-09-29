#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

// to pop a determined item
inline void swap_erase_at(size_t index, 
                         std::vector<int>& item_idx,
                         std::vector<double>& evidence, 
                         std::vector<double>& passed_t,
                         std::vector<double>& V_dt,
                         std::vector<double>& evidence_retention_dt,
                         std::vector<double>& inhibition_beta_dt) {
  size_t last_idx = evidence.size() - 1;
  if (index != last_idx) {
    std::swap(item_idx[index], item_idx[last_idx]);
    std::swap(evidence[index], evidence[last_idx]);
    std::swap(passed_t[index], passed_t[last_idx]);
    std::swap(V_dt[index], V_dt[last_idx]);
    std::swap(evidence_retention_dt[index], evidence_retention_dt[last_idx]);
    std::swap(inhibition_beta_dt[index], inhibition_beta_dt[last_idx]);
  }
  item_idx.pop_back();
  evidence.pop_back();
  passed_t.pop_back();
  V_dt.pop_back();
  evidence_retention_dt.pop_back();
  inhibition_beta_dt.pop_back();
}

//' Simulate evidence accumulation in a drift-diffusion model
//'
// [[Rcpp::export]]
List accumulate_evidence_lca_gi(
  NumericVector A,
  NumericVector V,
  NumericVector ndt,
  NumericVector beta,
  NumericVector k,
  double max_t,
  double dt,
  int max_reached,
  Function noise_func = R_NilValue
) {
  // heuristic batch config
  constexpr double MIN_BATCH_X = 256;
  constexpr double MAX_BATCH_X = 2048;

  // size of V is the number of items
  int n_items = V.size();
  
  // Input validation
  if (A.size() > n_items || A.size() < max_reached) {
    stop("Length of A must be <= number of items and >= max_reached. Got: A.size()=" + std::to_string(A.size()) + ", n_items=" + std::to_string(n_items) + ", max_reached=" + std::to_string(max_reached));
  }
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
  if (beta.size() != n_items) {
    stop("Length of beta must be equal to number of items");
  }
  if (k.size() != n_items) {
    stop("Length of k must be equal to number of items");
  }

  // Copy V to STL vector and ensure values are positive (set negative values to small positive)
  std::vector<double> V_dt(n_items);
  for (size_t i = 0; i < static_cast<size_t>(n_items); i++) {
    V_dt[i] = V[i] * dt;
  }
  // Convert k to dt-scaled retention
  std::vector<double> evidence_retention_dt(n_items);
  for (size_t i = 0; i < static_cast<size_t>(n_items); i++) {
    evidence_retention_dt[i] = 1.0 + (beta[i] - k[i]) * dt;
    if (evidence_retention_dt[i] < 0.0) {
      evidence_retention_dt[i] = 0.0;
    }
  }
  // Convert beta to dt-scaled inhibition
  std::vector<double> inhibition_beta_dt(n_items);
  for (size_t i = 0; i < static_cast<size_t>(n_items); i++) {
    inhibition_beta_dt[i] = beta[i] * dt;
    if (inhibition_beta_dt[i] < 0.0) {
      inhibition_beta_dt[i] = 0.0;
    }
  }

  // Initialize status
  std::vector<double> evidence(n_items, 0.0);
  std::vector<int> item_idx(n_items);
  std::iota(item_idx.begin(), item_idx.end(), 0);
  std::vector<double> passed_t(ndt.begin(), ndt.end());
  double t = 0.0; // Time variable starting from 0
  int n_recalled = 0;
  int n_undetermined = n_items;

  // Pre-allocate result vectors using STL
  std::vector<int> reached_item_idx;
  std::vector<double> rts;
  reached_item_idx.reserve(max_reached);
  rts.reserve(max_reached);

  // Noise batching
  NumericVector noise_batch;
  double heuristic_steps = max_t / dt / 10;
  size_t noise_batch_X = static_cast<size_t>(std::max(MIN_BATCH_X, std::min(MAX_BATCH_X, heuristic_steps)));
  size_t noise_batch_size = noise_batch_X * n_items;
  size_t noise_batch_index = noise_batch_size + 1; // to trigger initial noise generation

  do
  {
    // check timeout
    for (size_t i = 0; i < evidence.size();) {
      if (passed_t[i] < max_t) {
        i++;
      }
      else{
        // timeout, remove the item
        swap_erase_at(i, item_idx, evidence, passed_t, V_dt, evidence_retention_dt, inhibition_beta_dt);
        n_undetermined--;
        // Don't increment i since we've moved a new element to position i
      }
    }

    // check enough buffer
    if (noise_batch_index + evidence.size() >= noise_batch_size) {
      try {
        SEXP noise_result = noise_func(noise_batch_size, dt);
        noise_batch = as<NumericVector>(noise_result);
        noise_batch_index = 0;
      } catch (const std::exception& e) {
        stop("Error calling custom noise function: " + std::string(e.what()));
      }
      // size validation
      if (static_cast<size_t>(noise_batch.size()) != noise_batch_size) {
        stop("Custom noise function signature: function(n, dt) where n is the number of noise values needed and dt is the time step.");
      }
    }

    // check evidence reached threshold
    for (size_t i = 0; i < evidence.size();) {
      if (evidence[i] < A[n_recalled]) {
        i++;
      }
      else {
        int selected_idx = item_idx[i];
        reached_item_idx.push_back(selected_idx + 1);
        rts.push_back(passed_t[i]);
        n_recalled++;
        n_undetermined--;
        swap_erase_at(i, item_idx, evidence, passed_t, V_dt, evidence_retention_dt, inhibition_beta_dt);
        // only allow one item to be recalled
        break;
      }
    }

    // calculate total inhibition
    double inhibition_sum = 0.0;
    for (size_t i = 0; i < evidence.size(); i++) {
      inhibition_sum += evidence[i];
    }

    // update evidence for remaining items
    t += dt; // Update time variable
    for (size_t i = 0; i < evidence.size(); i++) {
      passed_t[i] += dt;
      double noise = noise_batch[noise_batch_index + i];
      evidence[i] = std::max(0.0,
        evidence_retention_dt[i] * evidence[i] + V_dt[i] - inhibition_sum * inhibition_beta_dt[i] + noise
      );
    }
    noise_batch_index += evidence.size();
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