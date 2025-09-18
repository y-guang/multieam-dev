#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
List accumulate_evidence_ddm(
  NumericVector A,
  NumericVector V,
  NumericVector ndt,
  double dt,
  int max_reached,
  double max_t,
  String noise_mechanism = "add",
  Function noise_func = R_NilValue
) {
  constexpr double MIN_BATCH_X = 256;
  constexpr double MAX_BATCH_X = 2048;
  int n_items = V.size();
  
  // Input validation
  if (A.size() > n_items || A.size() < max_reached) {
    stop("Length of A must be <= number of items and >= max_reached");
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
  if (noise_func == R_NilValue)
  {
    stop("noise_func parameter is required and cannot be NULL");
  }
  
  
  // Check if noise function is provided
  if (Rf_isNull(noise_func)) {
    stop("noise_func parameter is required and cannot be NULL");
  }

  // Define noise mechanism enum
  enum NoiseType { ADDITIVE, MULTIPLICATIVE };
  
  // Validate noise mechanism and set type
  NoiseType noise_type;
  if (noise_mechanism == "add") {
    noise_type = ADDITIVE;
  } else if (noise_mechanism == "mult") {
    noise_type = MULTIPLICATIVE;
  } else {
    stop("noise_mechanism must be 'add' or 'mult'");
  }

  // Copy V to STL vector and ensure values are positive (set negative values to small positive)
  std::vector<double> V_dt(n_items);
  for (size_t i = 0; i < static_cast<size_t>(n_items); i++) {
    V_dt[i] = V[i] * dt;
  }

  // Initialize status
  std::vector<double> evidence(n_items, 0.0);
  std::vector<int> item_idx(n_items);
  std::iota(item_idx.begin(), item_idx.end(), 0);
  std::vector<double> passed_t(n_items);
  for (size_t i = 0; i < static_cast<size_t>(n_items); i++) {
    passed_t[i] = ndt[i];
  }
  int n_recalled = 0;
  int n_determined = 0;
  int n_undetermined = n_items;

  // Pre-allocate result vectors using STL
  std::vector<int> reached_item_idx;
  std::vector<double> rts;
  reached_item_idx.reserve(max_reached);
  rts.reserve(max_reached);

  // Noise batching
  NumericVector noise_batch;
  size_t noise_batch_index = 0;
  double heuristic_steps = max_t / dt / 10;
  size_t noise_batch_X = static_cast<size_t>(std::max(MIN_BATCH_X, std::min(MAX_BATCH_X, heuristic_steps)));
  size_t noise_batch_size = noise_batch_X * n_items;
  noise_batch_index = noise_batch_size + 1; // to trigger initial noise generation

  do
  {
    // check timeout
    for (size_t i = 0; i < evidence.size();) {
      if (passed_t[i] < max_t) {
        i++;
      }
      else{
        // timeout - remove this item from consideration
        item_idx.erase(item_idx.begin() + i);
        evidence.erase(evidence.begin() + i);
        passed_t.erase(passed_t.begin() + i);
        n_determined++;
        n_undetermined--;
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
        n_determined++;
        n_undetermined--;

        evidence.erase(evidence.begin() + i);
        item_idx.erase(item_idx.begin() + i);
        passed_t.erase(passed_t.begin() + i);
        // only allow one item to be recalled
        break;
      }
    }

    // update evidence for remaining items
    for (size_t i = 0; i < evidence.size(); i++) {
      passed_t[i] += dt;
      double noise = noise_batch[noise_batch_index + i];
      switch (noise_type) {
        case ADDITIVE:
          evidence[i] = evidence[i] + V_dt[item_idx[i]] + noise;
          break;
        case MULTIPLICATIVE:
          evidence[i] = evidence[i] * (1.0 + noise) + V_dt[item_idx[i]];
          break;
        default:
          stop("Unknown noise type");
      }
    }
    noise_batch_index += evidence.size();
  } while (n_undetermined > 0 && n_recalled < max_reached);
  
  // Build output using STL vectors and convert to Rcpp types only at the end
  if (n_recalled > 0) {
    // Create final vectors with exact size needed
    IntegerVector output_item_indexes(n_recalled);
    NumericVector final_rts(n_recalled);
    IntegerVector final_pos(n_recalled);
    
    // Copy data from STL vectors to Rcpp vectors
    for (int i = 0; i < n_recalled; i++) {
      output_item_indexes[i] = reached_item_idx[i]; // words are already 1-based
      final_rts[i] = rts[i]; // rts already include ndt in passed_t
      final_pos[i] = i + 1; // Position list (1-based)
    }
    
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