#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
List accumulate_evidence_ddm_naive(
  NumericVector A,
  NumericVector V,
  NumericVector ndt,
  double dt,
  int max_reached,
  double max_t,
  String noise_mechanism = "add"
) {
  int n_items = V.size();
  
  // Input validation
  if (A.size() > n_items) {
    stop("Length of A must be <= number of items");
  }
  if (max_reached <= 0 || max_reached > n_items) {
    stop("max_reached must be > 0 and <= n_items");
  }
  if (dt <= 0 || max_t <= 0) {
    stop("dt and max_t must be > 0");
  }
  
  // Copy V to STL vector and ensure values are positive (set negative values to small positive)
  std::vector<double> V_local(n_items);
  for (int i = 0; i < n_items; i++) {
    V_local[i] = (V[i] < 0) ? 1e-8 : V[i];
  }
  
  // Initialize variables using STL containers for performance
  std::vector<double> evidence(n_items, 0.0);
  std::vector<bool> reached(n_items, false);
  
  // Pre-allocate result vectors using STL
  std::vector<int> words;
  std::vector<double> rts;
  words.reserve(max_reached);
  rts.reserve(max_reached);
  int n_recalled = 0;
  double t = 0.0;
  
  // Main simulation loop
  while (n_recalled < max_reached && t <= max_t) {
    int recall_idx = n_recalled;
    double A_current = A[recall_idx];
    
    t += dt;
    
    // Generate noise (simple normal noise for now)
    NumericVector noise = rnorm(n_items, 0.0, sqrt(dt));
    
    // Update evidence based on noise mechanism using STL containers
    if (noise_mechanism == "add") {
      for (int i = 0; i < n_items; i++) {
        evidence[i] = evidence[i] + V_local[i] * dt + noise[i];
      }
    } else if (noise_mechanism == "mult") {
      for (int i = 0; i < n_items; i++) {
        evidence[i] = evidence[i] * (1.0 + noise[i]) + V_local[i] * dt;
      }
    } else {
      stop("noise_mechanism must be 'add' or 'mult'");
    }
    
    // Check for threshold crossings using STL vector
    std::vector<int> just_cross;
    just_cross.reserve(n_items); // Reserve space for better performance
    for (int i = 0; i < n_items; i++) {
      if (!reached[i] && evidence[i] >= A_current) {
        just_cross.push_back(i + 1); // R uses 1-based indexing
      }
    }
    
    // If any items crossed threshold, randomly select one
    if (just_cross.size() > 0) {
      IntegerVector rcpp_just_cross(just_cross.begin(), just_cross.end());
      int selected_idx = sample(rcpp_just_cross, 1)[0] - 1; // Convert back to 0-based
      reached[selected_idx] = true;
      words.push_back(selected_idx + 1); // Store as 1-based for R
      rts.push_back(t);
      n_recalled++;
    }
  }
  
  // Build output using STL vectors and convert to Rcpp types only at the end
  if (n_recalled > 0) {
    // Create final vectors with exact size needed
    IntegerVector final_words(n_recalled);
    NumericVector final_rts(n_recalled);
    IntegerVector final_pos(n_recalled);
    
    // Copy data from STL vectors to Rcpp vectors and add non-decision times
    for (int i = 0; i < n_recalled; i++) {
      final_words[i] = words[i];
      final_rts[i] = rts[i] + ndt[words[i] - 1]; // words are 1-based, ndt is 0-based
      final_pos[i] = i + 1; // Position list (1-based)
    }
    
    return List::create(
      Named("words") = final_words,
      Named("rts") = final_rts,
      Named("pos") = final_pos
    );
  } else {
    return List::create(
      Named("words") = IntegerVector(0),
      Named("rts") = NumericVector(0),
      Named("pos") = IntegerVector(0)
    );
  }
}


// [[Rcpp::export]]
List accumulate_evidence_ddm_naive_with_custom_noise(
  NumericVector A,
  NumericVector V,
  NumericVector ndt,
  double dt,
  int max_reached,
  double max_t,
  String noise_mechanism = "add",
  Function noise_func = R_NilValue
) {
  int n_items = V.size();
  
  // Input validation
  if (A.size() > n_items) {
    stop("Length of A must be <= number of items");
  }
  if (max_reached <= 0 || max_reached > n_items) {
    stop("max_reached must be > 0 and <= n_items");
  }
  if (dt <= 0 || max_t <= 0) {
    stop("dt and max_t must be > 0");
  }
  
  // Check if noise function is provided
  if (Rf_isNull(noise_func)) {
    stop("noise_func parameter is required and cannot be NULL");
  }
  
  // Copy V to STL vector and ensure values are positive (set negative values to small positive)
  std::vector<double> V_local(n_items);
  for (int i = 0; i < n_items; i++) {
    V_local[i] = (V[i] < 0) ? 1e-8 : V[i];
  }
  
  // Initialize variables using STL containers for performance
  std::vector<double> evidence(n_items, 0.0);
  std::vector<bool> reached(n_items, false);
  
  // Pre-allocate result vectors using STL
  std::vector<int> words;
  std::vector<double> rts;
  words.reserve(max_reached);
  rts.reserve(max_reached);
  int n_recalled = 0;
  double t = 0.0;
  
  // Noise batching variables for performance
  NumericVector noise_batch;
  int noise_batch_index = 0;
  int noise_batch_size = 0;
  
  // Main simulation loop
  while (n_recalled < max_reached && t <= max_t) {
    int recall_idx = n_recalled;
    double A_current = A[recall_idx];
    
    t += dt;
    
    // Generate noise using the custom noise function with batching for performance
    // Check if we need to get a new batch of noise values
    if (noise_batch_index + n_items > noise_batch_size) {
      // Calculate mean of V_local
      double mean_V = 0.0;
      for (int i = 0; i < n_items; i++) {
        mean_V += V_local[i];
      }
      mean_V /= n_items;
      
      // Calculate clipped number of noise values: clip(n_items, A_current/mean_V, 10000*n_items)
      double ratio = A_current / mean_V;
      int noise_count = static_cast<int>(std::max(static_cast<double>(n_items), 
                                        std::min(ratio, static_cast<double>(10000 * n_items))));
      
      try {
        SEXP noise_result = noise_func(noise_count, dt);
        noise_batch = as<NumericVector>(noise_result);
        noise_batch_size = noise_batch.size();
        noise_batch_index = 0;
      } catch (const std::exception& e) {
        stop("Error calling custom noise function: " + std::string(e.what()));
      }
    }
    
    // Extract n_items noise values from the current batch
    NumericVector noise(n_items);
    for (int i = 0; i < n_items; i++) {
      noise[i] = noise_batch[noise_batch_index + i];
    }
    noise_batch_index += n_items;
    
    // Update evidence based on noise mechanism using STL containers
    if (noise_mechanism == "add") {
      for (int i = 0; i < n_items; i++) {
        evidence[i] = evidence[i] + V_local[i] * dt + noise[i];
      }
    } else if (noise_mechanism == "mult") {
      for (int i = 0; i < n_items; i++) {
        evidence[i] = evidence[i] * (1.0 + noise[i]) + V_local[i] * dt;
      } 
    } else {
      stop("noise_mechanism must be 'add' or 'mult'");
    }
    
    // Check for threshold crossings using STL vector
    std::vector<int> just_cross;
    just_cross.reserve(n_items); // Reserve space for better performance
    for (int i = 0; i < n_items; i++) {
      if (!reached[i] && evidence[i] >= A_current) {
        just_cross.push_back(i + 1); // R uses 1-based indexing
      }
    }
    
    // If any items crossed threshold, randomly select one
    if (just_cross.size() > 0) {
      IntegerVector rcpp_just_cross(just_cross.begin(), just_cross.end());
      int selected_idx = sample(rcpp_just_cross, 1)[0] - 1; // Convert back to 0-based
      reached[selected_idx] = true;
      words.push_back(selected_idx + 1); // Store as 1-based for R
      rts.push_back(t);
      n_recalled++;
    }
  }
  
  // Build output using STL vectors and convert to Rcpp types only at the end
  if (n_recalled > 0) {
    // Create final vectors with exact size needed
    IntegerVector final_words(n_recalled);
    NumericVector final_rts(n_recalled);
    IntegerVector final_pos(n_recalled);
    
    // Copy data from STL vectors to Rcpp vectors and add non-decision times
    for (int i = 0; i < n_recalled; i++) {
      final_words[i] = words[i];
      final_rts[i] = rts[i] + ndt[words[i] - 1]; // words are 1-based, ndt is 0-based
      final_pos[i] = i + 1; // Position list (1-based)
    }
    
    return List::create(
      Named("words") = final_words,
      Named("rts") = final_rts,
      Named("pos") = final_pos
    );
  } else {
    return List::create(
      Named("words") = IntegerVector(0),
      Named("rts") = NumericVector(0),
      Named("pos") = IntegerVector(0)
    );
  }
}

// [[Rcpp::export]]
List accumulate_evidence_ddm_opt(
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
  if (dt <= 0 || max_t <= 0) {
    stop("dt and max_t must be > 0");
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
  std::vector<double> V_local(n_items);
  for (int i = 0; i < n_items; i++) {
    V_local[i] = (V[i] < 0) ? 1e-8 : V[i];
  }

  // Initialize status
  std::vector<double> evidence(n_items, 0.0);
  std::vector<int> item_idx(n_items);
  std::iota(item_idx.begin(), item_idx.end(), 0);
  std::vector<double> passed_t(n_items);
  for (int i = 0; i < n_items; i++) {
    passed_t[i] = ndt[i];
  }
  int n_recalled = 0;
  int n_determined = 0;
  int n_undetermined = n_items;

  // Pre-allocate result vectors using STL
  std::vector<int> words;
  std::vector<double> rts;
  words.reserve(max_reached);
  rts.reserve(max_reached);

  // Noise batching
  NumericVector noise_batch;
  int noise_batch_index = 0;
  double heuristic_steps = max_t / dt / 10;
  int noise_batch_X = static_cast<int>(std::max(MIN_BATCH_X, std::min(MAX_BATCH_X, heuristic_steps)));
  int noise_batch_size = noise_batch_X * n_items;
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
    }

    // check evidence reached threshold
    for (size_t i = 0; i < evidence.size();) {
      if (evidence[i] < A[n_recalled]) {
        i++;
      }
      else {
        int selected_idx = item_idx[i];
        words.push_back(selected_idx + 1);
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
          evidence[i] = evidence[i] + V_local[item_idx[i]] * dt + noise;
          break;
        case MULTIPLICATIVE:
          evidence[i] = evidence[i] * (1.0 + noise) + V_local[item_idx[i]] * dt;
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
    IntegerVector final_words(n_recalled);
    NumericVector final_rts(n_recalled);
    IntegerVector final_pos(n_recalled);
    
    // Copy data from STL vectors to Rcpp vectors
    for (int i = 0; i < n_recalled; i++) {
      final_words[i] = words[i]; // words are already 1-based
      final_rts[i] = rts[i]; // rts already include ndt in passed_t
      final_pos[i] = i + 1; // Position list (1-based)
    }
    
    return List::create(
      Named("words") = final_words,
      Named("rts") = final_rts,
      Named("pos") = final_pos
    );
  } else {
    return List::create(
      Named("words") = IntegerVector(0),
      Named("rts") = NumericVector(0),
      Named("pos") = IntegerVector(0)
    );
  }
}