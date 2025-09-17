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
  
  // Main simulation loop
  while (n_recalled < max_reached && t <= max_t) {
    int recall_idx = n_recalled;
    double A_current = A[recall_idx];
    
    t += dt;
    
    // Generate noise using the custom noise function
    // Pass n_items and dt as arguments to the noise function
    NumericVector noise;
    try {
      SEXP noise_result = noise_func(n_items, dt);
      noise = as<NumericVector>(noise_result);
      
      // Validate that the noise function returned the correct number of values
      if (noise.size() != n_items) {
        stop("Custom noise function must return a vector of length n_items");
      }
    } catch (const std::exception& e) {
      stop("Error calling custom noise function: " + std::string(e.what()));
    }
    
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