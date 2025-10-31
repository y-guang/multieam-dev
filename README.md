# multieam

multieam is a high-performance simulation infrastructure for the Drift Diffusion Model (DDM) and its generalized variants,  two-boundary models, racing models, leaky competing accumulator (LCA) models.

It provides a modern, consistent API for simulation-based modelling, making it easy to define, run, and evaluate large-scale evidence accumulation experiments with high computational efficiency.

## Features

- ⚙️ High-Performance Backend
  - **Pure C++ core** with fast vectorization-friendly algorithms designed. 100x speedup compared with naive python implementations.
  - Memory-efficient computation, enabling more than **10 million** trials on a standard desktop machine.
  - Parallel execution with near-linear speed-up across multiple CPU cores.
- 🧩 Modern API
  - Flexible architecture: composable api, defines several formulas, you can easily customize your own models.
  - modern R api：Built around modern R conventions, it provides a declarative, tidy-style interface.
  - Unified data pipeline: provides a consistent set of data manipulation and summarization utilities for seamless simulation, evaluation, and visualization.

## Example

