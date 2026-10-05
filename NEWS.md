# affirmoTM 0.1.0

First release.

* `tm_run()` runs the transition model for iABC and usual care from the input files in `inputs/` and writes the
  results to `outputs/`: costs, QALYs and events for each arm, the incremental cost and QALYs, the ICER with its
  quadrant, and the cost-effectiveness plane.
* Inputs: two transition matrices, the cohort, entry states, costs, utilities, the intervention cost and
  `settings.yaml`. Matrices, costs and utilities may vary by stratum and by year since entry.
* Every input is checked before a run starts.
* `tm_example_inputs()` copies a complete set of example inputs into an analysis folder.
* Six PDF tutorials in `tutorials/`.
