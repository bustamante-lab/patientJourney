# PatientTrajectories package for WEKA
An R package to analyze patient's trajectories in the Electronic Medical Record


## Description
EBMC builds a tree-augmented naïve Bayes model (TAN). EBMC searches over the subspace of Bayesian networks that best predict the target node. To make the search efficient, it starts with an empty network and greedily identifies the set of independent parents of the target that predict it well. Then, EBMC transforms the current network into a statistically equivalent network where the parent nodes become children of the target with arcs between them. It then searches iteratively for a new set of parents given the current structure. Finally, it greedily eliminates arcs between the children nodes.


## Citation
For citation and more information refer to:

>A.L. Pineda et al. (2017). "Therapeutic importance of timely immunophenotyping of breast cancer in a resource-constrained setting: a retrospective hospital-based cohort study".


## Current development status
This package is in development by Arturo Pineda, PhD
