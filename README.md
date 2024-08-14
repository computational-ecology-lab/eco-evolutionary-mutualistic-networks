# eco-evolutionary-mutualistic-networks
Simulating eco-evolutionary dynamics in mutualistic networks

In this repository we make available the code for the eco-evolutionary model simulations developed as part of the paper:

**Structural stability predicts evolutionary stability in mutualistic model ecosystems**

Authors: Miguel Lurgi & Alberto Pascual-García

**Dependencies:** The ecological model and simulations are implemented as as fortran package that must be downloaded and added to the working path of the R script provided here. This fortran package provides de routines to run the ecological dynamics of the simulations and is through R. The package can be found here: https://github.com/apascualgarcia/RatioDependent

This repository contains scipts implementing the numerical simulations as well as the routines develop to plot and present the results.

The scripts presented include:

**/src/mutualistic_evolution_console.r** - This is the main script implementing the evo-evolutionary simulations. Parameter values of delta and rho control the environmental perturbations and the competition regimes respectively. It depends on the Fortran routines mentioned above for ecological dynamics.

**/src/figures.r** - Includes all the code necessary to re-create the figures summarising the simulations outcomes presented in the paper.

**/src/structural-stability.r** - Standalone script presenting the implementation of the numerical simulations to quantify structural stability numerically, i.e. from only ecological dynamics. Depends on the ecological dynamics script in Fortran.

**/src/output-data.rda** - Data file with a summary data frame containing the output of selected simulations and that can be loaded in R to recreate the figures of the paper without running all simulations from scratch.
