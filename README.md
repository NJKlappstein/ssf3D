**R package to fit three-dimensional step selection functions (SSFs) to animal tracking data**

This package contains tools to process three-dimensional tracking data for use in SSFs, based on the work in Klappstein et al. (2025). 

Reference: [Klappstein, N., Michelot, T., Togunov, R., & Flemming, J. M. (2025). Three-dimensional correlated random walks for animal movement and habitat selection. bioRxiv.](https://www.biorxiv.org/content/10.1101/2025.11.07.687274.abstract)

### Vignettes

There are two main vignettes with this package: 
- [Step selection analysis in three dimensions](https://github.com/NJKlappstein/ssf3D/blob/main/inst/ssf3D_example.pdf): tutorial with full workflow of data processing, model fitting, and interpretation
- [Simulating three-dimensional CRWs](https://github.com/NJKlappstein/ssf3D/blob/main/inst/sim_crws.html): script to simulate from isotropic, anisotropic, and biased CRWs (as well as plotting functions for the data and movement metrics) 

### Installation

You can install the package from Github with `devtools`:
``` R
devtools::install_github("NJKlappstein/ssf3D")
```

Note that the package is under development, and will be updated periodically!

