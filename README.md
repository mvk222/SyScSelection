# SyScSelection
Quasi-Monte-Carlo algorithm for systematic generation of shock scenarios from an arbitrary multivariate elliptical distribution. The algorithm selects a systematic mesh of arbitrary fineness that approximately evenly covers an isoprobability ellipsoid in d dimensions.<br />
(Flood, Mark D. &amp; Korenko, George G. "Systematic Scenario Selection", Office of Financial Research Working Paper #0005, 2013)

## Installation:
```
install.packages("devtools")
library(devtools)
install_github("mvk222/SyScSelection")
library(SyScSelection)
```

## Usage:
### Example ellipsodial mesh for a normal distribution:
- Estimate the mean and covariance matrix from the data:<br />
```mu <- colMeans(data)```<br />
```sig <- cov(data)```

- The number of dimensions, d, is taken directly from the data:<br />
```d <- length(data[1,])```

- Get the size parameter for a normal dist’n at a 95% threshold:<br />
```calpha <- sizeparam_normal_distn(.95, d)```

- Create a hyperellipsoid object. Note that the constructor takes the **inverse of the disperion matrix**:<br />
```hellip <- hyperellipsoid(mu, solve(sig), calpha)```

- Scenarios are calculated as a mesh of fineness 3. The number of scenarios is a function of the dimensionality of the hyperellipsoid and the fineness of the mesh:<br />
```scenarios <- hypercube_mesh(3, hellip)```

### Example ellipsodial mesh for a t distribution:
- Estimate the mean, covariance, and degrees of freedom from the data:<br />
```mu <- colMeans(data)```<br />
```sig <- cov(data)```<br />
```nu <- dim(data)[1] - 1```

- The number of dimensions, d, is taken directly from the data:<br />
```d <- length(data[1,])```

- Get the size parameter for a normal dist’n at a 95% threshold:<br />
```calpha <- sizeparam_t_distn(.95, d, nu)```

- Create a hyperellipsoid object. Note that the constructor takes the **inverse of the disperion matrix**:<br />
```hellip <- hyperellipsoid(mu, solve(sig), calpha)```

- Scenarios are calculated as a mesh of fineness 3. The number of scenarios is a function of the dimensionality of the hyperellipsoid and the fineness of the mesh:<br />
```scenarios <- hypercube_mesh(3, hellip)```
