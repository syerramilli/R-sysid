# sysid: System Identification in R

`sysid` provides functions for constructing mathematical models of dynamical systems from measured input-output data. It supports discrete-time model estimation using time-domain and frequency-domain data.

Supported model structures include ARX, ARMAX, Output Error (OE), Box-Jenkins (BJ), and instrumental variable methods.

## Installation

Install from CRAN:

```r
install.packages("sysid")
```

## Usage

```r
library(sysid)

# Load example data (simulated ARX process)
data(arxsim)

# Split into training and validation sets
train <- dataSlice(arxsim, end = 1500)
test  <- dataSlice(arxsim, start = 1501)

# Estimate an ARX model: na=1, nb=2, nk=2
model <- arx(train, order = c(1, 2, 2))
print(model)

# Compare model predictions against validation data
compare(test, model)

# Plot residual diagnostics
residplot(model)
```

## Documentation

Full documentation is available on [CRAN](https://CRAN.R-project.org/package=sysid).

## Citation

If you use `sysid` in your research, please cite:

> Yerramilli, S., Moudgalya, K. M., & Tangirala, A. K. (2017, January). SYSID: An open-source library for system identification. In 2017 Indian Control Conference (ICC) (pp. 53-58). IEEE.


BibTeX entry:

```bibtex
@inproceedings{yerramilli2017sysid,
  title={SYSID: An open-source library for system identification},
  author={Yerramilli, Suraj and Moudgalya, Kannan M and Tangirala, Arun K},
  booktitle={2017 Indian Control Conference (ICC)},
  pages={53--58},
  year={2017},
  organization={IEEE}
}
```

## License

GPL-3
