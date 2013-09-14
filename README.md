# AutoCast

[AutoCast][] is an R package that is a streamlined and user-friendly version of the [YourCast][] forecasting software, which focuses on generating forecasts for multiple groups over time in a single geographic region. In general, prior parameters for the smoothness of forecasts across groups and time cannot be interpreted intuitively, and it is diﬃcult to anticipate how diﬀerent levels of smoothness priors interact to produce varying degrees of smoothness and prediction error in the resulting forecasts. AutoCast allows users to address these tradeoﬀs directly by varying prior parameters across a range of values and measuring how well the model produces smooth forecasts and predicts future observations in a validation exercise.

## How to install

### Installation requirements
`AutoCast` requires [R][] version 1.9.0 or higher.

### Installation
```R
install.packages("AutoCast",repos="http://gking.harvard.edu")
```

## License
Creative Commons Attribution- Noncommercial-No Derivative Works 3.0 License, for academic use only.

[AutoCast]: http://gking.harvard.edu/software/autocast-automated-bayesian-forecasting-yourcast
[YourCast]: http://gking.harvard.edu/yourcast
[R]: http://cran.r-project.org
