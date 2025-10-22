# StepRegShiny

A Shiny application package that provides an interactive web interface for the StepReg package.

## Description

StepRegShiny is a companion package to StepReg that provides a comprehensive web-based interface for performing stepwise regression analysis. This package contains the Shiny application components that were previously part of the main StepReg package, allowing for better separation of concerns and independent development of the web interface.

## Features

- Interactive data upload and preparation tools
- Support for multiple regression types (linear, logistic, Cox, Poisson, Gamma, negative binomial)
- Multiple stepwise selection strategies (forward, backward, bidirectional, subset)
- Comprehensive visualization tools
- Report generation capabilities
- Model voting across different strategies and metrics

## Installation

```r
# Install from CRAN (when available)
install.packages("StepRegShiny")

# Or install from GitHub
devtools::install_github("JunhuiLi1017/StepReg")
```

## Usage

```r
library(StepRegShiny)

# Launch the Shiny application
StepRegShinyApp()
```

## Dependencies

This package requires the StepReg package to function properly. Make sure you have StepReg installed:

```r
install.packages("StepReg")
```

## Author

- **Maintainer:** Junhui Li <junhui.li11@umassmed.edu>
- **Authors:** Junhui Li, Kai Hu, Xiaohuan Lu, Sushmita N Nayak, Cesar Bautista Sotelo, Michael A Lodato, Wenxin Liu, Lihua Julie Zhu

## License

MIT + file LICENSE

## Links

- [GitHub Repository](https://github.com/JunhuiLi1017/StepReg)
- [CRAN Package Page](https://CRAN.R-project.org/package=StepReg)
- [Report Issues](https://github.com/JunhuiLi1017/StepReg/issues)
# StepRegShiny
