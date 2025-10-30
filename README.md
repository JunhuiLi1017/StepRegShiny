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
pak::install_package("StepRegShiny")
install.packages("StepRegShiny")

# Or install from GitHub
devtools::install_github("JunhuiLi1017/StepRegShiny")
```

## Usage

```r
library(StepRegShiny)

# Launch the Shiny application
StepRegGUI()
```

## Dependencies

This package requires the StepReg package to function properly. Make sure you have StepReg installed.

## Author

- **Maintainer:** Junhui Li <junhui.li11@umassmed.edu>
- **Authors:** Junhui Li, Kai Hu, Xiaohuan Lu, Sushmita N Nayak, Cesar Bautista Sotelo, Michael A Lodato, Wenxin Liu, Lihua Julie Zhu

## License

MIT + file LICENSE

## Links

- [GitHub Repository](https://github.com/JunhuiLi1017/StepRegShiny)
- [CRAN Package Page](https://CRAN.R-project.org/package=StepRegShiny)
- [Report Issues](https://github.com/JunhuiLi1017/StepRegShiny/issues)
- [FAQ](https://github.com/JunhuiLi1017/StepRegShiny/blob/main/inst/FAQ.md)
