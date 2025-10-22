# StepRegShiny NEWS

## Version 1.0.0 (2025-01-27)

### Initial Release

- **NEW PACKAGE**: StepRegShiny is a new companion package to StepReg
- **FEATURE**: Interactive Shiny web interface for stepwise regression analysis
- **FEATURE**: Support for multiple regression types (linear, logistic, Cox, Poisson, Gamma, negative binomial)
- **FEATURE**: Multiple stepwise selection strategies (forward, backward, bidirectional, subset)
- **FEATURE**: Comprehensive visualization tools and report generation
- **FEATURE**: Model voting across different strategies and metrics
- **CHANGE**: Shiny components separated from main StepReg package for better modularity
- **DEPENDENCY**: Requires StepReg package to function

### Migration from StepReg

This package contains the Shiny application components that were previously part of the main StepReg package. Users who were using the `StepRegShinyApp()` function from StepReg should now install and use this separate package.

**Migration steps:**
1. Install StepRegShiny: `install.packages("StepRegShiny")`
2. Use the new function: `StepRegShiny::StepRegShinyApp()`
3. The original StepReg package no longer includes Shiny components
