# Frequently Asked Questions (FAQ) - StepRegShiny

Welcome to the StepRegShiny FAQ! This document addresses common questions about installation, usage, and troubleshooting. If you have a question that isn't covered here, please [open an issue](https://github.com/JunhuiLi1017/StepRegShiny/issues/new) on GitHub.

## Installation

### Q: What's the best way to install StepRegShiny?

**A:** We recommend using the `pak` package manager for the most reliable installation:

```r
pak::pkg_install("StepRegShiny")
```

# Using pak with GitHub
```r
pak::pkg_install("JunhuiLi1017/StepRegShiny")
```
Alternative installation methods:

```r
# From CRAN (if available)
install.packages("StepRegShiny")
```

```r
# From GitHub
devtools::install_github("JunhuiLi1017/StepRegShiny")
```

**Red Hat/CentOS/Fedora:**
```bash
sudo yum install cairo-devel libwebp-devel ImageMagick-c++-devel ImageMagick-devel
# or for newer versions:
sudo dnf install cairo-devel libwebp-devel ImageMagick-c++-devel ImageMagick-devel
```

**macOS:**
```bash
# Install Homebrew if you haven't already
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install dependencies
brew install cairo imagemagick webp

# For X11 support (if needed)
brew install --cask xquartz
```

**Windows:**
Most dependencies should be automatically handled, but if you encounter issues, try installing Rtools and ensure you have the latest version of R.

## Usage

### Q: How do I use StepRegShiny?

**A:** You have several options:

1. **Local GUI Application:**
   ```r
   library(StepRegShiny)
   StepRegGUI()
   ```

2. **Online Shiny Application:**
   Visit the [StepReg Shiny application](https://junhuili1017.shinyapps.io/StepRegShiny/) in your web browser.

3. **For StepReg package usage:**
   Refer to the [StepReg vignette](https://CRAN.R-project.org/package=StepReg/vignettes/StepReg.html) for detailed examples.

### Q: Can I use the application without installing the package?

**A:** Yes! You can use the [online Shiny application](https://junhuili1017.shinyapps.io/StepRegShiny/) directly in your web browser without any local installation.

## Troubleshooting

### Q: The application won't start or crashes. What should I check?

**A:** Try these troubleshooting steps:

1. **Check R version:** Ensure you're using R 4.0 or later
2. **Update packages:** Run `update.packages(ask = FALSE)`
3. **Clear R cache:** Restart R and try again
4. **Check system libraries:** Ensure all required system libraries are installed (see installation section)
5. **Memory issues:** Try closing other applications if you're running out of memory

### Q: I'm getting "X11 library" errors on macOS. How do I fix this?

**A:** Install XQuartz:
```bash
brew install --cask xquartz
```
Then restart your computer and try again.

### Q: The Shiny application is slow or unresponsive. What can I do?

**A:** 
- Try the local GUI version: `StepRegGUI()`
- Close other browser tabs and applications
- Use a modern web browser (Chrome, Firefox, Safari, Edge)
- Check your internet connection if using the online version

## Getting Help

### Q: Where can I get additional help?

**A:** 
- **Documentation:** Check the [StepReg vignette](https://CRAN.R-project.org/package=StepReg/vignettes/StepReg.html)
- **Issues:** Report bugs or request features on [GitHub Issues](https://github.com/JunhuiLi1017/StepRegShiny/issues)
- **Online Demo:** Try the [StepReg Shiny application](https://junhuili1017.shinyapps.io/StepRegShiny/)

### Q: How do I report a bug?

**A:** Please [open an issue](https://github.com/JunhuiLi1017/StepRegShiny/issues/new) on GitHub with:
- Your operating system and R version
- The complete error message
- Steps to reproduce the problem
- Your session info: `sessionInfo()`
