# Potools Hard Test Result

## Task
Install `potools` R package and provide a simple example of using it.

## Result

### Usage
1. Open the `Potools Hard test.R` file in RStudio.
2. Install the "Potools" package by running `install.packages("Potools")`.
3. Load the `getText` package.
4. Make sure the working directory is set to path where this repository is cloned.

### Files
- R Folder: This Folder contains a custom R Package (made by using packages like `devtools`,`roxygen2`) named as "SCCTempConverter" . 
- SCCTempConverter: Custom R package that converts the temperature from either Fahrenheit to Celsius and vice versa. It even prints the statement written in getText.
- inst/po : contains .pot, .po, .mo files generated using `translate_package()` function from potools package.
- DESCRIPTION - contains description of SCCTempConverter package.
- Potools Hard test.R : contains code of translating the language and printing as per the system language.Here the translation is from English to Spanish.

### Description
1. Created R Package called SCCTempConverter.
2. Used getText function to print the statement in the language set on the system.(if translation is provided).
3. Used potools package to translate message to Spanish.
4. Returned the translated message using getText.
