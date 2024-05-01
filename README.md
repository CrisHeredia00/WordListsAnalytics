# ODMeans R Package

## Description 

Application to estimate statistical values using properties provided by a group of individuals to describe concepts using 'shiny'. It estimates the underlying distribution to generate new descriptive words (Canessa et al. 2023), applies a new clustering model, and uses simulations to estimate the probability that two persons describe the same words based on their descriptions (Canessa et al. 2022).

## Installation

Install from CRAN:

```R
install.packages("WordListsAnalytics")
```

## Getting started

To execute WordListsAnalytics Shiny app you can use the function WordListsAnalytics(). This will open a Shiny app where data can be upload and manipulated.

For example:

```R
WordListsAnalytics()
```

## Data available

In the package there are two PLT data frames that can be loaded: CPN_27 and CPN_120.

To load the CPN_27 data into the R environment:

```R
data(CPN_27)
```

To load the CPN_120 data into the R environment:

```R
data(CPN_120)
```

## License

This project is licensed under GPL >= 3.
