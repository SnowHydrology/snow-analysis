# Welcome

This repo supports research by a Powell Center Synthesis Working Group exploring the "critical zone as a mediator of hydroclimate-ecosystem asynchrony."

It includes three notebooks in the `/analysis` directory that:

1. Programmatically access SNOTEL
2. Define a data-driven subset of SNOTEL stations and compute a series of snowpack metrics
3. Access and analyze spatial snowpack data

Supporting data can be found in the `/data` directory.

# Getting Started

You can read a static version of the notebooks by opening each of the three PDF documents in `/analysis`. However, I recommend the interactive versions (either the .RMD or .nb.html files).

To run and deploy them, you need:

- An up-to-date version of R
  - I used 4.4.2
- The following packages:
  - tidyverse
  - snotelr
  - cowplot
  - knitr
  - Kendall
  - trend
  - terra
  - sf
  - exactextractr
  
Enjoy!