# Grabny: Spatial Analysis of Ride-Hailing Patterns in Singapore

Grabny is a Shiny web application that allows non-technical users to perform and visualise Network Kernel Density Estimation (NKDE) on road networks — no programming required. It was developed as part of my university capstone project analysing large-scale Grab ride-hailing pick-up and drop-off trajectory data in Singapore, using spatial analysis techniques including Kernel Density Estimation (KDE) and NKDE.


## Overview

Traditional spatial KDE struggles to accurately analyse point patterns constrained by road networks. This project addresses that limitation by applying NKDE to better detect travel demand patterns along road networks — enabling transportation service providers to optimise service efficiency, and urban planners to study pick-up/drop-off zone design and manage traffic flows.

The project culminates in **Grabny**, an interactive Shiny web application that allows non-technical users to implement and visualise NKDE models without any programming knowledge.


## Features

- **Data Preparation** — Cleaning and preprocessing of large-scale Grab trajectory data
- **KDE Analysis** — Spatial and spatio-temporal kernel density estimation using `spatstat` and `ks`
- **NKDE Analysis** — Network-constrained KDE using the `spNetwork` R package, with support for multiple NKDE methods and adaptive bandwidths
- **Grabny App** — A user-friendly web application featuring:
  - Data-driven optimal bandwidth selection tool
  - NKDE modelling tool with choice of fixed or adaptive bandwidth estimators
  - Interactive map visualisation of road segment densities, with and without basemap


## Project Structure

```
.
├── Analysis/
│   ├── KDE/          # Spatial & spatio-temporal KDE notebooks
│   ├── NKDE/         # Network KDE notebooks & output visualisations
│   ├── Setup/        # Data preparation & preprocessing
│   └── Shiny/
│       └── Grabny/   # Shiny web application
├── Images/           # Project images and logo
├── _quarto.yml       # Quarto site configuration
├── index.qmd         # Site homepage
├── about.qmd         # About page
└── styles.css / styles.scss
```


## Tech Stack

| Tool | Purpose |
|---|---|
| R | Primary analysis language |
| Quarto | Project blog / documentation site |
| Shiny | Interactive web application |
| `spNetwork` | Network KDE implementation |
| `spatstat` | Spatial KDE |
| `ks` | Temporal KDE |
| Leaflet | Interactive map visualisation |
| Netlify | Blog hosting |


## Getting Started

### Prerequisites

- R (≥ 4.0)
- RStudio
- Quarto CLI

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/magdalenecjw/grabny.git
   cd grabny
   ```

2. Open `Capstone.Rproj` in RStudio.

3. Install the required R packages:
   ```r
   install.packages(c("shiny", "spNetwork", "spatstat", "ks", "leaflet", "tidyverse"))
   ```

### Running the Shiny App

Open the `Analysis/Shiny/Grabny/` folder in RStudio and run:

```r
shiny::runApp("Analysis/Shiny/Grabny")
```


## Documentation

Full data exploration, methodology, and findings are documented on the project blog:
[capstonejourney.netlify.app](https://capstonejourney.netlify.app)

---

## Acknowledgements

This project was developed as part of my capstone requirement for the Master of IT in Business programme at Singapore Management University (SMU), under the supervision of Prof Kam Tin Seong. Ride-hailing trajectory data is sourced from the Grab-Posisi dataset (Huang et al., 2019).


## References

Huang, X., Yin, Y., Lim, S., Wang, G., Hu, B., Varadarajan, J., Zheng, S., Bulusu, A., & Zimmermann, R. (2019). Grab-Posisi. Proceedings of the 3rd ACM SIGSPATIAL International Workshop on Prediction of Human Mobility, 1–10. https://doi.org/10.1145/3356995.3364536
