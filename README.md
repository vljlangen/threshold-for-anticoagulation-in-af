# Estimating the Threshold for Anticoagulation in Atrial Fibrillation

## Overview

This project evaluates the threshold at which anticoagulation provides a net benefit for patients with atrial fibrillation (AF) using a **Markov decision model**. By simulating different anticoagulation strategies, we assess the balance between stroke prevention and bleeding risks.

## Methods

- **Markov Decision Model**: A state-transition model capturing stroke, bleeding events, and mortality.
- **Data Sources**: Clinical risk scores, event probabilities, and outcome utilities from existing literature.
- **Threshold Analysis**: Identifying the CHA2DS2-VASc score at which anticoagulation provides a net clinical benefit.

## Repository Structure

```
├── README.md                 # Project documentation
├── LICENSE                   # Project license
├── main_analyses.R           # Main analysis script
├── figure1.R                 # Script for generating Figure 1
├── figure2.R                 # Script for generating Figure 2
├── supplementary_figure2.R   # Script for generating Supplementary Figure 2
```

## Usage

### Requirements

- **R (tested on version 4.4.1)**

### Running the Model

```r
source("main_analyses.R")
source("figure1.R")
source("figure2.R")
source("supplementary_figure2.R")
```

## Authors & Contributions

- **Aleksi Winstén, Ville Langén, Konsta Teppo** – Model development, analysis, and manuscript writing.

## License

This project is licensed under the MIT License.

