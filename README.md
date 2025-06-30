# Eckmann Revisited: NOAC Therapy Analysis

A comprehensive decision-analytic model examining Novel Oral Anticoagulant (NOAC) therapy in patients with atrial fibrillation, focusing on treatment thresholds and net benefit analysis across varying stroke risk levels.

## Project Overview

This project evaluates the threshold at which anticoagulation provides a net benefit for patients with atrial fibrillation (AF) using a **Markov decision model**. By simulating different anticoagulation strategies, we assess the balance between stroke prevention and bleeding risks.

While randomized trials have clearly demonstrated the benefits of anticoagulant therapy in atrial fibrillation (AF) patients at high risk of ischemic stroke, less is known about the benefit in lower-risk patients. This project addresses the critical question: exactly how low baseline stroke risk justifies attempts to reduce it with oral anticoagulants?

This study develops a Markov decision model to estimate the impact of initiating non-vitamin K antagonist oral anticoagulants (NOACs) on quality-adjusted life-years (QALYs) in AF patients across a range of non-anticoagulated ischemic stroke risk levels. The analysis includes:

- **Main analysis** for 60, 70, and 80-year-old patient cohorts
- **Probabilistic sensitivity analysis** with Monte Carlo simulations
- **Tipping point analysis** to identify treatment thresholds
- **Comprehensive figure generation** for manuscript publication

## Project Structure

### Main Analysis Scripts
- `01-main-analysis-70-year-olds.R` - Primary analysis for 70-year-old cohort
- `02-main-analysis-60-year-olds.R` - Analysis for 60-year-old cohort  
- `03-main-analysis-80-year-olds.R` - Analysis for 80-year-old cohort

### Sensitivity Analysis
- `04-probabilistic-sensitivity-analysis-simulation.R` - Monte Carlo simulations
- `05-merge-probabilistic-sensitivity-analysis-results.R` - Results aggregation

### Tipping Point Analysis
- `06-figure1.R` - Treatment threshold analysis for 70-year-old cohort (main analysis)
- `12-tipping-point-60-year-olds.R` - Treatment threshold analysis for 60-year-old cohort
- `13-tipping-point-80-year-olds.R` - Treatment threshold analysis for 80-year-old cohort

### Figure Generation
- `06-figure1.R` - Main manuscript Figure 1
- `07-figure2.R` - Main manuscript Figure 2
- `09_figure_S1.R` - Supplementary Figure S1
- `10-figure_S2.R` - Supplementary Figure S2
- `11-figure_S3.R` - Supplementary Figure S3
- `14-figure_S4.R` - Supplementary Figure S4 (probability analysis)
- `15-figure_S5.R` - Supplementary Figure S5 (QALY analysis)

### Output Directories
- `figures/` - Generated plots in PDF and PNG formats

## Getting Started

### Prerequisites

This project requires R with the following packages:
- `ggplot2` - Data visualization
- `dplyr` - Data manipulation
- `showtext` - Font management for plots
- `magick` - Image processing
- `grid` - Graphics layout
- `gt` - Table generation
- `ggthemes` - Additional plot themes
- `pacman` - Package management

### Installation

1. Clone this repository
2. Install required packages:
```r
install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr, showtext, magick, grid, gt, ggthemes)
```

### Running the Analysis

#### Complete Analysis Pipeline
Run scripts in numerical order for full analysis.

## Key Outputs
- **Treatment Tipping Points**: Annual non-anticoagulated ischemic stroke risk thresholds where NOAC therapy provides net benefit
- **QALY Differences**: Quality-adjusted life year gains/losses comparing NOAC therapy vs. withholding treatment
- **Probability Analysis**: Likelihood of treatment benefit across varying stroke risk levels in sensitivity analyses
- **Risk-Stratified Outcomes**: Benefits quantified across different baseline stroke risk categories

## Analysis Methodology

The analysis employs a **Markov decision model** incorporating:

### Data Sources
- **Randomized controlled trial data** on NOAC effects on:
  - Severity and risk of ischemic stroke
  - Major bleeding events
  - Mortality outcomes
- **Quality of life impact data** from previous evidence
- **Non-anticoagulated event rates** averaged from observational studies

### Statistical Methods
- **Markov modeling** for long-term outcome projection across AF patient populations
- **Monte Carlo simulation** for uncertainty analysis
- **Probabilistic sensitivity analysis** with 1,000 iterations to account for uncertainty in treatment effects
- **Linear regression** for tipping point estimation
- **Quality-adjusted life years (QALYs)** as primary outcome measure

### Clinical Focus
- **Treatment threshold identification** for when NOACs provide net benefit
- **Risk-stratified analysis** across varying non-anticoagulated ischemic stroke risk levels
- **Age-specific modeling** for 60, 70, and 80-year-old patient cohorts

## Research Keywords

Atrial fibrillation • Anticoagulation • Stroke risk • Treatment threshold • Net benefit • Quality of life • NOACs • Decision analysis

## Citation

If you use this code or methodology, please cite the associated manuscript or the Zenodo page (DOI 10.5281/zenodo.14941360).

## Contributors

- Winstén, Aleksi
- Langén, Ville
- Teppo, Konsta

## License

This project is licensed under the MIT License.


---

*Last updated: June 29, 2025* 