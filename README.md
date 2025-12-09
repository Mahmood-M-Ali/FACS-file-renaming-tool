This work is licensed under Creative Commons Attribution-NonCommercial 4.0 International License.

Author
Mahmood Mohammed Ali Université Grenoble Alpes | Institute for Advanced Biosciences Epigenetics of Regeneration and Cancer Group

mahmood.mohammed-ali@univ-grenoble-alpes.fr 
GitHub: @Mahmood-M-Ali 
LinkedIn: Mahmood Mohammed Ali

#  FACS File Renaming Tool — Helper Utility for FlowIC50

[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-App-blue)](https://shiny.rstudio.com/)
[![License: CC BY-NC 4.0](https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/4.0/)
[![DOI](https://zenodo.org/badge/1113365916.svg)](https://doi.org/10.5281/zenodo.17873024)


---

## Overview

This Shiny application provides a **secondary utility** to support the main [FlowIC50 Analyzer](https://github.com/Mahmood-M-Ali/FlowIC50).  
It was designed to help researchers **rename plate-based FCS files** into a standardized format before running IC50 analysis.

By ensuring consistent file naming, this tool makes downstream analysis with the IC50 Analyzer smoother, reproducible, and publication-ready.

---

## Related Project

- **Primary Tool:** [FlowIC50 Analyzer](https://github.com/Mahmood-M-Ali/FlowIC50)  
- **Web App:** [FACS-Analysis on Hugging Face](https://huggingface.co/spaces/mahmood-iab/FACS-Analysis)  
- **DOI:** [10.5281/zenodo.17872796](https://doi.org/10.5281/zenodo.17872796)

---

## Features

- Upload multiple `.fcs` files from plate experiments  
- Define **rows (cell lines)** and **columns (treatments & concentrations)**  
- Quick-fill option to apply treatments across all columns  
- Preview renamed files before download  
- Export renamed files as a **ZIP package**  
- Standardized format:  CellLine_Treatment_ConcentrationuM_Replicate.fcs


**Examples:**
- `Ly18_YF2_0uM_Rep1.fcs`  
- `HT_DMSO_50uM_Rep2.fcs`  
- `SUDHL4_Compound_10.5uM_Rep1.fcs`  

---

### Output
Preview Table: Original vs. renamed filenames

ZIP Download: Renamed .fcs files ready for IC50 analysis

---

## Quick Start

### Prerequisites
- R ≥ 4.0  
- Required packages: `shiny`, `bslib`, `DT`, `zip`, `dplyr`

### Run Locally
```r
# Clone the repository
git clone https://github.com/Mahmood-M-Ali/FACS-File-Renaming-Tool.git

# Launch the app
shiny::runApp("FACS-File-Renaming-Tool/app.R")



