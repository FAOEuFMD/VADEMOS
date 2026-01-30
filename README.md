# VADEMOS - Vaccine Demand Estimation Model for Foot-and-Mouth Disease

[![License: CC BY-NC-SA 3.0 IGO](https://img.shields.io/badge/License-CC%20BY--NC--SA%203.0%20IGO-lightgrey.svg)](https://creativecommons.org/licenses/by-nc-sa/3.0/igo/)
[![R Version](https://img.shields.io/badge/R-%3E%3D4.1.0-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Interactive%20Web%20App-brightgreen.svg)](https://shiny.rstudio.com/)

## Overview

VADEMOS is a decision-support tool developed by the European Commission for the Control of Foot-and-Mouth Disease (EuFMD) that estimates the number of vaccine doses needed for foot-and-mouth disease (FMD) control at national and regional levels. The tool aims to bridge the gap between FMD vaccine supply and demand in endemic countries.

This deterministic quantitative model predicts vaccine dose demand based on livestock population forecasts, disease control policies related to the Progressive Control Pathway for FMD (PCP-FMD) stages, and vaccination schedules according to age demographics.

## Features

### 🔬 **Dual Vaccination Strategies**
- **Prophylactic Vaccination**: Estimates annual doses needed for routine prevention campaigns
- **Emergency Vaccination**: Calculates doses required for outbreak response within specified radii

### 📊 **Advanced Modeling**
- **AutoEnsemble Forecasting**: Combines multiple models for reliable livestock population predictions
- **GIS Integration**: Uses Global Livestock Production and Health Atlas (GLW4) density data
- **PCP-FMD Integration**: Incorporates country-specific control pathway stages

### 🌍 **Data Sources**
- **Population Data**: FAO FAOSTAT Statistical Database
- **Outbreak Data**: WOAH World Animal Health Information System (WAHIS)
- **Density Data**: FAO Gridded Livestock of the World (GLW) 4
- **PCP-FMD Stages**: EuFMD PCP-FMD Dashboard

## Installation

### Prerequisites
- R (≥ 4.1.0)
- RStudio (recommended)

### Setup
1. Clone the repository:
```bash
git clone <repository-url>
cd VADEMOS
```

2. Install dependencies using `renv`:
```r
# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
}

# Restore project dependencies
renv::restore()
```

3. Set up environment variables:
   - Copy `.Renviron.example` to `.Renviron`
   - Configure database credentials if using database logging

## Usage

### Running the Application

#### Local Development
```r
# Open R/RStudio in the project directory
shiny::runApp()
```

#### Production Deployment
The application is configured for deployment on shinyapps.io. Deployment configurations are stored in `rsconnect/`.

### Application Structure

#### **About Section**
- **Summary**: Overview of VADEMOS functionality and objectives
- **Parameters**: Detailed explanation of model inputs and assumptions
- **Mathematical Models**: Technical documentation of underlying calculations

#### **Tool Section**
Interactive interface with:
- **Parameter Selection**: Year, species, and geographical area selection
- **Vaccination Scenarios**: Prophylactic and emergency vaccination planning
- **Results Visualization**: Interactive maps and charts
- **Data Export**: Download results for further analysis

## Model Parameters

### 1. **Livestock Population Predictions**
- Uses AutoEnsemble modeling technique
- Defaults to latest FAOSTAT data (2023) when predictions are uncertain
- Supports manual data input for improved accuracy

### 2. **Vaccination Schedules**
- Species-specific vaccination frequencies
- Age-stratified protocols (youngstock vs. adults)
- Primary vaccination courses and booster schedules

### 3. **PCP-FMD Integration**
- Stage-specific vaccination coverage percentages
- Expert opinion-based outbreak response strategies
- Progressive control pathway alignment

### 4. **Geographic Analysis**
- Livestock population density mapping
- Administrative boundary analysis
- Emergency vaccination radius calculations (0-100km)

## Mathematical Models

### Population Prediction
```
P(t,c,s) = AutoEnsemble(X(c,s))
if P(t,c,s) ∈ [CI_L, CI_U], else P(t,c,s) = FAOSTAT(2023)
```

### Vaccination Requirements
```
Prophylactic: PVR = (YVR + AVR) × C(s,PCP)
Emergency: EVR = π × radius² × head_km²
```

Where:
- `P(t,c,s)`: Population of species `s` in country `c` at time `t`
- `YVR/AVR`: Young/Adult stock vaccination requirements
- `C(s,PCP)`: Coverage based on PCP stage

## Styling and Design

### FAO Official Color Palette
VADEMOS adheres to the FAO's professional color scheme to ensure visual consistency with FAO digital platforms:

| Color | Hex Code | Usage |
|-------|----------|-------|
| Dark Grey | `#545454` | Headers and text elements |
| Light Grey | `#F2F2F2` | Secondary backgrounds and subtle accents |
| White FAO | `#F7F8F9` | Main content backgrounds |
| UN Blue | `#5792c9` | Links and interactive elements |
| Caption Blue | `#1C4767` | Deeper blue accents and important headings |
| Sage Green | `#75AD82` | Secondary accent for buttons, borders, highlights |
| EuFMD Green | `#073f23` | Primary brand color (navbar, primary buttons) |

This cohesive color system maintains accessibility and readability standards across the application.

## File Structure

```
VADEMOS/
├── app.R                    # Main Shiny application
├── UIParts.R               # UI component definitions
├── renv.lock               # Package dependency lock file
├── .Renviron              # Environment variables (not tracked)
├── data/
│   ├── delphi-round1.csv   # Expert opinion data
│   ├── Outbreaks_Wahis.xlsx # Historical outbreak data
│   └── wahis_data.csv      # Processed WAHIS data
├── www/                    # Web assets
│   ├── about_part1.Rhtml   # About page content
│   ├── about_part2.Rhtml   # Parameters documentation
│   ├── about_part3.Rhtml   # Mathematical models
│   ├── styles.css          # Custom styling
│   └── *.png              # Images and logos
└── rsconnect/             # Deployment configurations
```

## Development

### Key Dependencies
- **Shiny**: Web application framework
- **Leaflet**: Interactive mapping
- **DT**: Data tables
- **plotly**: Interactive visualizations
- **sf/raster**: Spatial data processing
- **dplyr/tidyverse**: Data manipulation

### Database Integration
- MySQL/PostgreSQL support for logging
- Configurable through environment variables
- Session tracking and usage analytics

### Deployment
- Configured for shinyapps.io
- Multiple deployment targets supported
- Automated logging and monitoring

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 IGO license (CC BY-NC-SA 3.0 IGO). See [LICENSE](https://creativecommons.org/licenses/by-nc-sa/3.0/igo/) for details.

## Citation

If you use VADEMOS in your research or work, please cite:

```
EuFMD (2024). VADEMOS - Vaccine Demand Estimation Model for Foot-and-Mouth Disease. 
European Commission for the Control of Foot-and-Mouth Disease (EuFMD), 
Food and Agriculture Organization of the United Nations.
```

## Contact

For questions, support, or collaboration opportunities:
- **Email**: eufmd@fao.org
- **Website**: [EuFMD Official Site](https://www.fao.org/eufmd)
- **PCP Dashboard**: [PCP-FMD Dashboard](https://tableau.apps.fao.org/views/PCP/PCPDASHBOARD)

## Acknowledgments

This tool was developed by the EuFMD under its 2019-2023 workplan as part of Pillar III, Component 3.4. We acknowledge the contributions of:
- FAO for livestock population and density data
- WOAH for outbreak surveillance data
- Expert consultants for parameter validation
- The global FMD research community

---

*For detailed technical documentation and parameter distributions, please refer to the full model documentation (available for download from the application) or contact the development team.*
