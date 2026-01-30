# VADEMOS Development Guide

VADEMOS is a decision-support Shiny application for estimating FMD vaccine demand at national/regional levels. The app combines livestock forecasting, GIS mapping, and PCP-FMD stage-based coverage calculations.

## Architecture

### Core Components
- **[app.R](../app.R)**: Single-file Shiny app containing complete UI and server logic (~1500 lines)
- **[UIParts.R](../UIParts.R)**: UI component factory - generates the multi-step tool interface with parameter selection panels
- **[www/](../www/)**: Static assets including help documentation (help1-5.Rhtml), about pages (about_part1-3.Rhtml), and styles
- **gadm_cache/**: Disk-cached administrative boundary files (RDS format) to avoid re-downloading GADM data

### Data Flow
1. User selects year/species/country → queries forecasted livestock populations
2. User configures vaccination parameters (schedules, youngstock proportions, PCP stages)
3. `get_results()` calculates vaccine requirements: `(youngstock * ys_schedule + adults * adult_schedule) * coverage`
4. Results displayed via interactive Leaflet maps with regional breakdowns and DT tables

## Development Workflow

### Running Locally
```r
# Restore package environment (required on first run)
renv::restore()

# Launch app
shiny::runApp()
```

Or use VS Code task: **"Run VADEMOS Shiny App"** which handles renv restore + runs on port 3838.

### Package Management
- **renv** locks all dependencies in `renv.lock` - always run `renv::restore()` after pulling changes
- Environment uses R 4.1+ with key packages: shiny, leaflet, sf, plotly, DT, geodata
- Two R version library paths exist: `renv/library/R-4.1/` and `R-4.4/` - check active version

### Configuration
- Create `.Renviron` with database credentials for logging:
  ```
  DB_HOST=...
  DB_USER=...
  DB_PASSWORD=...
  DB_NAME=...
  ```
- App detects deployment environment via `Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps"`
- Free-tier deployments use sequential processing (`plan(sequential)`); local dev uses 2 workers

## Key Patterns

### GADM Caching Strategy
The app implements aggressive disk caching for GADM administrative boundaries to work within shinyapps.io memory limits:

```r
get_gadm_data(country_code) # Checks gadm_cache/{CODE}_adm1.rds first
manage_cache_size(max_size_mb = 50) # Auto-cleans old cache files
```

**Never** load GADM data into memory cache - always use disk-only caching. Cache management runs on startup and enforces 30MB limit on server, 50MB locally.

**Critical memory optimizations:**
- Load sf geometries one country at a time (not all at once in `lapply`)
- Simplify geometries immediately after loading (`st_simplify` with dTolerance = 0.01)
- Use pre-allocated lists instead of iterative `rbind()` when building expanded_data
- Call `rm()` + `gc()` immediately after merging large datasets

### Reactive Calculation Chain
1. `forecasted_data()` - retrieves/computes livestock predictions
2. `pcp_data()` - loads PCP-FMD stage data for coverage defaults
3. `user_vaccine_schedule$selections` - reactive values storing UI inputs
4. `get_results()` - main calculation engine combining all inputs
5. `expanded_data` - reactive dataset joining forecast + vaccine requirements + geographic data

### UI Generation Pattern
Dynamic UI elements created via `renderUI()` with input IDs following convention:
- `vschedule_{species}_{age}` - vaccine schedules (e.g., `vschedule_lr_as`)
- `ysprop{species}` - youngstock proportions (e.g., `ysproplr`)
- `prophylactic_vc_{species}` - coverage percentages (e.g., `prophylactic_vc_lr`)

Species codes: `lr` (large ruminants: cattle/buffalo/camels), `sr` (small ruminants: goats/sheep), `p` (pigs)

### Multi-Step "About" Interface
Uses hidden input + conditionalPanel pattern:
```r
hidden(textInput("step", NULL, value = 1))
conditionalPanel(condition = "input.step == '1'", includeHTML("www/about_part1.Rhtml"))
```
Navigation buttons update `step` value to switch between summary/parameters/models sections.

## Species-Specific Logic
Vaccination calculations differ by species group:

```r
if (specie %in% c("Cattle", "Buffalo", "Camels")) {
  ys_prop <- youngstock_proportions$selections$lr  # Large ruminants
  ys_vac_schedule <- user_vaccine_schedule$selections$lr_ys
  adult_vac_schedule <- user_vaccine_schedule$selections$lr_as
}
```

Always check species group membership before accessing proportion/schedule reactive values. Coverage percentages also vary by species.

## Database Integration
MySQL connection configured but optional (app works without DB):
- `store_logs_in_db()` handles session logging via shinylogs
- Connection credentials from environment variables
- Logs stored via custom `store_custom()` function

## Deployment
Configured for shinyapps.io with multiple deployment targets in `rsconnect/shinyapps.io/eufmd/`:
- VADEMOS.dcf (production)
- Vademos_Staging.dcf (staging)

Memory optimization critical for free tier:
- No parallel processing on server
- Disk-only caching (no memory cache)
- Aggressive garbage collection after cache operations

## Common Modifications

### Adding New Species
1. Update species picker choices in [UIParts.R](../UIParts.R#L55)
2. Add species logic to calculation loops in `get_results()` around [app.R:Line 995](../app.R#L995)
3. Add coverage input UI for new species group
4. Update species grouping conditionals (currently lr/sr/p)

### Adjusting Vaccination Formulas
Main calculation in `get_results()`:
```r
youngstock_vaccine_requirement <- youngstock_value * ys_vac_schedule * coverage
adultstock_vaccine_requirement <- adultstock_value * adult_vac_schedule * coverage
total_vaccine_requirement <- youngstock_vaccine_requirement + adultstock_vaccine_requirement
```

Modify before line 1085 where results are formatted.

### Customizing Maps
Leaflet configuration around line 1250-1400 of [app.R](../app.R#L1250):
- Base tiles use OpenStreetMap
- Polygons colored via `colorNumeric()` palette
- Click events trigger detail popups with DT tables
- `sf_data_list` contains GADM boundaries joined with vaccine estimates

## Styling and Branding

### FAO Official Color Palette
The app uses FAO's professional color scheme defined in [www/styles.css](../www/styles.css):
- **Dark Grey** (`#545454`) - Headers and text elements
- **Light Grey** (`#F2F2F2`) - Secondary backgrounds and subtle accents
- **White FAO** (`#F7F8F9`) - Main content backgrounds for clean appearance
- **UN Blue** (`#5792c9`) - Links and interactive elements (UN affiliation)
- **Caption Blue** (`#1C4767`) - Deeper blue accents and important headings
- **Sage Green** (`#75AD82`) - Secondary accent for buttons, borders, highlights
- **EuFMD Green** (`#073f23`) - Primary brand color used in navbar and buttons

When modifying UI elements, maintain these colors for visual consistency with FAO digital platforms and accessibility standards.
