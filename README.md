# Dayton Easement Tax Simulator (Shiny)

Interactive prototype: click parcels on an OSM basemap to put them in
conservation easement and watch Dayton's city tax capacity, rate, and burden
shift update in real time.

## First-time setup

```r
# From project root, with the full pipeline run and pipeline_checkpoint.RData saved:
setwd("dayton_easement_app")
source("data_prep.R")   # writes dayton_app_data.rds
```

## Run

```r
shiny::runApp("dayton_easement_app")
```

## Scope (v1)

- Clean slate baseline — no existing easements applied; user builds scenarios from zero
- Global slider (0–100%) — deduction rate applied to every selected parcel
- All parcels clickable (priority and non-priority)
- City tax rate recalculates; county and school rates held fixed
- Implied levy = baseline NTC × 2024 city rate, held fixed across scenarios

## Queued for later

- Layer existing easements as a pre-selected scenario
- Per-parcel slider (per-PID deduction rates)
- Multi-city version
- Deploy to shinyapps.io or shinylive (static)
