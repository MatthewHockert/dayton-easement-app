# Dayton Conservation Easement Tax Simulator (Shiny)
#
# Run:  shiny::runApp("dayton_easement_app")
#
# Prereq: dayton_app_data.rds in this folder (see data_prep.R).

library(shiny)
library(leaflet)
library(data.table)
library(sf)
library(scales)

app_data <- readRDS("dayton_app_data.rds")

parcels      <- app_data$parcels        # subrecord-level data.table
geom         <- app_data$geom           # sf, one row per PID, with tax_capacity/zone
zoning       <- app_data$zoning
parks        <- app_data$parks
adj          <- app_data$adj            # named list: PID -> neighboring PIDs
parcels_touching_parks     <- app_data$parcels_touching_parks
parcels_touching_easements <- app_data$parcels_touching_easements
if (is.null(parcels_touching_easements)) parcels_touching_easements <- character(0)
existing_easement_pids     <- app_data$existing_easement_pids
if (is.null(existing_easement_pids)) existing_easement_pids <- character(0)
# Anchors = parks OR existing easements (both are "already protected" for corridor scoring)
anchor_pids <- unique(c(parcels_touching_parks, parcels_touching_easements))

# Lookup: PID -> (acres, priority_score) for fast score calc
pid_lookup <- as.data.table(sf::st_drop_geometry(geom))[, .(PID, acres, priority_score)]
setkey(pid_lookup, PID)

# Natural Systems Score denominators
ZONATION_7_TOTAL_N       <- app_data$zonation_7_total_n
NTC_PER_ACRE             <- app_data$ntc_per_acre
dev_candidates           <- app_data$dev_candidates
if (is.null(dev_candidates)) dev_candidates <- data.table::data.table()
BASELINE_AVG_DIST_MILES  <- app_data$baseline_avg_dist_miles
protected_base_m         <- app_data$protected_base_m       # sf in UTM15N
parcel_cent_m            <- app_data$parcel_cent_m          # centroids in UTM15N
baseline_protected_pids  <- app_data$baseline_protected_pids

# Projected parcel geom for on-the-fly distance calcs
geom_m <- sf::st_transform(geom, 26915)

baseline_ntc  <- app_data$baseline_ntc
baseline_levy <- app_data$baseline_levy
baseline_rate <- app_data$baseline_rate
total_city_acres <- parcels[, sum(ACREAGE, na.rm = TRUE), by = PID][, sum(V1, na.rm = TRUE)]

zone_colors <- c(
  "Agricultural"                  = "#F1C40F",   # warm yellow (productive ag)
  "Agricultural (Transitional)"   = "#FCE96A",   # pale yellow
  "Residential (High Density)"    = "#08306B",   # dark navy blue
  "Residential (Medium Density)"  = "#2171B5",   # strong mid blue
  "Residential (Low Density)"     = "#9ECAE1",   # clear light blue
  "Mixed Use"                     = "#C44536",   # brick red (moved off gold)
  "Commercial"                    = "#8E44AD",
  "Industrial"                    = "#5A6268",
  "Parks / Recreation"            = "#2D6A4F",   # green (only green left)
  "Essential Services"            = "#ADB5BD",
  "Other"                         = "#CCCCCC"
)
zone_pal <- colorFactor(unname(zone_colors), levels = names(zone_colors),
                        ordered = TRUE, na.color = "#CCCCCC")

# Display labels: show the unit/acre range instead of the broad density word.
# Internal `display_category` values are kept as logic/color keys; only the
# user-facing text is relabeled. Ranges are the project's generic densities
# (city_report.qmd) — confirm against Dayton's zoning ordinance before relying
# on the exact thresholds.
density_range_labels <- c(
  "Residential (Low Density)"    = "Residential (1)",
  "Residential (Medium Density)" = "Residential (2-6)",
  "Residential (High Density)"   = "Residential (>6)"
)
relabel_density <- function(x) {
  out <- unname(density_range_labels[as.character(x)])
  ifelse(is.na(out), as.character(x), out)
}
# Carry a display-only label alongside the logic key
zoning$disp_label <- relabel_density(zoning$display_category)
geom$disp_label   <- relabel_density(geom$display_category)

# Zonation palette: 0 = no data (grey), 1-7 = yellow → dark green ramp
zonation_colors <- c(
  "0" = "#EEEEEE",   # no data
  "1" = "#FFFFCC",
  "2" = "#D9F0A3",
  "3" = "#ADDD8E",
  "4" = "#78C679",
  "5" = "#41AB5D",
  "6" = "#238443",
  "7" = "#005A32"
)
zonation_pal <- colorFactor(unname(zonation_colors), levels = names(zonation_colors),
                            ordered = TRUE, na.color = "#EEEEEE")

# ---------------- UI ----------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: -apple-system, sans-serif; }
    .metric { padding: 6px 0; border-bottom: 1px solid #eee; }
    .metric .label { font-size: 12px; color: #666; text-transform: uppercase; }
    .metric .value { font-size: 18px; font-weight: 600; }
    .metric .delta.pos { color: #B2182B; }
    .metric .delta.neg { color: #2166AC; }
    .sidebar { padding: 15px; background: #fafafa; height: 100vh;
               overflow-y: auto; border-left: 1px solid #ddd; }
  "))),

  titlePanel("Dayton Conservation Easement Tax Simulator"),

  fluidRow(
    column(8, leafletOutput("map", height = "88vh")),
    column(4, div(class = "sidebar",
      div(style = "background:#1B4332; color:white; padding:12px 14px; border-radius:6px; margin-bottom:14px;",
          div(style = "font-size:13px; opacity:0.85; letter-spacing:0.5px; margin-bottom:12px;",
              "NATURAL SYSTEMS SCORES"),

          # --- Score 1: High Priority Parcels ---
          div(style = "margin-bottom:12px;",
            div(style = "display:flex; justify-content:space-between; align-items:baseline;",
                tags$span(style = "font-size:13px;", "High Priority Parcels"),
                tags$span(style = "font-size:18px; font-weight:700;",
                          textOutput("s1_value", inline = TRUE),
                          tags$span("/100", style = "font-size:12px; opacity:0.7;"))),
            div(style = "margin-top:4px; background:rgba(255,255,255,0.15); height:6px; border-radius:3px; overflow:hidden;",
                div(class = "s1bar", style = "height:100%; background:#52B788; width:0%; transition:width 0.3s;")),
            div(style = "font-size:10px; opacity:0.75; margin-top:3px;",
                textOutput("s1_detail", inline = TRUE))),

          # --- Score 2: Connectedness ---
          div(style = "margin-bottom:12px;",
            div(style = "display:flex; justify-content:space-between; align-items:baseline;",
                tags$span(style = "font-size:13px;", "Connectedness"),
                tags$span(style = "font-size:18px; font-weight:700;",
                          textOutput("s2_value", inline = TRUE),
                          tags$span("/100", style = "font-size:12px; opacity:0.7;"))),
            div(style = "margin-top:4px; background:rgba(255,255,255,0.15); height:6px; border-radius:3px; overflow:hidden;",
                div(class = "s2bar", style = "height:100%; background:#52B788; width:0%; transition:width 0.3s;")),
            div(style = "font-size:10px; opacity:0.75; margin-top:3px;",
                textOutput("s2_detail", inline = TRUE))),

          # --- Score 3: Distance Reduction ---
          div(style = "margin-bottom:6px;",
            div(style = "display:flex; justify-content:space-between; align-items:baseline;",
                tags$span(style = "font-size:13px;", "Distance to Protected"),
                tags$span(style = "font-size:18px; font-weight:700;",
                          textOutput("s3_value", inline = TRUE),
                          tags$span("/100", style = "font-size:12px; opacity:0.7;"))),
            div(style = "margin-top:4px; background:rgba(255,255,255,0.15); height:6px; border-radius:3px; overflow:hidden;",
                div(class = "s3bar", style = "height:100%; background:#52B788; width:0%; transition:width 0.3s;")),
            div(style = "font-size:10px; opacity:0.75; margin-top:3px;",
                textOutput("s3_detail", inline = TRUE)))
      ),
      tags$script(HTML(
        "Shiny.addCustomMessageHandler('scorebars', function(pct) {
           document.querySelector('.s1bar').style.width = pct.s1 + '%';
           document.querySelector('.s2bar').style.width = pct.s2 + '%';
           document.querySelector('.s3bar').style.width = pct.s3 + '%';
         });"
      )),
      h4("City fiscal dashboard"),
      div(class = "metric",
          div(class = "label", "2024 City Levy (fixed)"),
          div(class = "value", textOutput("levy", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "Total City Tax Capacity"),
          div(class = "value", textOutput("ntc", inline = TRUE)),
          div(class = "delta", textOutput("ntc_delta", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "City Tax Rate"),
          div(class = "value", textOutput("rate", inline = TRUE)),
          div(class = "delta", textOutput("rate_delta", inline = TRUE))),
      hr(),
      h4("Easement selection"),
      div(class = "metric",
          div(class = "label", "Parcels selected"),
          div(class = "value", textOutput("n_sel", inline = TRUE))),
      sliderInput("deduction", "Deduction rate (applies to all selected)",
                  min = 0, max = 100, value = 100, step = 5, post = "%"),
      div(class = "metric",
          div(class = "label", "Easement acres"),
          div(class = "value", textOutput("off_ea_acres", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "Lost tax capacity"),
          div(class = "value", textOutput("off_lost_ntc", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "Lost capacity per acre"),
          div(class = "value", textOutput("off_ea_per_ac", inline = TRUE))),
      hr(),
      h4("Densification offset"),
      div(style = "font-size:10px; color:#888; text-transform:uppercase; margin-bottom:4px;",
          "Acres needed to offset"),
      uiOutput("offset_rows"),
      tags$hr(style = "margin:8px 0; border-top:1px dashed #ccc;"),
      div(style = "font-size:10px; color:#888; text-transform:uppercase; margin-bottom:4px;",
          "Suggested development (black layer)"),
      div(class = "metric",
          div(class = "label", "Parcels to develop"),
          div(class = "value", textOutput("sug_n", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "Total acres"),
          div(class = "value", textOutput("sug_ac", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "Tax capacity gained"),
          div(class = "value", textOutput("sug_gain", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "Net vs. lost capacity"),
          div(class = "value", textOutput("sug_net", inline = TRUE)),
          div(class = "delta", textOutput("sug_coverage", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "Within current zoning"),
          div(class = "value", textOutput("sug_infill_count", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "Would need rezone"),
          div(class = "value", textOutput("sug_rezone_count", inline = TRUE))),
      uiOutput("sug_by_type"),
      p(style = "font-size:10px; color:#888; margin-top:4px;",
        "Optimization priorities (in order): (1) vacant infill lots first, ",
        "(2) highest $/acre gain, (3) minimize land used, ",
        "(4) prefer parcels near existing residential. ",
        "Toggle 'Suggested Development' in the layer control to view."),
      hr(),
      h4("Tax increase on remaining parcels"),
      p(style = "font-size:12px; color:#666; margin-top:-8px;",
        "The city still collects the same levy, so easement-eligible capacity ",
        "is picked up by non-easement parcels."),
      div(class = "metric",
          div(class = "label", "Total tax shifted to remaining parcels"),
          div(class = "value", textOutput("total_shift", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "Average tax increase (per remaining parcel)"),
          div(class = "value", textOutput("avg_shift", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "Average % tax increase (per remaining parcel)"),
          div(class = "value", textOutput("avg_pct_shift", inline = TRUE))),
      br(),
      actionButton("reset", "Reset", width = "100%",
                   icon = icon("rotate-left")),
      br(), br(),
      actionButton("show_methodology", "View Methodology",
                   width = "100%", icon = icon("info-circle"),
                   style = "background:#fff; border:1px solid #ccc;"),
      br(), br(),
      p(style = "font-size:11px; color:#888;",
        "Click a parcel to add it to easement. Click again to remove.")
    ))
  )
)

# ---------------- Server ------------------------------------------------
server <- function(input, output, session) {

  selected <- reactiveVal(character(0))   # selected PIDs

  # ---- base map (rendered once) ----------------------------------------
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles(group = "OpenStreetMap") |>
      addPolygons(data = zoning,
                  fillColor = ~zone_pal(display_category),
                  fillOpacity = 0.80, color = "#444", weight = 0.4,
                  group = "Zoning",
                  label = ~paste0(zone_code, " — ", zone_name,
                                  " (", disp_label, ")")) |>
      addPolygons(data = geom,
                  layerId = ~PID,
                  fillColor = "#ffffff", fillOpacity = 0,
                  color = "#00000010", weight = 0.2,
                  highlightOptions = highlightOptions(
                    weight = 2.5, color = "#000", fillOpacity = 0,
                    bringToFront = TRUE),
                  group = "Parcels",
                  label = ~lapply(paste0(
                    "<b>PID ", PID, "</b>",
                    ifelse(in_sim, "", " <i>(exempt)</i>"), "<br>",
                    "Zone: ", ifelse(is.na(zone_code), "—",
                                     paste0(zone_code, " (", disp_label, ")")), "<br>",
                    "Zonation priority: ", priority_score, "/7<br>",
                    "Tax capacity: ",
                    ifelse(in_sim,
                           paste0("$", formatC(tax_capacity, format = "d", big.mark = ",")),
                           "n/a"), "<br>",
                    "Acres: ",
                    ifelse(is.na(acres), "n/a",
                           formatC(acres, digits = 2, format = "f")), "<br>",
                    "Type: ", prop_type
                  ), htmltools::HTML)) |>
      addPolygons(data = parks,
                  fill = FALSE,
                  color = "#1B4332", weight = 3, opacity = 0.9,
                  group = "Protected Land",
                  options = pathOptions(interactive = FALSE)) |>
      addPolygons(data = geom[geom$is_existing_easement, ],
                  fillColor = "#14B8A6", fillOpacity = 0.55,
                  color = "#0F766E", weight = 2, opacity = 0.9,
                  group = "Existing Easements",
                  options = pathOptions(interactive = FALSE),
                  label = ~lapply(paste0(
                    "<b>Existing easement</b><br>",
                    "PID: ", PID, "<br>",
                    "Holder: ", ifelse(is.na(easement_holder), "—", easement_holder), "<br>",
                    "Recorded acres: ",
                    ifelse(is.na(recorded_acres), "—",
                           formatC(recorded_acres, digits = 2, format = "f"))
                  ), htmltools::HTML)) |>
      addPolygons(data = geom,
                  fillColor = ~zonation_pal(as.character(priority_score)),
                  fillOpacity = 0.75, stroke = FALSE,
                  group = "Zonation Priority",
                  options = pathOptions(interactive = FALSE),
                  label = ~paste0("Zonation priority: ", priority_score, "/7")) |>
      addLayersControl(
        overlayGroups = c("Zoning", "Zonation Priority", "Parcels",
                          "Protected Land", "Existing Easements",
                          "Suggested Development"),
        options = layersControlOptions(collapsed = FALSE)) |>
      hideGroup("Zonation Priority") |>
      hideGroup("Suggested Development") |>
      addLegend("bottomleft", colors = unname(zone_colors),
                labels = relabel_density(names(zone_colors)), title = "Zoning",
                opacity = 0.8, group = "Zoning") |>
      addLegend("bottomleft", pal = zonation_pal,
                values = names(zonation_colors),
                title = "Zonation priority<br><small>0 = no data, 7 = highest</small>",
                opacity = 0.8, group = "Zonation Priority") |>
      fitBounds(
        lng1 = min(st_bbox(geom)[["xmin"]], st_bbox(zoning)[["xmin"]]),
        lat1 = min(st_bbox(geom)[["ymin"]], st_bbox(zoning)[["ymin"]]),
        lng2 = max(st_bbox(geom)[["xmax"]], st_bbox(zoning)[["xmax"]]),
        lat2 = max(st_bbox(geom)[["ymax"]], st_bbox(zoning)[["ymax"]]))
  })

  # ---- parcel click toggles selection ----------------------------------
  # Only taxable parcels (in_sim) are eligible for easement — exempt parcels
  # (parks, municipal, schools) have no tax capacity to deduct.
  eligible_pids <- unique(geom$PID[geom$in_sim])
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    pid <- click$id
    if (is.null(pid)) return()
    if (!pid %in% eligible_pids) {
      showNotification("Exempt parcels (parks, municipal, schools) can't be placed in easement — they have no tax capacity.",
                       type = "warning", duration = 3)
      return()
    }
    cur <- selected()
    if (pid %in% cur) selected(setdiff(cur, pid)) else selected(c(cur, pid))
  })

  observeEvent(input$show_methodology, {
    showModal(modalDialog(
      title = "Methodology & Definitions",
      size = "l", easyClose = TRUE,
      tags$h4("Natural Systems Scores"),
      tags$p(tags$b("1. High Priority Parcels"), " — fraction of Dayton's zonation-7 (top ~1% of conservation priority) parcels placed in easement."),
      tags$code("score = 100 × (# zonation-7 parcels selected) / (# zonation-7 parcels in Dayton)"),
      tags$p(tags$b("2. Connectedness"), " — fraction of your selections adjacent to at least one other selection. Parks / existing easements do not count here (they're scored in #3). Requires ≥ 2 selections."),
      tags$code("score = 100 × (# selections touching another selection) / (# selected)"),
      tags$p(tags$b("3. Distance to Protected"), " — reduction in the average distance from unprotected parcel centroids to the nearest protected region (parks + existing easements + your selections, dissolved)."),
      tags$code("score = 100 × (1 − current_avg_dist / baseline_avg_dist)"),

      tags$hr(),
      tags$h4("City Fiscal Dashboard"),
      tags$p(tags$b("Baseline"), " reflects today's state — existing HC conservation easements are already applied (their priority subrecords deducted)."),
      tags$p(tags$b("City Levy"), " is held fixed (",
             tags$code("baseline_ntc × city_rate"), "). The city still needs the same dollars whether easements exist or not."),
      tags$p(tags$b("Total City Tax Capacity"), " — sum of LOCAL_NET_TAX_CAPACITY across all Dayton parcels, with easement deductions applied (100% on existing easements; slider% on user-selected)."),
      tags$p(tags$b("City Tax Rate"), " is the rate required to raise the fixed levy against the (reduced) tax capacity:"),
      tags$code("rate_new = city_levy / ntc_new"),
      tags$p("Per-parcel taxes follow the proportional-rate method:"),
      tags$code("tax_after = NTC_eff × baseline_rate × (rate_new / baseline_rate)"),

      tags$hr(),
      tags$h4("Densification Offset"),
      tags$p(tags$b("Lost tax capacity"), " — NTC removed by your selections: ",
             tags$code("ntc_removed = baseline_ntc − ntc_new")),
      tags$p(tags$b("Lost capacity per acre"), " — average $ of tax capacity lost per easement acre in your scenario:"),
      tags$code("ea_per_ac = ntc_removed / easement_acres"),
      tags$p("This is specific to your selection mix. Ag land is ~$150–400/ac; residential or commercial parcels drive it much higher."),
      tags$p(tags$b("Acres needed by type"), " — how many acres of each development type would have to exist at Dayton's current average $/acre for that type to recoup the lost capacity:"),
      tags$code("offset_acres[type] = ntc_removed / ntc_per_ac[type]"),
      tags$p(tags$b("Equivalency ratio"), " — how many easement acres one acre of that development type offsets:"),
      tags$code("ratio = ntc_per_ac[type] / ea_per_ac"),
      tags$p("\"1 ac = 22 ea\" means 1 acre of that use replaces 22 acres of your easement in tax capacity."),

      tags$hr(),
      tags$h4("Tax Increase on Remaining Parcels"),
      tags$p("Since the levy is fixed, capacity removed by easements is picked up by non-easement parcels via the rate increase. Dashboard shows the aggregate and average increase across those parcels. Note: this section reflects easements only; it does not assume the suggested development has been built."),

      tags$hr(),
      tags$h4("Suggested Development (Black Layer)"),
      tags$p("A greedy optimizer proposes parcels to upzone/build on that would offset the tax capacity your easements removed. Toggle the layer via the map's layer control."),
      tags$p(tags$b("Candidates"), " — taxable parcels that are either (a) in Agricultural / Agricultural (Transitional) / Other zoning, or (b) have a vacant property type, and are not already protected or user-selected."),
      tags$p(tags$b("Filters applied to candidates:")),
      tags$ul(
        tags$li(tags$b("Min area ≥ 1 acre"), " — drops sliver remnants."),
        tags$li(tags$b("Polsby-Popper compactness ≥ 0.25"), " — drops long thin shapes (railroad / utility ROWs, drainage and stormwater strips). Formula: 4π × area / perimeter², where 0 = infinitely thin, 1 = perfect circle."),
        tags$li(tags$b("< 50% wetland overlap"), " — drops parcels protected from construction by regulation (NWI)."),
        tags$li(tags$b("Zonation score < 5"), " — excludes high conservation-priority land (top ~25% by Zonation) from development candidates. Vacant wooded/natural areas with high conservation value shouldn't be suggested for paving, even if zoning allows it."),
        tags$li(tags$b("Current NTC/acre ≥ $50"), " — drops absurdly under-assessed parcels (holding ponds, drainage easements, sewer tracts) that are often valued at ~$1 even when zoning allows development."),
        tags$li(tags$b("Landlocked parcels are NOT filtered"), " yet — would require a road-network dataset.")
      ),
      tags$p(tags$b("Target type per candidate"), " — the density category of the parcel's nearest Residential-Medium or Residential-High neighbor."),
      tags$p(tags$b("Score:")),
      tags$code("base_score = gain_per_acre / (1 + distance_to_residential_km)"),
      tags$br(),
      tags$code("score = base_score × (2 if vacant-infill, else 1)"),
      tags$p("The 2× multiplier prioritizes empty lots already inside residential neighborhoods over greenfield ag conversion."),
      tags$p(tags$b("Greedy pick"), " — parcels are sorted by score descending; selected one at a time until cumulative NTC gain ≥ lost tax capacity. The last pick may overshoot since parcels can't be split; see the 'Net vs. lost capacity' row for the coverage ratio."),
      tags$p(tags$b("Within zoning vs. needs rezone"), " — each suggested parcel is flagged based on whether its current zoning is already Residential (any density). Green-flagged picks are vacant-lot infill; red-flagged picks would require city council rezone approval."),

      tags$hr(),
      tags$h4("Data Sources"),
      tags$ul(
        tags$li(tags$b("Assessor extract (2024):"), " parcels, tax capacity, subrecords, property types, priority subrecord flags."),
        tags$li(tags$b("Met Council open parcels GDB:"), " tax-exempt parcels (parks, municipal, schools)."),
        tags$li(tags$b("HC established easements:"), " closed conservation easements (MCWD filtered out, multi-PID easements split proportionally)."),
        tags$li(tags$b("Zonation priority rankings:"), " kristine_parcel_coded_zonation; score 1 = 0–25th pct, 7 = 99–100th pct."),
        tags$li(tags$b("Dayton zoning shapefile + density lookup:"), " DaytonZoning/ + data/dayton_zoning_density.csv."),
        tags$li(tags$b("2024 tax rates:"), " Conservation Project Hennepin Rates 2024.csv."),
        tags$li(tags$b("National Wetland Inventory (NWI):"), " hennepin_wetland_merged.shp — used to filter out undevelopable wetland parcels from the suggested-development candidate pool.")
      ),

      footer = modalButton("Close")
    ))
  })

  observeEvent(input$reset, {
    selected(character(0))
    updateSliderInput(session, "deduction", value = 100)
    bb <- sf::st_bbox(geom)
    leafletProxy("map") |>
      flyToBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
                  lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
  })

  # ---- redraw selection highlight --------------------------------------
  observe({
    sel <- selected()
    proxy <- leafletProxy("map") |> clearGroup("sel")
    if (length(sel) > 0) {
      sel_geom <- geom[geom$PID %in% sel, ]
      proxy |> addPolygons(data = sel_geom,
                           group = "sel",
                           fillColor = "#FF006E", fillOpacity = 0.70,
                           color = "#6A0033", weight = 2,
                           options = pathOptions(interactive = FALSE))
    }
  })

  # ---- redraw suggested development layer ------------------------------
  observe({
    sug <- scenario()$suggested_pids
    proxy <- leafletProxy("map") |> clearGroup("Suggested Development")
    if (length(sug) > 0) {
      sug_geom <- geom[geom$PID %in% sug, ]
      # Attach target_type + gain details from dev_candidates
      dc_lookup <- dev_candidates[, .(PID, target_type, target_per_ac, gain_per_ac,
                                       ntc_gain, acres, is_vacant, within_zoning, current_zone)]
      sug_geom <- merge(sug_geom, dc_lookup, by = "PID", all.x = TRUE,
                        suffixes = c("", "_dc"))
      proxy |> addPolygons(data = sug_geom,
                           group = "Suggested Development",
                           fillColor = "#000000", fillOpacity = 0.85,
                           color = "#00E5FF", weight = 3.5,
                           dashArray = "4,4",
                           options = pathOptions(interactive = FALSE),
                           label = ~lapply(paste0(
                             "<b>Suggested for development</b>",
                             ifelse(is_vacant, " <i>(vacant infill)</i>", ""), "<br>",
                             "PID: ", PID, "<br>",
                             "Current zone: ", current_zone, "<br>",
                             "Develop as: <b>", target_type, "</b><br>",
                             ifelse(within_zoning,
                                    "<b>✓ Within current zoning</b>",
                                    "<b>⚠ Would need rezone</b>"),
                             "<br>",
                             "Parcel: ",
                             formatC(acres_dc, digits = 2, format = "f"), " ac<br>",
                             "Target $/ac: $",
                             formatC(target_per_ac, format = "d", big.mark = ","), "<br>",
                             "NTC gain if developed: $",
                             formatC(ntc_gain, format = "d", big.mark = ",")
                           ), htmltools::HTML))
    }
  })

  # ---- Core reactive math ----------------------------------------------
  # Returns a list: ntc_new, rate_new, delta_levy, sel_acres, n_sel,
  #                 avg_shift_dollar, avg_shift_pct
  scenario <- reactive({
    sel <- selected()
    d <- input$deduction / 100

    p <- copy(parcels)
    p[, is_sel := PID %in% sel]
    # Per-subrecord effective NTC:
    #   - existing-easement priority subrecords: already protected (100% off) UNLESS
    #     the user has selected that PID, in which case the slider overrides
    #   - user-selected parcels: slider determines deduction
    #   - all others: full LNTC
    p[, is_existing_ea := PID %in% existing_easement_pids]
    p[, base_ntc_today := fifelse(
         is_existing_ea & is_priority_subrecord == 1, 0,
         LOCAL_NET_TAX_CAPACITY)]
    p[, ntc_eff := base_ntc_today]
    p[is_sel == TRUE, ntc_eff := LOCAL_NET_TAX_CAPACITY * (1 - d)]

    # "Baseline tax" for the shift calculation = today's state per subrecord
    p[, base_tax_today := base_ntc_today * baseline_rate]

    ntc_new  <- p[, sum(ntc_eff, na.rm = TRUE)]
    rate_new <- baseline_levy / ntc_new

    # proportional rate approach (matches total_tax_functions.R)
    p[, tax_new := ntc_eff * baseline_rate * (rate_new / baseline_rate)]
    p[, delta  := tax_new - base_tax_today]

    if (length(sel) == 0) {
      # Short-circuit: no easements selected → exactly zero shift
      # (avoids floating-point drift from the rate-proportion math)
      total_shift      <- 0
      avg_shift_dollar <- 0
      avg_shift_pct    <- 0
    } else {
      non_esmt <- p[is_sel == FALSE, .(delta = sum(delta, na.rm = TRUE),
                                       base  = sum(base_tax_today, na.rm = TRUE)),
                    by = PID]
      total_shift <- sum(non_esmt$delta, na.rm = TRUE)
      avg_shift_dollar <- if (nrow(non_esmt) == 0) 0 else mean(non_esmt$delta, na.rm = TRUE)
      avg_shift_pct    <- if (nrow(non_esmt) == 0) 0 else
        mean(non_esmt[base > 0, delta / base * 100], na.rm = TRUE)
    }

    sel_acres <- if (length(sel) == 0) 0 else
      parcels[PID %in% sel, sum(ACREAGE, na.rm = TRUE), by = PID][, sum(V1, na.rm = TRUE)]

    # ---- Natural Systems Score: three independent 0-100 subscores ----------
    s <- if (length(sel) > 0) pid_lookup[PID %in% sel] else pid_lookup[FALSE]

    # Score 1: # of zonation-7 parcels selected / total # of zonation-7 parcels
    s1_n     <- if (length(sel) > 0) s[priority_score == 7, .N] else 0
    s1_total <- ZONATION_7_TOTAL_N
    s1_pct   <- if (s1_total > 0) 100 * min(1, s1_n / s1_total) else 0
    s1_detail <- sprintf("%d of %d top-priority (zonation=7) parcels in easement",
                         s1_n, s1_total)

    # Score 2: fraction of selections adjacent to ANOTHER selection
    # (parks/easements are handled separately in Score 3)
    if (length(sel) < 2) {
      s2_pct <- 0
      s2_detail <- if (length(sel) == 0) "no parcels selected yet"
                   else "select a 2nd parcel to start a cluster"
    } else {
      connected_flags <- vapply(sel, function(pid) {
        length(intersect(adj[[as.character(pid)]], setdiff(sel, pid))) > 0
      }, logical(1))
      n_conn <- sum(connected_flags)
      s2_pct <- 100 * n_conn / length(sel)
      s2_detail <- sprintf("%d of %d selections touching another selection",
                           n_conn, length(sel))
    }

    # Score 3: reduction in avg distance from unprotected parcels to nearest
    # protected region. Monotone — adding any easement brings its parcel to 0
    # and may pull neighbors' nearest-distances down.
    if (length(sel) == 0 || is.na(BASELINE_AVG_DIST_MILES) ||
        BASELINE_AVG_DIST_MILES == 0) {
      current_avg_dist_miles <- BASELINE_AVG_DIST_MILES
      s3_pct <- 0
    } else {
      sel_geom  <- sf::st_geometry(geom_m[geom_m$PID %in% sel, ])
      base_geom <- sf::st_geometry(protected_base_m)
      all_regions <- sf::st_cast(sf::st_union(c(base_geom, sel_geom)), "POLYGON")

      now_protected_pids <- unique(c(baseline_protected_pids, sel))
      unprotected_idx    <- which(!geom$PID %in% now_protected_pids)
      cents              <- parcel_cent_m[unprotected_idx, ]

      if (length(cents) > 0 && length(all_regions) > 0) {
        D <- sf::st_distance(cents, all_regions)
        min_dists_m <- apply(as.matrix(D), 1, min)
        current_avg_dist_miles <- mean(min_dists_m) / 1609.344
        s3_pct <- 100 * max(0, min(1,
                    1 - current_avg_dist_miles / BASELINE_AVG_DIST_MILES))
      } else {
        current_avg_dist_miles <- 0
        s3_pct <- 100
      }
    }
    s3_detail <- if (is.na(BASELINE_AVG_DIST_MILES)) "n/a" else {
      cur <- ifelse(is.na(current_avg_dist_miles),
                    BASELINE_AVG_DIST_MILES, current_avg_dist_miles)
      drop <- BASELINE_AVG_DIST_MILES - cur
      sprintf("Avg %.3f mi to protected  |  baseline %.3f mi  |  −%.3f mi",
              cur, BASELINE_AVG_DIST_MILES, drop)
    }

    # Offset math: how much new development of each type would recoup the
    # NTC removed by user's selections (ntc baseline is "today" state).
    ntc_removed  <- max(0, baseline_ntc - ntc_new)
    ea_acres     <- sel_acres
    ea_per_ac    <- if (ea_acres > 0) ntc_removed / ea_acres else NA_real_
    offset_acres <- if (ntc_removed == 0) rep(0, length(NTC_PER_ACRE)) else
      ntc_removed / NTC_PER_ACRE
    names(offset_acres) <- names(NTC_PER_ACRE)
    # Ratio: "1 acre of type X offsets how many easement acres"
    offset_ratio <- if (is.na(ea_per_ac) || ea_per_ac == 0)
      rep(NA_real_, length(NTC_PER_ACRE)) else NTC_PER_ACRE / ea_per_ac
    names(offset_ratio) <- names(NTC_PER_ACRE)

    # Suggested development: greedy over candidates (sorted by score in prep)
    # until cumulative NTC gain ≥ ntc_removed. Also exclude user-selected.
    if (ntc_removed == 0 || nrow(dev_candidates) == 0) {
      suggested_pids <- character(0)
      sug_acres      <- 0
      sug_gain       <- 0
    } else {
      cd <- dev_candidates[!PID %in% sel]
      cd[, cum_gain := cumsum(ntc_gain)]
      n_needed <- which(cd$cum_gain >= ntc_removed)[1]
      if (is.na(n_needed)) n_needed <- nrow(cd)  # not enough capacity
      suggested_pids <- cd$PID[seq_len(n_needed)]
      sug_acres      <- sum(cd$acres[seq_len(n_needed)])
      sug_gain       <- cd$cum_gain[n_needed]
    }

    list(ntc_new = ntc_new, rate_new = rate_new,
         sel_acres = sel_acres, n_sel = length(sel),
         total_shift = total_shift,
         avg_shift_dollar = avg_shift_dollar,
         avg_shift_pct = avg_shift_pct,
         s1_pct = s1_pct, s1_detail = s1_detail,
         s2_pct = s2_pct, s2_detail = s2_detail,
         s3_pct = s3_pct, s3_detail = s3_detail,
         offset_acres = offset_acres,
         offset_ratio = offset_ratio,
         ntc_removed  = ntc_removed,
         ea_acres     = ea_acres,
         ea_per_ac    = ea_per_ac,
         suggested_pids = suggested_pids,
         sug_acres      = sug_acres,
         sug_gain       = sug_gain,
         sug_breakdown  = if (length(suggested_pids) > 0)
             dev_candidates[PID %in% suggested_pids,
                           .(parcels = .N, acres = sum(acres)),
                           by = target_type]
           else data.table::data.table(),
         sug_n_rezone = if (length(suggested_pids) > 0)
             dev_candidates[PID %in% suggested_pids, sum(!within_zoning)]
           else 0L,
         sug_n_infill = if (length(suggested_pids) > 0)
             dev_candidates[PID %in% suggested_pids, sum(within_zoning)]
           else 0L)
  })

  # ---- outputs ---------------------------------------------------------
  output$levy <- renderText(dollar(baseline_levy))
  output$ntc  <- renderText(dollar(scenario()$ntc_new))
  output$ntc_delta <- renderText({
    pct <- (scenario()$ntc_new / baseline_ntc) - 1
    sprintf("Baseline: %s  |  %s%s",
            dollar(baseline_ntc),
            ifelse(pct >= 0, "+", ""),
            percent(pct, accuracy = 0.01))
  })
  output$rate <- renderText(percent(scenario()$rate_new, accuracy = 0.001))
  output$rate_delta <- renderText({
    pct <- (scenario()$rate_new / baseline_rate) - 1
    sprintf("Baseline: %s  |  Bills %s%s",
            percent(baseline_rate, accuracy = 0.001),
            ifelse(pct >= 0, "up ", "down "),
            percent(abs(pct), accuracy = 0.01))
  })
  output$n_sel  <- renderText(scenario()$n_sel)
  output$acres  <- renderText(comma(scenario()$sel_acres, accuracy = 0.1))
  output$acres_pct <- renderText(
    sprintf("%s of city land", percent(scenario()$sel_acres / total_city_acres,
                                       accuracy = 0.01)))
  output$s1_value  <- renderText(round(scenario()$s1_pct))
  output$s1_detail <- renderText(scenario()$s1_detail)
  output$s2_value  <- renderText(round(scenario()$s2_pct))
  output$s2_detail <- renderText(scenario()$s2_detail)
  output$s3_value  <- renderText(round(scenario()$s3_pct))
  output$s3_detail <- renderText(scenario()$s3_detail)
  observe({
    session$sendCustomMessage("scorebars",
      list(s1 = scenario()$s1_pct,
           s2 = scenario()$s2_pct,
           s3 = scenario()$s3_pct))
  })

  fmt_acres <- function(x) {
    if (is.na(x) || is.infinite(x) || x == 0) return("—")
    paste0(formatC(x, digits = 1, format = "f"), " ac")
  }
  fmt_ratio <- function(r) {
    if (is.na(r) || is.infinite(r) || r == 0) return("—")
    sprintf("1 ac = %.1f ea", r)
  }

  output$off_ea_acres  <- renderText({
    x <- scenario()$ea_acres
    if (is.null(x) || x == 0) "—" else
      paste0(formatC(x, digits = 1, format = "f"), " ac")
  })
  output$off_lost_ntc  <- renderText({
    x <- scenario()$ntc_removed
    if (is.null(x) || x == 0) "—" else dollar(x, accuracy = 1)
  })
  output$off_ea_per_ac <- renderText({
    x <- scenario()$ea_per_ac
    if (is.null(x) || is.na(x)) "—" else dollar(x, accuracy = 1)
  })

  off_labels <- c(
    "Residential (Low Density)"    = "Residential (1)",
    "Residential (Medium Density)" = "Residential (2-6)",
    "Residential (High Density)"   = "Residential (>6)",
    "Commercial"                   = "Commercial"
  )
  output$offset_rows <- renderUI({
    sc <- scenario()
    tagList(lapply(names(off_labels), function(cat) {
      ac_val  <- sc$offset_acres[cat]
      per_ac  <- NTC_PER_ACRE[cat]
      ratio   <- sc$offset_ratio[cat]
      fmt_ac  <- if (is.na(ac_val) || is.infinite(ac_val) || ac_val == 0) "—"
                 else paste0(formatC(ac_val, digits = 1, format = "f"), " ac")
      fmt_pa  <- dollar(per_ac, accuracy = 1)
      fmt_r   <- if (is.na(ratio) || is.infinite(ratio) || ratio == 0) "—"
                 else paste0(formatC(ratio, digits = 1, format = "f"), " ea")

      div(style = "padding:8px 0; border-bottom:1px solid #eee;",
          div(style = "font-size:12px; color:#333; font-weight:600; margin-bottom:4px;",
              off_labels[cat]),
          tags$ul(style = "margin:0; padding-left:18px; font-size:11px; color:#555; line-height:1.5;",
            tags$li(tags$b(fmt_pa, "/ac"),
                    " — Dayton's average tax capacity per acre (computed from existing parcels in this zoning)"),
            tags$li(tags$b(fmt_ac),
                    " — acres needed to generate the tax capacity your easements removed"),
            tags$li(tags$b("1 ac = ", fmt_r),
                    " — one acre replaces this many acres of your current easement in tax capacity")
          ))
    }))
  })

  output$sug_n    <- renderText(length(scenario()$suggested_pids))
  output$sug_ac   <- renderText({
    x <- scenario()$sug_acres
    if (is.null(x) || x == 0) "—" else paste0(formatC(x, digits = 1, format = "f"), " ac")
  })
  output$sug_gain <- renderText({
    x <- scenario()$sug_gain
    if (is.null(x) || x == 0) "—" else dollar(x, accuracy = 1)
  })
  output$sug_net <- renderText({
    sc <- scenario()
    if (is.null(sc$sug_gain) || sc$sug_gain == 0) return("—")
    d <- sc$sug_gain - sc$ntc_removed
    paste0(ifelse(d >= 0, "+", "−"), dollar(abs(d), accuracy = 1))
  })
  output$sug_coverage <- renderText({
    sc <- scenario()
    if (is.null(sc$sug_gain) || sc$sug_gain == 0 ||
        sc$ntc_removed == 0) return("")
    ratio <- sc$sug_gain / sc$ntc_removed
    sprintf("%.2f× coverage of lost capacity", ratio)
  })
  output$sug_infill_count <- renderText({
    n <- scenario()$sug_n_infill
    if (is.null(n)) 0L else as.character(n)
  })
  output$sug_rezone_count <- renderText({
    n <- scenario()$sug_n_rezone
    if (is.null(n)) 0L else as.character(n)
  })
  output$sug_by_type <- renderUI({
    br <- scenario()$sug_breakdown
    if (is.null(br) || nrow(br) == 0) return(NULL)
    tagList(
      div(style = "font-size:10px; color:#888; text-transform:uppercase; margin-top:6px;",
          "Develop-as breakdown"),
      tagList(lapply(seq_len(nrow(br)), function(i) {
        div(style = "font-size:11px; color:#555; padding:2px 0;",
            sprintf("%s: %d parcel%s, %.1f ac",
                    br$target_type[i], br$parcels[i],
                    ifelse(br$parcels[i] == 1, "", "s"),
                    br$acres[i]))
      }))
    )
  })

  output$total_shift <- renderText(
    dollar(scenario()$total_shift, accuracy = 1))
  output$avg_shift <- renderText(
    dollar(scenario()$avg_shift_dollar, accuracy = 0.01))
  output$avg_pct_shift <- renderText(
    percent(scenario()$avg_shift_pct / 100, accuracy = 0.001))
}

shinyApp(ui, server)
