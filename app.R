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
# Offset $/ac rows are built below (offset_stats_built), not from the RDS buckets
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

## Built property type from raw assessor codes ----
# Built type per PID from the raw PROPERTY_TYPE codes (dayton_parcel_codes.csv,
# written by build_offset_benchmarks.R). Grouped under main.R's SUPER_MAP groups
# with these changes (decisions, Oct 2026): townhouse (Y) is its own type;
# apartments are A/AX plus condos (X/XC) and low-income rentals (apartment
# buildings); two-unit (DB) and triplex (TP) stay residential, as Hennepin names
# them. Codes main.R left unmapped go to their main group (D, B, S residential;
# CR special use), and K is common area (HOA lots, outlots).
code_groups <- c(
  R = "Residential", RL = "Residential", RZ = "Residential", RM = "Residential", XM = "Residential",
  D = "Residential", B = "Residential", S = "Residential", DB = "Residential", TP = "Residential",
  Y = "Townhome",
  A = "Apartment", AX = "Apartment", X = "Apartment", XC = "Apartment",
  HL = "Apartment", HM = "Apartment", HR = "Apartment", HS = "Apartment",
  C = "Commercial", LC = "Commercial", SL = "Commercial",
  I = "Industrial", LI = "Industrial",
  F = "Agricultural", FF = "Agricultural", FP = "Agricultural", LF = "Agricultural",
  FH = "Agricultural", FM = "Agricultural",
  LA = "Vacant", LR = "Vacant", LL = "Vacant", LM = "Vacant", LV = "Vacant",
  SC = "Special use", NC = "Special use", NI = "Special use", GC = "Special use",
  HF = "Special use", CR = "Special use",
  ME = "Manufactured homes", MH = "Manufactured homes",
  K = "Common area"
)
# Multi-code PIDs: commercial + any housing = mixed use; any farm subrecord =
# agricultural; otherwise the most intensive use present wins.
use_priority <- c("Apartment", "Townhome", "Commercial", "Industrial", "Residential",
                  "Manufactured homes", "Special use", "Vacant", "Common area", "Unknown")
built_type_from_codes <- function(codes, acres, in_sim) {
  vapply(seq_along(codes), function(i) {
    if (!in_sim[i]) return("Exempt (public, parks, schools)")
    if (is.na(codes[i]) || codes[i] == "") return("Unknown")
    grp <- unname(code_groups[strsplit(codes[i], ",", fixed = TRUE)[[1]]])
    grp <- unique(ifelse(is.na(grp), "Unknown", grp))
    if ("Commercial" %in% grp && any(c("Residential", "Townhome", "Apartment") %in% grp)) return("Mixed use")
    if ("Agricultural" %in% grp) return("Agricultural")
    pick <- intersect(use_priority, grp)[1]
    # 2-acre split ~ the R-2 "Single-Family Large Lot" minimum (90,000 SF = 2.07 ac),
    # rounded to 2 (decision, Oct 2026): 2+ ac lots meet Dayton's large-lot standard
    if (pick == "Residential") {
      pick <- if (isTRUE(acres[i] >= 2)) "Residential, lot 2 ac or more" else "Residential, lot under 2 ac"
    }
    pick
  }, character(1))
}
parcel_codes <- fread("dayton_parcel_codes.csv", colClasses = "character")
parcel_plats <- fread("dayton_parcel_plats.csv", colClasses = "character")
geom$codes      <- parcel_codes$codes[match(geom$PID, parcel_codes$PID)]
geom$plat_name  <- parcel_plats$plat_name[match(geom$PID, parcel_plats$PID)]
geom$n_groups   <- vapply(strsplit(ifelse(is.na(geom$codes), "", geom$codes), ",", fixed = TRUE),
                          function(cd) length(unique(code_groups[cd])), integer(1))
geom$built_type <- built_type_from_codes(geom$codes, geom$acres, geom$in_sim)

## Offset rows ----
# Replaces the inherited du/acre buckets (1 / 2-6 / >6), which were generic
# project labels, not Dayton standards. Rows are built property types.
# $/ac is acre-weighted (sum tax capacity / sum acres), matching data_prep.R.
# Taxable parcels only (in_sim) with acres > 0; exempt parcels carry no capacity.
taxable_parcels_df <- as.data.table(sf::st_drop_geometry(geom))[in_sim == TRUE & acres > 0]
is_subdivision <- function(plat_name) !is.na(plat_name) & !grepl("^unplatted", plat_name, ignore.case = TRUE)

## Offset rows by built type ----
# $/ac of development: each type's tax capacity over its lot acres plus its
# share of its subdivision's common lots (K: HOA land, outlots). Common land in
# a plat is split by each type's share of the plat's other parcels -- a judgment
# call; splitting by lot acres would give large lots more. This matches how peer
# apartments (whose grounds sit on the parcel) count land. Dayton rows use single-use PIDs only (one code group).
# Dayton has 1 apartment parcel and no mixed use, so those rows use peer-city
# benchmarks (build_offset_benchmarks.R; outer-ring peers).
subdivision_df <- taxable_parcels_df[is_subdivision(plat_name)]
plat_common <- subdivision_df[built_type == "Common area",
                              .(common_ac = sum(acres), common_tc = sum(tax_capacity)), by = plat_name]
plat_type_share <- subdivision_df[built_type != "Common area", .(n = .N), by = .(plat_name, built_type)][
  , share := n / sum(n), by = plat_name]
common_alloc <- merge(plat_type_share, plat_common, by = "plat_name")[
  , .(alloc_ac = sum(share * common_ac), alloc_tc = sum(share * common_tc)), by = built_type]
dayton_built_types <- c("Residential, lot under 2 ac", "Residential, lot 2 ac or more",
                        "Townhome", "Commercial", "Industrial")
dayton_built_stats <- rbindlist(lapply(dayton_built_types, function(type_name) {
  type_parcels <- taxable_parcels_df[built_type == type_name & n_groups == 1]
  alloc    <- common_alloc[built_type == type_name]
  extra_ac <- if (nrow(alloc)) alloc$alloc_ac else 0
  extra_tc <- if (nrow(alloc)) alloc$alloc_tc else 0
  data.table(category   = type_name,
             parcels    = nrow(type_parcels),
             acres      = sum(type_parcels$acres) + extra_ac,
             ntc_per_ac = (sum(type_parcels$tax_capacity) + extra_tc) / (sum(type_parcels$acres) + extra_ac),
             basis_note = sprintf("Dayton parcels with this use, incl. %s ac share of subdivision common lots",
                                  formatC(extra_ac, format = "f", digits = 1)))
}))
peer_benchmarks <- fread("offset_benchmarks_peers.csv")
peer_stats <- peer_benchmarks[, .(category   = paste0(category, " (peer cities)"),
                                  parcels, acres, ntc_per_ac,
                                  basis_note = paste0("benchmark from ", sub("^[^:]*: ", "", geography)))]
offset_stats_built <- rbind(dayton_built_stats, peer_stats)
expected_rows <- c("Residential, lot under 2 ac", "Residential, lot 2 ac or more", "Townhome",
                   "Apartments (peer cities)", "Mixed use (peer cities)", "Commercial", "Industrial")
stopifnot(setequal(offset_stats_built$category, expected_rows))
# Rank rows by $/ac, high to low, keeping the two residential lot-size rows
# together: the pair sits where its higher-value row (under 2 ac) ranks.
ranked_rows <- setdiff(offset_stats_built[order(-ntc_per_ac), category], "Residential, lot 2 ac or more")
ranked_rows <- append(ranked_rows, "Residential, lot 2 ac or more",
                      after = match("Residential, lot under 2 ac", ranked_rows))
offset_stats_built <- offset_stats_built[match(ranked_rows, category)]


# Residential density family runs light -> dark = low -> high density. The former
# single "Residential (High Density)" bucket is split into its three underlying
# zoning districts so RM / RH / R-MH each get a distinct color + legend entry.
zone_colors <- c(
  "Agricultural"                     = "#F1C40F",   # warm yellow (productive ag)
  "Agricultural (Transitional)"      = "#FCE96A",   # pale yellow
  "High-density multifamily"       = "#08306B",   # darkest navy (highest density)
  "Townhome / multifamily"    = "#2171B5",   # strong blue
  "Manufactured home park"              = "#EC7014",   # orange (distinct housing type)
  "Residential (Medium Density)"     = "#6BAED6",   # mid-light blue (SF / townhome)
  "Residential (Low Density)"        = "#C6DBEF",   # lightest blue (large lot)
  "Mixed Use"                        = "#C44536",   # brick red (moved off gold)
  "Commercial"                       = "#8E44AD",
  "Industrial"                       = "#5A6268",
  "Parks / Recreation"               = "#2D6A4F",   # green (only green left)
  "Essential Services"               = "#ADB5BD",
  "Other"                            = "#CCCCCC"
)
# Map a zone_code + its display_category to a color key: split the three
# high-density districts by code, otherwise fall back to the display_category.
zone_color_cat <- function(code, disp) {
  out <- as.character(disp)
  out[code %in% "R-M"]  <- "Townhome / multifamily"
  out[code %in% "RH"]   <- "High-density multifamily"
  out[code %in% "R-MH"] <- "Manufactured home park"
  out
}
zone_pal <- colorFactor(unname(zone_colors), levels = names(zone_colors),
                        ordered = TRUE, na.color = "#CCCCCC")

# Display labels: housing-type names shared by the legend, hover and offset rows.
# Internal `display_category` values stay as logic/color keys; only user-facing
# text is relabeled. (Replaced the old generic du/acre ranges 1 / 2-6 / >6.)
density_range_labels <- c(
  "Residential (Low Density)"    = "Large-lot single-family",
  "Residential (Medium Density)" = "Single-family",
  "Residential (High Density)"   = "Multifamily",  # fallback only; RM/RH/R-MH are split by color_cat
  # data_prep.R keys A-2 (and the retired A-3) as "Transitional"; in the current
  # code A-2 is the Special Homestead Agricultural district, so relabel for display
  "Agricultural (Transitional)"  = "Agricultural (Homestead)"
)
# Zoning districts appended to legend entries so the map reads without the modal
legend_district_codes <- c(
  "Residential (Low Density)"    = "R-E, R-2",
  "Residential (Medium Density)" = "R-1, R-1A, R-3, R-O",
  "Townhome / multifamily"       = "RM",
  "High-density multifamily"     = "RH",
  "Manufactured home park"       = "R-MH"
)
zone_legend_labels <- function(keys) {
  lbl   <- relabel_density(keys)
  codes <- unname(legend_district_codes[keys])
  ifelse(is.na(codes), lbl, paste0(lbl, " (", codes, ")"))
}
relabel_density <- function(x) {
  out <- unname(density_range_labels[as.character(x)])
  ifelse(is.na(out), as.character(x), out)
}
# Carry a display-only label alongside the logic key
# Color key that splits RM / RH / R-MH out of the aggregated high-density bucket
zoning$color_cat  <- zone_color_cat(zoning$zone_code, zoning$display_category)
geom$color_cat    <- zone_color_cat(geom$zone_code, geom$display_category)
zoning$disp_label <- relabel_density(zoning$color_cat)
geom$disp_label   <- relabel_density(geom$color_cat)

## Built type colors (map) ----
built_colors <- c(
  "Residential, lot under 2 ac"     = "#6BAED6",
  "Residential, lot 2 ac or more"   = "#C6DBEF",
  "Townhome"                        = "#41B6C4",
  "Apartment"                       = "#08519C",
  "Manufactured homes"              = "#EC7014",
  "Mixed use"                       = "#C44536",
  "Commercial"                      = "#8E44AD",
  "Industrial"                      = "#5A6268",
  "Agricultural"                    = "#F1C40F",
  "Vacant"                          = "#E6DCC3",
  "Common area"                     = "#E5DAF0",
  "Special use"                     = "#ADB5BD",
  "Exempt (public, parks, schools)" = "#B7D3B0",
  "Unknown"                         = "#CCCCCC"
)
stopifnot(all(geom$built_type %in% names(built_colors)))
built_pal <- colorFactor(unname(built_colors), levels = names(built_colors),
                         ordered = TRUE, na.color = "#CCCCCC")

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

  tags$details(open = NA,
    style = "margin:0 15px 10px; padding:8px 14px; background:#F0F7F4; border:1px solid #CBE3D8; border-radius:6px; font-size:13px;",
    tags$summary(style = "cursor:pointer; font-weight:600; color:#1B4332;",
                 "How to use this tool"),
    tags$ol(style = "margin:8px 0 2px; padding-left:20px; line-height:1.6; color:#333;",
      tags$li("Click a parcel on the map to place it under a conservation easement. Click it again to remove it."),
      tags$li("The panel on the right recalculates live: the city's total tax capacity falls, the tax rate rises to raise the same fixed levy, and that increase is shifted onto the remaining (non-easement) parcels."),
      tags$li("Use the Deduction rate slider to model a partial easement (anything less than a full tax exemption)."),
      tags$li("Switch the map between Built type and Zoning at its top-left; the Parcels checkbox at the top-right turns that coloring on or off. Natural Value Score, Protected Land and Existing Easements are in the same control."),
      tags$li("The Definitions and Methodology buttons at the bottom of the panel explain every term and formula.")
    )
  ),

  fluidRow(
    column(8, div(style = "position:relative;",
      leafletOutput("map", height = "88vh"),
      # Map fill toggle; the "Parcels" checkbox in the layer control turns it on/off
      absolutePanel(top = 10, left = 55, draggable = FALSE,
        style = "background:rgba(255,255,255,0.92); padding:4px 10px 0; border-radius:4px; box-shadow:0 1px 4px rgba(0,0,0,0.3); z-index:1000;",
        radioButtons("map_fill", label = NULL, choices = c("Built type", "Zoning"),
                     selected = "Built type", inline = TRUE)))),
    column(4, div(class = "sidebar",
      # --- Natural Systems Scores panel commented out (Aug 2026) ---------------
      # (High Priority Parcels / Connectedness / Distance to Protected, plus the
      #  scorebars JS handler). Server-side score math still computes but is unused;
      #  re-enable this block + the scorebars observe/output bindings to restore.
      # div(style = "background:#1B4332; color:white; padding:12px 14px; border-radius:6px; margin-bottom:14px;",
      #     div(style = "font-size:13px; opacity:0.85; letter-spacing:0.5px; margin-bottom:12px;",
      #         "NATURAL SYSTEMS SCORES"),
      #     # --- Score 1: High Priority Parcels ---
      #     div(style = "margin-bottom:12px;",
      #       div(style = "display:flex; justify-content:space-between; align-items:baseline;",
      #           tags$span(style = "font-size:13px;", "High Priority Parcels"),
      #           tags$span(style = "font-size:18px; font-weight:700;",
      #                     textOutput("s1_value", inline = TRUE),
      #                     tags$span("/100", style = "font-size:12px; opacity:0.7;"))),
      #       div(style = "margin-top:4px; background:rgba(255,255,255,0.15); height:6px; border-radius:3px; overflow:hidden;",
      #           div(class = "s1bar", style = "height:100%; background:#52B788; width:0%; transition:width 0.3s;")),
      #       div(style = "font-size:10px; opacity:0.75; margin-top:3px;",
      #           textOutput("s1_detail", inline = TRUE))),
      #     # --- Score 2: Connectedness ---
      #     div(style = "margin-bottom:12px;",
      #       div(style = "display:flex; justify-content:space-between; align-items:baseline;",
      #           tags$span(style = "font-size:13px;", "Connectedness"),
      #           tags$span(style = "font-size:18px; font-weight:700;",
      #                     textOutput("s2_value", inline = TRUE),
      #                     tags$span("/100", style = "font-size:12px; opacity:0.7;"))),
      #       div(style = "margin-top:4px; background:rgba(255,255,255,0.15); height:6px; border-radius:3px; overflow:hidden;",
      #           div(class = "s2bar", style = "height:100%; background:#52B788; width:0%; transition:width 0.3s;")),
      #       div(style = "font-size:10px; opacity:0.75; margin-top:3px;",
      #           textOutput("s2_detail", inline = TRUE))),
      #     # --- Score 3: Distance Reduction ---
      #     div(style = "margin-bottom:6px;",
      #       div(style = "display:flex; justify-content:space-between; align-items:baseline;",
      #           tags$span(style = "font-size:13px;", "Distance to Protected"),
      #           tags$span(style = "font-size:18px; font-weight:700;",
      #                     textOutput("s3_value", inline = TRUE),
      #                     tags$span("/100", style = "font-size:12px; opacity:0.7;"))),
      #       div(style = "margin-top:4px; background:rgba(255,255,255,0.15); height:6px; border-radius:3px; overflow:hidden;",
      #           div(class = "s3bar", style = "height:100%; background:#52B788; width:0%; transition:width 0.3s;")),
      #       div(style = "font-size:10px; opacity:0.75; margin-top:3px;",
      #           textOutput("s3_detail", inline = TRUE)))
      # ),
      # tags$script(HTML(
      #   "Shiny.addCustomMessageHandler('scorebars', function(pct) {
      #      document.querySelector('.s1bar').style.width = pct.s1 + '%';
      #      document.querySelector('.s2bar').style.width = pct.s2 + '%';
      #      document.querySelector('.s3bar').style.width = pct.s3 + '%';
      #    });"
      # )),
      # Tax shift summary, compact and first: the headline effect of the selection
      div(style = "background:#F2F2F2; border:1px solid #DDDDDD; border-radius:6px; padding:8px 10px; margin-bottom:12px;",
          div(style = "font-size:11px; font-weight:600; color:#555; text-transform:uppercase; margin-bottom:4px;",
              "City taxes shifted to remaining parcels"),
          div(style = "display:flex; justify-content:space-between; gap:8px; font-size:10px; color:#666;",
              div(div("Total city taxes"),  div(style = "font-size:14px; font-weight:600; color:#333;", textOutput("total_shift", inline = TRUE))),
              div(div("Avg per parcel"), div(style = "font-size:14px; font-weight:600; color:#333;", textOutput("avg_shift", inline = TRUE))),
              div(div("Avg % increase"), div(style = "font-size:14px; font-weight:600; color:#333;", textOutput("avg_pct_shift", inline = TRUE))))),
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
          div(class = "label", "Lost tax capacity (tax base, not taxes)"),
          div(class = "value", textOutput("off_lost_ntc", inline = TRUE))),
      div(class = "metric",
          div(class = "label", "Lost tax capacity per acre"),
          div(class = "value", textOutput("off_ea_per_ac", inline = TRUE))),
      hr(),
      h4("Densification offset"),
      div(style = "font-size:10px; color:#888; margin:-4px 0 6px; line-height:1.4;",
          "By built property type: what each use yields per acre today."),
      div(style = "font-size:10px; color:#888; text-transform:uppercase; margin-bottom:4px;",
          "Acres needed to offset"),
      uiOutput("offset_rows"),
      # --- Suggested Development (black layer) subsection commented out (Aug 2026) ---
      # Greedy per-parcel development picks + map layer disabled. Densification
      # offset (acres-needed-by-type, above) is retained. Re-enable this block,
      # the "Suggested Development" map layer/observe, and the sug_* output
      # bindings to restore.
      # tags$hr(style = "margin:8px 0; border-top:1px dashed #ccc;"),
      # div(style = "font-size:10px; color:#888; text-transform:uppercase; margin-bottom:4px;",
      #     "Suggested development (black layer)"),
      # div(class = "metric",
      #     div(class = "label", "Total acres to develop"),
      #     div(class = "value", textOutput("sug_ac", inline = TRUE))),
      # div(class = "metric",
      #     div(class = "label", "Tax capacity gained"),
      #     div(class = "value", textOutput("sug_gain", inline = TRUE))),
      # div(class = "metric",
      #     div(class = "label", "Net vs. lost capacity"),
      #     div(class = "value", textOutput("sug_net", inline = TRUE)),
      #     div(class = "delta", textOutput("sug_coverage", inline = TRUE))),
      # div(class = "metric",
      #     div(class = "label", "Within current zoning"),
      #     div(class = "value", textOutput("sug_infill_count", inline = TRUE))),
      # div(class = "metric",
      #     div(class = "label", "Would need rezone"),
      #     div(class = "value", textOutput("sug_rezone_count", inline = TRUE))),
      # uiOutput("sug_by_type"),
      # p(style = "font-size:10px; color:#888; margin-top:4px;",
      #   "Optimization priorities (in order): (1) vacant infill lots first, ",
      #   "(2) highest $/acre gain, (3) minimize land used, ",
      #   "(4) prefer parcels near existing residential. ",
      #   "Toggle 'Suggested Development' in the layer control to view."),
      hr(),
      actionButton("reset", "Reset", width = "100%",
                   icon = icon("rotate-left")),
      br(), br(),
      actionButton("show_definitions", "Definitions",
                   width = "100%", icon = icon("book"),
                   style = "background:#fff; border:1px solid #ccc;"),
      br(), br(),
      actionButton("show_methodology", "Methodology",
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
      # Two fills (zoning polygons, parcels by built use) toggled by input$map_fill.
      # Both are non-interactive so clicks reach the parcel click layer on top.
      addPolygons(data = zoning,
                  fillColor = ~zone_pal(color_cat),
                  fillOpacity = 0.80, color = "#444", weight = 0.4,
                  group = "Zoning",
                  options = pathOptions(interactive = FALSE)) |>
      addPolygons(data = geom,
                  fillColor = ~built_pal(built_type),
                  fillOpacity = 0.80, color = "#444", weight = 0.4,
                  group = "Built type",
                  options = pathOptions(interactive = FALSE)) |>
      # Parcel outlines: the "Parcels" master switch
      addPolygons(data = geom, fill = FALSE, color = "#00000030", weight = 0.3,
                  group = "Parcels", options = pathOptions(interactive = FALSE)) |>
      # Click / hover layer: always on (not in the layer control)
      addPolygons(data = geom,
                  layerId = ~PID,
                  fillColor = "#ffffff", fillOpacity = 0,
                  color = "#00000010", weight = 0.2,
                  highlightOptions = highlightOptions(
                    weight = 2.5, color = "#000", fillOpacity = 0,
                    bringToFront = TRUE),
                  group = "parcel_click",
                  label = ~lapply(paste0(
                    "<b>PID ", PID, "</b>",
                    ifelse(in_sim, "", " <i>(exempt)</i>"), "<br>",
                    "Zone: ", ifelse(is.na(zone_code), "—",
                                     paste0(zone_code, " (", disp_label, ")")), "<br>",
                    "Natural Value Score: ", priority_score, "/7<br>",
                    "Tax capacity: ",
                    ifelse(in_sim,
                           paste0("$", formatC(tax_capacity, format = "d", big.mark = ",")),
                           "n/a"), "<br>",
                    "Acres: ",
                    ifelse(is.na(acres), "n/a",
                           formatC(acres, digits = 2, format = "f")), "<br>",
                    "Built type: ", built_type, "<br>",
                    "Tax capacity / acre: ",
                    ifelse(in_sim & !is.na(acres) & acres > 0,
                           paste0("$", comma(tax_capacity / acres, accuracy = 1)),
                           "n/a")
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
                  group = "Natural Value Score",
                  options = pathOptions(interactive = FALSE),
                  label = ~paste0("Natural Value Score: ", priority_score, "/7")) |>
      addLayersControl(
        overlayGroups = c("Parcels", "Natural Value Score",
                          "Protected Land", "Existing Easements"),
        options = layersControlOptions(collapsed = FALSE)) |>
      # Zoning / Built type legends are swapped by the map-fill observer below
      # (leaflet only auto-hides legends for groups in the layer control)
      addLegend("bottomleft", pal = zonation_pal,
                values = names(zonation_colors),
                title = "Natural Value Score<br><small>1 = low, 7 = highest conservation value; 0 = not scored</small>",
                opacity = 0.8, group = "Natural Value Score") |>
      # Hide after the legends are added so the legends start hidden too
      hideGroup(c("Natural Value Score", "Zoning")) |>
      fitBounds(
        lng1 = min(st_bbox(geom)[["xmin"]], st_bbox(zoning)[["xmin"]]),
        lat1 = min(st_bbox(geom)[["ymin"]], st_bbox(zoning)[["ymin"]]),
        lng2 = max(st_bbox(geom)[["xmax"]], st_bbox(zoning)[["xmax"]]),
        lat2 = max(st_bbox(geom)[["ymax"]], st_bbox(zoning)[["ymax"]]))
  })

  # ---- map fill: built type vs zoning, gated by the "Parcels" checkbox ----
  observe({
    visible_groups <- input$map_groups
    parcels_on <- is.null(visible_groups) || "Parcels" %in% visible_groups
    fill       <- if (is.null(input$map_fill)) "Built type" else input$map_fill
    proxy      <- leafletProxy("map") |> removeControl("fill_legend")
    if (parcels_on) {
      proxy |> showGroup(fill) |> hideGroup(setdiff(c("Zoning", "Built type"), fill))
      if (fill == "Zoning") {
        proxy |> addLegend("bottomleft", colors = unname(zone_colors),
                           labels = zone_legend_labels(names(zone_colors)), title = "Zoning",
                           opacity = 0.8, layerId = "fill_legend")
      } else {
        proxy |> addLegend("bottomleft", colors = unname(built_colors),
                           labels = names(built_colors), title = "Built type",
                           opacity = 0.8, layerId = "fill_legend")
      }
    } else {
      proxy |> hideGroup(c("Zoning", "Built type"))
    }
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

  observeEvent(input$show_definitions, {
    dfn <- function(term, ...) tags$p(style = "margin:6px 0;",
      tags$b(term), " — ", ...)
    showModal(modalDialog(
      title = "Definitions",
      size = "l", easyClose = TRUE,
      dfn("Natural Value Score",
          "A 1–7 conservation-priority ranking from the Zonation analysis: 1 = lowest, ",
          "7 = highest natural/conservation value (top ~1%); 0 = not scored. ",
          "Higher scores flag land most valuable to protect."),
      dfn("Tax Capacity",
          "A parcel's taxable value after Minnesota class rates are applied ",
          "(market value × class rate, which varies by property type). ",
          "Property taxes are levied against tax capacity, not market value."),
      dfn("Levy",
          "The fixed dollar amount the city must raise from property taxes each year. ",
          "It is held constant in this tool — easements don't change what the city needs, ",
          "only who pays it."),
      dfn("Tax Rate",
          "Levy ÷ total tax capacity. When easements remove tax capacity, the rate must ",
          "rise so the same levy is still collected."),
      dfn("Conservation Easement",
          "A permanent legal restriction on a parcel. Here it removes the parcel's ",
          "eligible tax capacity from the city's tax base in exchange for protection."),
      dfn("Deduction Rate",
          "How much of a selected parcel's tax capacity the easement removes: ",
          "100% = a full exemption, lower values = a partial easement."),
      dfn("Lost Tax Capacity",
          "The total tax capacity removed by the easements you have selected."),
      dfn("City Taxes Shifted",
          "Dollars of city property tax moved onto the remaining parcels. The city levy is ",
          "fixed, so when easements shrink the tax base, the city rate rises and every other ",
          "parcel pays a little more. This is taxes (base × rate), not tax base, and covers ",
          "the city levy only; county and school rates are held fixed. Shown in total and per remaining parcel."),
      dfn("Densification Offset",
          "How much new development — expressed in acres by type — would generate enough ",
          "tax capacity to replace what the easements removed, at the current average ",
          "tax capacity per acre for that built property type (Dayton parcels, with ",
          "peer-city benchmarks for apartments and mixed use)."),
      dfn("Suggested Development",
          "An illustrative, automated set of candidate parcels whose development would ",
          "offset the lost capacity. It is a what-if scenario, not a recommendation to ",
          "develop any specific property."),
      tags$hr(),
      tags$p(style = "font-size:12px; color:#888;",
             "For the underlying formulas and data sources, see the ",
             tags$b("Methodology"), " button."),
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$show_methodology, {
    showModal(modalDialog(
      title = "Methodology",
      size = "l", easyClose = TRUE,
      tags$h4("Natural Systems Scores"),
      tags$p(tags$b("1. High Priority Parcels"), " — fraction of Dayton's top Natural Value Score parcels (score 7, top ~1% of conservation priority) placed in easement."),
      tags$code("score = 100 × (# score-7 parcels selected) / (# score-7 parcels in Dayton)"),
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
      tags$p(tags$b("Acres needed by type"), " — how many acres of each development type would have to exist at the current average $/acre for that type to recoup the lost capacity. Types are Dayton parcels by actual assessor property type (residential split at a 2-acre lot, about the R-2 large-lot minimum; townhomes separate), each counted with its share of subdivision common lots. Apartments and mixed use are benchmarked on outer-ring peer cities (Thrive 2040 Suburban Edge / Emerging Suburban Edge Hennepin cities, excluding Plymouth and Maple Grove; mixed use from Rogers and Orono) because Dayton has almost none built."),
      tags$code("offset_acres[type] = ntc_removed / ntc_per_ac[type]"),
      tags$p(tags$b("Equivalency ratio"), " — how many easement acres one acre of that development type offsets:"),
      tags$code("ratio = ntc_per_ac[type] / ea_per_ac"),
      tags$p("\"1 ac = 22 ea\" means 1 acre of that use replaces 22 acres of your easement in tax capacity."),

      tags$hr(),
      tags$h4("City Taxes Shifted to Remaining Parcels"),
      tags$p("Since the city levy is fixed, the city taxes that the removed capacity used to pay are picked up by non-easement parcels via the rate increase: shift = remaining tax base × (new rate − old rate), which is roughly lost tax capacity × city rate. These are city tax dollars, not tax base; county and school rates are held fixed. Dashboard shows the aggregate and average increase across those parcels. Note: this section reflects easements only; it does not assume the suggested development has been built."),

      tags$hr(),
      tags$h4("Suggested Development (Black Layer)"),
      tags$p("A greedy optimizer proposes parcels to upzone/build on that would offset the tax capacity your easements removed. Toggle the layer via the map's layer control."),
      tags$p(tags$b("Candidates"), " — taxable parcels that are either (a) in Agricultural / Agricultural (Homestead) / Other zoning, or (b) have a vacant property type, and are not already protected or user-selected."),
      tags$p(tags$b("Filters applied to candidates:")),
      tags$ul(
        tags$li(tags$b("Min area ≥ 1 acre"), " — drops sliver remnants."),
        tags$li(tags$b("Polsby-Popper compactness ≥ 0.25"), " — drops long thin shapes (railroad / utility ROWs, drainage and stormwater strips). Formula: 4π × area / perimeter², where 0 = infinitely thin, 1 = perfect circle."),
        tags$li(tags$b("< 50% wetland overlap"), " — drops parcels protected from construction by regulation (NWI)."),
        tags$li(tags$b("Natural Value Score < 5"), " — excludes high conservation-priority land (top ~25% by Natural Value Score) from development candidates. Vacant wooded/natural areas with high conservation value shouldn't be suggested for paving, even if zoning allows it."),
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
        tags$li(tags$b("Natural Value Score (Zonation conservation prioritization):"), " kristine_parcel_coded_zonation; score 1 = 0–25th pct, 7 = 99–100th pct."),
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

  # ---- redraw suggested development layer (DISABLED Aug 2026) -----------
  # Suggested Development turned off; if(FALSE) disables the observe reversibly.
  if (FALSE) observe({
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
    s1_detail <- sprintf("%d of %d top natural-value (score 7) parcels in easement",
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
    # Per-type offset acres and ratios are computed in output$offset_rows, so
    # switching the built/zoning toggle doesn't rerun this whole reactive.

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

  output$offset_rows <- renderUI({
    sc    <- scenario()
    tbl   <- offset_stats_built
    no_selection <- sc$ntc_removed == 0
    list_style   <- "margin:0; padding-left:18px; font-size:11px; color:#555; line-height:1.5;"
    select_hint  <- tags$li(style = "color:#999;", "Select parcels on the map to see the acres needed to offset.")
    tagList(lapply(seq_len(nrow(tbl)), function(i) {
      cat     <- tbl$category[i]
      per_ac  <- tbl$ntc_per_ac[i]
      # acres of this type needed to recoup the lost capacity, and
      # "1 acre of this type offsets how many easement acres"
      ac_val  <- if (no_selection) NA_real_ else sc$ntc_removed / per_ac
      ratio   <- if (is.na(sc$ea_per_ac) || sc$ea_per_ac == 0) NA_real_ else per_ac / sc$ea_per_ac
      fmt_ac  <- if (is.na(ac_val) || is.infinite(ac_val)) "—"
                 else paste0(formatC(ac_val, digits = 1, format = "f"), " ac")
      fmt_pa  <- dollar(per_ac, accuracy = 1)
      fmt_r   <- if (is.na(ratio) || is.infinite(ratio) || ratio == 0) "—"
                 else paste0(formatC(ratio, digits = 1, format = "f"), " ea")
      # sample details live in a hover tooltip on the row title
      hover   <- sprintf("%s; %s parcels, %s ac", tbl$basis_note[i],
                         comma(tbl$parcels[i]), comma(tbl$acres[i], accuracy = 0.1))
      title   <- div(style = "font-size:12px; color:#333; font-weight:600; margin-bottom:4px;",
                     span(title = hover, style = "cursor:help; border-bottom:1px dotted #999;", cat),
                     span(title = hover, style = "cursor:help; color:#999; font-weight:400; margin-left:4px;", "ⓘ"))

      {
        body <- tags$ul(style = list_style,
          tags$li(tags$b(fmt_pa, "/ac"), " — current average tax capacity per acre"),
          if (no_selection) select_hint else tagList(
            tags$li(tags$b(fmt_ac), " — acres needed to generate the tax capacity your easements removed"),
            tags$li(tags$b("1 ac = ", fmt_r),
                    " — one acre replaces this many acres of your current easement in tax capacity")))
      }
      div(style = "padding:8px 0; border-bottom:1px solid #eee;", title, body)
    }))
  })

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
