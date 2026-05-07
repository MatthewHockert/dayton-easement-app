# One-time data prep for the Dayton easement Shiny app.
# Run from the project root after the full pipeline has been sourced
# (pipeline_checkpoint.RData must exist at the project root).

library(data.table)
library(dplyr)
library(sf)

proj_root <- if (file.exists("pipeline_checkpoint.RData")) {
  normalizePath(".", winslash = "/")
} else if (file.exists("../pipeline_checkpoint.RData")) {
  normalizePath("..", winslash = "/")
} else {
  stop("Cannot find pipeline_checkpoint.RData. Run from project root or dayton_easement_app/.")
}
out_dir <- file.path(proj_root, "dayton_easement_app")

load(file.path(proj_root, "pipeline_checkpoint.RData"))

CITY <- "Dayton"

# --- Parcels (subrecord-level) -------------------------------------------
parcels <- as.data.table(res_selected$parcels)
if (inherits(parcels, "sf")) parcels <- sf::st_drop_geometry(parcels)
if ("geometry" %in% names(parcels)) parcels[["geometry"]] <- NULL

parcels <- parcels[CTU_NAME == CITY]
stopifnot(nrow(parcels) > 0)

# Pull the columns the app needs
keep_cols <- c("PID", "SUBRECORD_NO", "CTU_NAME", "CITY_ID", "SCHOOL_ID",
               "TAX_YEAR", "LOCAL_NET_TAX_CAPACITY", "ACREAGE",
               "is_priority_subrecord", "sub_super_property_type",
               "super_property_type", "priority_score")
keep_cols <- intersect(keep_cols, names(parcels))
parcels <- parcels[, ..keep_cols]

# --- Baseline city rate & levy (clean slate: no deductions applied) ------
rates <- as.data.table(tax_distrct_rates_2024)
rates[, DISTRICT_TYPE := tolower(DISTRICT_TYPE)]
city_rate <- rates[DISTRICT_TYPE == "city" & CITY_ID == parcels$CITY_ID[1] &
                   TAX_YEAR == 2024, unique(RATE)][1]

baseline_ntc_clean  <- parcels[, sum(LOCAL_NET_TAX_CAPACITY, na.rm = TRUE)]
baseline_levy <- baseline_ntc_clean * city_rate    # fixed city levy (invariant)

# "Today" baseline: existing easements already applied (100% LNTC deduction
# on priority subrecords). Computed below once we have the easement PID list.
# For now, baseline_ntc/baseline_rate are defined provisionally and overwritten later.
baseline_ntc  <- baseline_ntc_clean
baseline_rate <- city_rate

# Store per-row baseline tax (unchanged — this is just for math convenience)
parcels[, baseline_rate := city_rate]
parcels[, baseline_tax  := LOCAL_NET_TAX_CAPACITY * city_rate]

polygonize <- function(x) {
  # Only run make_valid on invalid features (the expensive op)
  inv <- !sf::st_is_valid(x)
  if (any(inv)) {
    message("  Fixing ", sum(inv), " invalid geometries...")
    x_fixed <- sf::st_make_valid(x[inv, ])
    x <- rbind(x[!inv, ], x_fixed)
  }
  # Handle any GEOMETRYCOLLECTIONs by extracting polygons per-row (rare case)
  tp <- as.character(sf::st_geometry_type(x))
  gc_idx <- which(tp == "GEOMETRYCOLLECTION")
  if (length(gc_idx) > 0) {
    message("  Extracting polygons from ", length(gc_idx), " GeometryCollections...")
    geoms <- sf::st_geometry(x)
    for (i in gc_idx) {
      parts <- tryCatch(
        sf::st_collection_extract(sf::st_sfc(geoms[[i]], crs = sf::st_crs(x)), "POLYGON"),
        error = function(e) sf::st_sfc(crs = sf::st_crs(x)))
      geoms[[i]] <- if (length(parts) == 0) sf::st_multipolygon()
                    else sf::st_union(parts)[[1]]
    }
    sf::st_geometry(x) <- geoms
  }
  # Drop non-polygon + empty
  tp <- as.character(sf::st_geometry_type(x))
  x <- x[tp %in% c("POLYGON", "MULTIPOLYGON"), ]
  x <- x[!sf::st_is_empty(x), ]
  sf::st_cast(x, "MULTIPOLYGON", warn = FALSE)
}

# --- Parcel geometries ---------------------------------------------------
# Dayton taxable parcels (assessor extract, already filtered above)
dayton_taxable_pids <- unique(parcels$PID)
message("Dayton taxable PIDs from assessor data: ", length(dayton_taxable_pids))

# Existing easements (closed HC conservation easements) in Dayton
message("Reading existing easements...")
existing_ea <- fread(file.path(proj_root, "data/established_easements_consolidated.csv"))
existing_ea[, PID := sprintf("%013s", as.character(PID))]
dayton_easements <- existing_ea[PID %in% unique(parcels$PID)]
existing_easement_pids <- unique(dayton_easements$PID)
message("Dayton parcels in existing easement: ", length(existing_easement_pids))

# Recompute baseline assuming existing easements already applied (today's state)
ntc_today_by_sub <- copy(parcels)
ntc_today_by_sub[, ntc_eff := LOCAL_NET_TAX_CAPACITY]
ntc_today_by_sub[PID %in% existing_easement_pids &
                 is_priority_subrecord == 1, ntc_eff := 0]
baseline_ntc  <- sum(ntc_today_by_sub$ntc_eff, na.rm = TRUE)
baseline_rate <- baseline_levy / baseline_ntc
message(sprintf("Baseline today (w/ existing easements): NTC=$%s, rate=%.4f%%",
                formatC(baseline_ntc, format = "d", big.mark = ","),
                baseline_rate * 100))

# Dayton exempt parcels (Met Council open parcels GDB), same approach as county_analysis.R
message("Reading Met Council exempt parcels...")
mc <- sf::st_read(
  file.path(proj_root, "data/fgdb_plan_parcels_open/plan_parcels_open.gdb"),
  query = paste0("SELECT county_pin, ctu_name, emv_total, acres_poly, xuseclass1, tax_exempt ",
                 "FROM plan_parcels_open ",
                 "WHERE co_name = 'Hennepin' AND ctu_name = 'Dayton' AND tax_exempt = 'Y'"),
  quiet = TRUE)
mc <- sf::st_drop_geometry(mc)
setDT(mc)
mc[, PID := trimws(as.character(county_pin))]
mc[, PID := fifelse(nchar(PID) == 12, paste0("0", PID), PID)]
dayton_exempt <- mc[!PID %in% dayton_taxable_pids]
message("Dayton exempt PIDs from Met Council: ", nrow(dayton_exempt))

dayton_pids <- unique(c(dayton_taxable_pids, dayton_exempt$PID))
message("Dayton total PIDs (taxable + exempt): ", length(dayton_pids))

geom_cache <- file.path(out_dir, "dayton_parcels_cache.rds")
if (file.exists(geom_cache)) {
  message("Loading cached Dayton parcel geometries...")
  geom <- readRDS(geom_cache)
} else {
  message("Reading parcel geometries and filtering by Dayton PIDs...")
  geom_all <- sf::st_read(file.path(proj_root, "data/parcel_geometries.shp"),
                          quiet = TRUE)
  geom_all <- geom_all[geom_all$PIN_2 %in% dayton_pids, c("PIN_2")]
  # parcel_geometries.shp contains yearly snapshots per PID (2005–2024);
  # keep the first geometry per PID
  geom_all <- geom_all[!duplicated(geom_all$PIN_2), ]
  message("Cleaning ", nrow(geom_all), " parcel polygons (projected CRS)...")
  geom_all <- polygonize(geom_all)
  geom <- sf::st_transform(geom_all, 4326)
  names(geom)[1] <- "PID"
  stopifnot(all(sf::st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON")))
  saveRDS(geom, geom_cache)
  message("Cached to ", geom_cache)
}

# Per-PID tax capacity (sum across subrecords) for hover
pid_attrs <- parcels[, .(
  tax_capacity = sum(LOCAL_NET_TAX_CAPACITY, na.rm = TRUE),
  acres = mean(ACREAGE, na.rm = TRUE),
  prop_type = paste(unique(sub_super_property_type), collapse = ", "),
  priority_score = suppressWarnings(max(priority_score, na.rm = TRUE))
), by = PID]
pid_attrs[is.infinite(priority_score), priority_score := 0]
pid_attrs[is.na(priority_score), priority_score := 0]
geom <- merge(geom, pid_attrs, by = "PID", all.x = TRUE)

# Annotate exempt parcels from Met Council data
exempt_info <- dayton_exempt[, .(PID, exempt_acres = acres_poly,
                                 xuseclass1 = xuseclass1)]
geom <- merge(geom, exempt_info, by = "PID", all.x = TRUE)
geom$in_sim       <- !is.na(geom$tax_capacity)     # TRUE for taxable parcels
geom$tax_capacity[!geom$in_sim] <- 0
geom$acres[!geom$in_sim]        <- geom$exempt_acres[!geom$in_sim]
geom$prop_type[!geom$in_sim]    <- paste0("Exempt — ", geom$xuseclass1[!geom$in_sim])
geom$priority_score[is.na(geom$priority_score)] <- 0
geom$exempt_acres <- NULL
geom$xuseclass1   <- NULL

# Flag existing easement parcels and attach easement meta
ea_info <- dayton_easements[, .(
  easement_holder = paste(unique(easement_holder), collapse = "; "),
  recorded_acres  = sum(recorded_acres, na.rm = TRUE)
), by = PID]
geom <- merge(geom, ea_info, by = "PID", all.x = TRUE)
geom$is_existing_easement <- geom$PID %in% existing_easement_pids

# --- Dayton zoning -------------------------------------------------------
zoning_sf <- sf::st_read(file.path(proj_root, "DaytonZoning/DaytonZoning.shp"),
                         quiet = TRUE)
zoning_sf <- polygonize(zoning_sf)
zoning_sf <- sf::st_transform(zoning_sf, 4326)
stopifnot(all(sf::st_geometry_type(zoning_sf) %in% c("POLYGON", "MULTIPOLYGON")))
zoning_density <- fread(file.path(proj_root, "data/dayton_zoning_density.csv"))
zone_col <- if ("ZONING" %in% names(zoning_sf)) "ZONING" else "Zoning"
zoning_sf$zone_code <- zoning_sf[[zone_col]]
zoning_sf <- merge(zoning_sf[, "zone_code"], zoning_density, by = "zone_code",
                   all.x = TRUE)
zoning_sf$category[is.na(zoning_sf$category)] <- "non_residential"

# Finer display categories (split "non_residential" into parks/commercial/industrial)
display_cat <- function(code) {
  dplyr::case_when(
    code %in% c("S-A", "A-1")                       ~ "Agricultural",
    code %in% c("A-2", "A-3")                       ~ "Agricultural (Transitional)",
    code %in% c("R-E", "R-2")                       ~ "Residential (Low Density)",
    code %in% c("R-1", "R-1A", "R-3", "R-O")        ~ "Residential (Medium Density)",
    code %in% c("R-M", "RH", "R-MH")                ~ "Residential (High Density)",
    code %in% c("V-M", "G-MU-4", "GMU-5")           ~ "Mixed Use",
    code %in% c("B-2", "B-3", "B-4", "B-P")         ~ "Commercial",
    code == "I-1"                                   ~ "Industrial",
    code == "P-R"                                   ~ "Parks / Recreation",
    code == "ES"                                    ~ "Essential Services",
    TRUE                                            ~ "Other"
  )
}
zoning_sf$display_category <- display_cat(zoning_sf$zone_code)

# Spatial-join zoning onto parcels (centroid in zone) for hover tooltip
suppressWarnings({
  cent <- sf::st_centroid(geom["PID"])
  joined <- sf::st_join(cent, zoning_sf[, c("zone_code", "zone_name", "display_category")],
                        join = sf::st_intersects)
})
zone_lookup <- as.data.table(sf::st_drop_geometry(joined))
setnames(zone_lookup, "PID", "PID")
zone_lookup <- zone_lookup[!duplicated(PID)]
geom_dt <- data.table(PID = geom$PID, row_idx = seq_len(nrow(geom)))
zone_lookup <- merge(geom_dt, zone_lookup, by = "PID", all.x = TRUE,
                     sort = FALSE)
setorder(zone_lookup, row_idx)
stopifnot(nrow(zone_lookup) == nrow(geom))
geom$zone_code        <- zone_lookup$zone_code
geom$zone_name        <- zone_lookup$zone_name
geom$display_category <- zone_lookup$display_category

# Protected land = parcels that are both P-R zoned AND tax-exempt
# (a P-R zone can overlap taxable parcels like private golf courses,
#  which shouldn't count as protected)
parks_sf <- geom[geom$display_category == "Parks / Recreation" & !geom$in_sim, ]
message("Protected parcels (P-R zoned + tax-exempt): ", nrow(parks_sf))

# --- Adjacency graph for connectivity scoring (cached) -----------------
adj_cache <- file.path(out_dir, "dayton_adjacency_cache.rds")
if (file.exists(adj_cache)) {
  message("Loading cached adjacency graph...")
  adj_data <- readRDS(adj_cache)
  adj <- adj_data$adj
  parcels_touching_parks <- adj_data$parcels_touching_parks
  parcels_touching_easements <- adj_data$parcels_touching_easements
  if (is.null(parcels_touching_easements)) parcels_touching_easements <- character(0)
} else {
  message("Building parcel adjacency graph (", nrow(geom), " parcels)...")
  geom_m <- sf::st_transform(geom, 26915)
  touches_list <- sf::st_is_within_distance(geom_m, geom_m, dist = 5)
  adj <- setNames(
    lapply(seq_along(touches_list), function(i) {
      setdiff(geom$PID[touches_list[[i]]], geom$PID[i])
    }),
    geom$PID
  )
  parks_m <- sf::st_transform(parks_sf, 26915)
  touches_parks <- lengths(sf::st_is_within_distance(geom_m, parks_m, dist = 5)) > 0
  parcels_touching_parks <- geom$PID[touches_parks]

  # Also anchor on existing-easement parcels (corridor can grow from them)
  ea_geom <- geom_m[geom$is_existing_easement, ]
  touches_ea <- if (nrow(ea_geom) > 0) {
    lengths(sf::st_is_within_distance(geom_m, ea_geom, dist = 5)) > 0
  } else rep(FALSE, nrow(geom_m))
  parcels_touching_easements <- geom$PID[touches_ea]

  saveRDS(list(adj = adj,
               parcels_touching_parks = parcels_touching_parks,
               parcels_touching_easements = parcels_touching_easements),
          adj_cache)
  message("Cached to ", adj_cache)
}

# --- Natural Systems Score denominators --------------------------------
# Score 1: total # of zonation-7 parcels in Dayton (denominator)
ZONATION_7_TOTAL_N <- sum(geom$priority_score == 7 & geom$in_sim, na.rm = TRUE)
message(sprintf("Zonation-7 total parcels (taxable): %d", ZONATION_7_TOTAL_N))

# Per-acre NTC by zoning display category — used for offset math in dashboard
offset_cats <- c("Residential (Low Density)", "Residential (Medium Density)",
                 "Residential (High Density)", "Commercial", "Mixed Use")
geom_dt <- as.data.table(sf::st_drop_geometry(geom))[in_sim == TRUE]
NTC_PER_ACRE <- sapply(offset_cats, function(cat) {
  rows <- geom_dt[display_category == cat & !is.na(tax_capacity) & acres > 0]
  if (nrow(rows) == 0) return(NA_real_)
  sum(rows$tax_capacity, na.rm = TRUE) / sum(rows$acres, na.rm = TRUE)
})
message("NTC per acre by category:")
print(round(NTC_PER_ACRE, 1))

# --- Development-candidate precompute (for suggested-offset layer) -------
# Candidates: currently developable land (ag / ag-trans / other), not protected,
# not already residential/commercial/industrial.
dev_candidate_cats <- c("Agricultural", "Agricultural (Transitional)", "Other")
is_vacant_pt <- grepl("vacant", geom$prop_type, ignore.case = TRUE)

# Geometric filters to drop undevelopable shapes:
#   (a) minimum 1 acre — no sliver lots
#   (b) Polsby-Popper compactness >= 0.25 — drops ROWs, drainage strips
geom_for_geom <- sf::st_transform(geom, 26915)
area_m2  <- as.numeric(sf::st_area(geom_for_geom))
perim_m  <- as.numeric(lwgeom::st_perimeter(geom_for_geom))
polsby   <- ifelse(perim_m > 0, 4 * pi * area_m2 / (perim_m^2), 0)

# Wetland filter: use the pre-cached wetland_fractions_2024.rds if present
# (built once by the main pipeline); otherwise compute on the fly from NWI.
wet_cache <- file.path(proj_root, "data/wetland_fractions_2024.rds")
wetland_path <- file.path(proj_root, "data/hennepin_wetland_merged.shp")
if (file.exists(wet_cache)) {
  message("Loading cached wetland fractions (wetland_fractions_2024.rds)...")
  wf <- readRDS(wet_cache)
  # wf is expected to be a data.frame/data.table with PID + wetland_fraction
  wf_dt <- as.data.table(wf)
  if ("PID" %in% names(wf_dt) && "wetland_fraction" %in% names(wf_dt)) {
    wf_lookup <- setNames(wf_dt$wetland_fraction, wf_dt$PID)
    wet_frac <- as.numeric(wf_lookup[geom$PID])
    wet_frac[is.na(wet_frac)] <- 0
  } else {
    message("wetland cache has unexpected columns — skipping.")
    wet_frac <- rep(0, nrow(geom))
  }
} else if (file.exists(wetland_path)) {
  message("Computing wetland overlap per parcel (no cache)...")
  wetlands <- sf::st_read(wetland_path, quiet = TRUE) |>
    sf::st_transform(26915) |> polygonize()
  wetland_union <- sf::st_union(wetlands)
  inter <- suppressWarnings(sf::st_intersection(geom_for_geom, wetland_union))
  wet_m2 <- rep(0, nrow(geom))
  if (nrow(inter) > 0) {
    match_idx <- match(inter$PID, geom$PID)
    wet_m2[match_idx] <- as.numeric(sf::st_area(inter))
  }
  wet_frac <- ifelse(area_m2 > 0, wet_m2 / area_m2, 0)
} else {
  message("No wetland data found — skipping wetland filter.")
  wet_frac <- rep(0, nrow(geom))
}
message(sprintf("Wetland coverage: %d parcels have >0 overlap; %d >25%%",
                sum(wet_frac > 0), sum(wet_frac > 0.25)))

# Parcels with near-zero NTC/acre are usually holding ponds, drainage
# easements, or other effectively-unusable land the assessor values at $1.
current_ntc_per_ac <- geom$tax_capacity / pmax(geom$acres, 0.01)
cand_idx <- which(
  geom$in_sim &
  (geom$display_category %in% dev_candidate_cats | is_vacant_pt) &
  !geom$PID %in% baseline_protected_pids &
  !is.na(geom$acres) & geom$acres >= 1 &              # min 1 acre
  polsby >= 0.25 &                                    # reasonable shape
  wet_frac < 0.25 &                                   # <25% wetland/water
  geom$priority_score < 5 &                           # not high conservation priority
  current_ntc_per_ac >= 50 &                          # not absurdly under-assessed
  !is.na(geom$tax_capacity))
message("Development candidates (after shape/wetland/min-area/zonation/NTC filters): ",
        length(cand_idx))

# Nearest residential parcel per candidate (medium or high density)
res_idx <- which(geom$display_category %in% c("Residential (Medium Density)",
                                                "Residential (High Density)"))
if (length(cand_idx) > 0 && length(res_idx) > 0) {
  cand_cents <- parcel_cent_m[cand_idx, ]
  res_cents  <- parcel_cent_m[res_idx, ]
  nn_d <- sf::st_distance(cand_cents, res_cents)
  nn_idx <- apply(as.matrix(nn_d), 1, which.min)
  nn_dist_m <- apply(as.matrix(nn_d), 1, min)
  nn_type <- geom$display_category[res_idx][nn_idx]
  nn_per_ac <- NTC_PER_ACRE[nn_type]
  cand_current_per_ac <- geom$tax_capacity[cand_idx] / geom$acres[cand_idx]
  cand_gain_per_ac <- pmax(0, nn_per_ac - cand_current_per_ac)
  cand_base_score  <- cand_gain_per_ac / (1 + nn_dist_m / 1000)  # distance in km

  # Infill bonus: vacant parcels get a 2x score boost so they float to the
  # top of the greedy pick before ag land is touched.
  cand_is_vacant <- grepl("vacant", geom$prop_type[cand_idx], ignore.case = TRUE)
  cand_score <- cand_base_score * ifelse(cand_is_vacant, 2, 1)

  cand_current_zone <- geom$display_category[cand_idx]
  cand_within_zoning <- grepl("^Residential", cand_current_zone)

  dev_candidates <- data.table(
    PID           = geom$PID[cand_idx],
    acres         = geom$acres[cand_idx],
    current_zone  = cand_current_zone,
    target_type   = nn_type,
    target_per_ac = as.numeric(nn_per_ac),
    gain_per_ac   = as.numeric(cand_gain_per_ac),
    dist_to_res_m = as.numeric(nn_dist_m),
    is_vacant     = cand_is_vacant,
    within_zoning = cand_within_zoning,
    score         = as.numeric(cand_score)
  )
  dev_candidates[, ntc_gain := gain_per_ac * acres]
  setorder(dev_candidates, -score)
} else {
  dev_candidates <- data.table()
}
message("Dev candidates computed: ", nrow(dev_candidates))

# Score 3: protected base = parks + existing easement parcels, dissolved so
# touching parcels become one "region" (Elm Creek's 80 sub-parcels → 1 region)
protected_parcels_sf <- rbind(
  sf::st_as_sf(parks_sf["PID"]),
  sf::st_as_sf(geom[geom$is_existing_easement, "PID"])
)
protected_parcels_m <- sf::st_transform(protected_parcels_sf, 26915)
# Dissolve: union everything, then split back into separate polygon pieces
pb_union     <- sf::st_union(protected_parcels_m)
pb_regions   <- sf::st_cast(pb_union, "POLYGON")
protected_base_m <- sf::st_sf(geometry = pb_regions)
message("Protected base set: ", nrow(protected_base_m), " region(s) (dissolved from ",
        nrow(protected_parcels_m), " parcels)")

# Baseline avg distance from each UNPROTECTED parcel centroid to nearest
# protected region. This metric is monotone: any added protected land brings
# its own parcel (distance → 0) and possibly neighbors closer to protection.
baseline_protected_pids <- unique(c(parks_sf$PID,
                                    geom$PID[geom$is_existing_easement]))
geom_m_all <- sf::st_transform(geom, 26915)
parcel_cent_m <- sf::st_centroid(geom_m_all)

is_protected_base <- geom$PID %in% baseline_protected_pids
unprotected_idx   <- which(!is_protected_base)
unprotected_cents <- parcel_cent_m[unprotected_idx, ]

if (nrow(protected_base_m) >= 1 && nrow(unprotected_cents) > 0) {
  D0 <- sf::st_distance(unprotected_cents, protected_base_m)
  baseline_min_dists_m <- apply(as.matrix(D0), 1, min)
  BASELINE_AVG_DIST_MILES <- mean(baseline_min_dists_m) / 1609.344
} else {
  BASELINE_AVG_DIST_MILES <- NA_real_
}
message(sprintf("Baseline avg distance from unprotected parcels to nearest protected: %.2f mi",
                BASELINE_AVG_DIST_MILES))

# --- Package and save ----------------------------------------------------
app_data <- list(
  city          = CITY,
  parcels       = parcels,
  geom          = geom,
  zoning        = zoning_sf,
  parks         = parks_sf,
  adj           = adj,
  parcels_touching_parks = parcels_touching_parks,
  parcels_touching_easements = parcels_touching_easements,
  existing_easement_pids     = existing_easement_pids,
  baseline_protected_pids    = baseline_protected_pids,
  zonation_7_total_n         = ZONATION_7_TOTAL_N,
  ntc_per_acre               = NTC_PER_ACRE,
  dev_candidates             = dev_candidates,
  protected_base_m           = protected_base_m,
  parcel_cent_m              = parcel_cent_m,
  baseline_avg_dist_miles    = BASELINE_AVG_DIST_MILES,
  baseline_ntc  = baseline_ntc,
  baseline_levy = baseline_levy,
  baseline_rate = baseline_rate
)

saveRDS(app_data, file.path(out_dir, "dayton_app_data.rds"))
message("Wrote ", file.path(out_dir, "dayton_app_data.rds"),
        " (", nrow(parcels), " subrecords, ", nrow(geom), " parcel geometries)")
