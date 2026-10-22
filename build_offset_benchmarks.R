# One-time build: peer-city tax capacity per acre for development types Dayton has
# too few of to benchmark locally (multifamily, mixed use). Feeds the "Built
# property type" view of the app's densification offset rows (apartments, mixed use).
#
# Input:  cache/subrecord_data.rds        2024 county-wide assessor subrecords (data_clean.R)
#         data/Hennepin Cities(Report).csv CITY_ID -> city name
#         data/fgdb_plan_parcels_open/plan_parcels_open.gdb  Met Council regional parcels (plat names)
# Output: dayton_easement_app/offset_benchmarks_peers.csv
#         dayton_easement_app/dayton_parcel_plats.csv   PID -> plat name, Dayton
#         dayton_easement_app/dayton_parcel_codes.csv   PID -> raw assessor PROPERTY_TYPE codes, Dayton
#         (used to group townhome / subdivision common lots with their development)
# Run:    Rscript dayton_easement_app/build_offset_benchmarks.R   (from project root)
#
# Kept out of data_prep.R because that script loads the full pipeline checkpoint
# at the top; this only needs the small subrecord cache.

library(data.table)

proj_root <- if (file.exists("cache/subrecord_data.rds")) "." else ".."
out_file  <- file.path(proj_root, "dayton_easement_app", "offset_benchmarks_peers.csv")

# Load subrecords ----
subrecords_2024 <- readRDS(file.path(proj_root, "cache", "subrecord_data.rds"))[TAX_YEAR == 2024]
subrecords_2024[, PROPERTY_TYPE := trimws(as.character(PROPERTY_TYPE))]

city_names <- fread(file.path(proj_root, "data", "Hennepin Cities(Report).csv"), select = c("CITY_ID", "NAME"))
mpls_id    <- as.numeric(city_names[NAME == "Minneapolis", CITY_ID])

# Outer-ring Hennepin cities = Met Council ThriveMSP 2040 community designation
# "Suburban Edge" or "Emerging Suburban Edge" (COMDESNAME), from the Council's
# LPH/LandUse MapServer layer 0, queried Oct 2026. Hand-copied list; re-query
# if designations are updated (Imagine 2050 replaces Thrive 2040).
outer_ring_cities <- c("Chanhassen", "Corcoran", "Dayton", "Greenfield", "Independence",
                       "Maple Grove", "Medina", "Minnetrista", "Orono", "Plymouth", "Rogers")
stopifnot(all(outer_ring_cities %in% city_names$NAME))
outer_ring_ids <- as.numeric(city_names[NAME %in% outer_ring_cities, CITY_ID])

# Benchmark peers (decision, Oct 2026): Plymouth and Maple Grove are dropped.
# They hold 85% of outer-ring multifamily acres and are larger, more built-out
# suburbs than Dayton. Mixed use uses only Rogers and Orono: small main-street
# and suburban apartment-over-commercial buildings, closest to Dayton's village.
mf_peer_cities <- setdiff(outer_ring_cities, c("Plymouth", "Maple Grove"))
mu_peer_cities <- c("Rogers", "Orono")
mf_peer_ids    <- as.numeric(city_names[NAME %in% mf_peer_cities, CITY_ID])
mu_peer_ids    <- as.numeric(city_names[NAME %in% mu_peer_cities, CITY_ID])

# Property type codes (main.R SUPER_MAP). Low-income housing (HL/HM/HR/HS) is
# excluded from both benchmarks by decision.
multifamily_codes <- c("DB", "TP", "A", "AX", "X", "XC")
apartment_codes   <- c("A", "AX")
commercial_codes  <- c("C", "LC", "SL")
residential_codes <- c("R", "RL", "RZ", "Y", "XM", "RM")

## Aggregate to PID ----
# Tax capacity sums across subrecords; ACREAGE is the full parcel repeated on
# every subrecord, so take the mean rather than the sum.
pid_df <- subrecords_2024[, .(
  lntc        = sum(LOCAL_NET_TAX_CAPACITY, na.rm = TRUE),
  acres       = mean(ACREAGE, na.rm = TRUE),
  city_id     = CITY_ID[1],
  only_mf     = all(PROPERTY_TYPE %in% multifamily_codes),
  only_apt    = all(PROPERTY_TYPE %in% apartment_codes),
  has_com     = any(PROPERTY_TYPE %in% commercial_codes),
  has_apt     = any(PROPERTY_TYPE %in% apartment_codes),
  has_res     = any(PROPERTY_TYPE %in% residential_codes)
), by = PID]

## Benchmark sample ----
# Minneapolis dropped: Dayton is outer-ring, and Minneapolis land values and
# towers don't transfer. Zero/NA-acre PIDs dropped: these are condo units whose
# land sits in a shared parcel, so they have no per-acre value.
benchmark_sample <- pid_df[city_id != mpls_id & acres > 0 & !is.na(acres)]
print(pid_df[, .(pids = .N, minneapolis = sum(city_id == mpls_id),
                 zero_acre_outside_mpls = sum(city_id != mpls_id & !(acres > 0)))])

summarise_benchmark <- function(sample_df, category, definition,
                                geography = "Hennepin County excl. Minneapolis") {
  sample_df[, .(category   = category,
                geography  = geography,
                parcels    = .N,
                acres      = sum(acres),
                ntc_per_ac = sum(lntc) / sum(acres),
                definition = definition,
                source     = "cache/subrecord_data.rds (2024 assessor subrecords)")]
}

offset_benchmarks <- rbind(
  # Apartments only (decision, Oct 2026): two-unit / triplex behave like
  # residential and Hennepin names them residential; condos have no acreage.
  summarise_benchmark(benchmark_sample[only_apt == TRUE & city_id %in% mf_peer_ids], "Apartments",
                      "All subrecords apartment (A, AX)",
                      geography = paste("Outer-ring peers:", paste(mf_peer_cities, collapse = ", "))),
  # Mixed use = commercial + apartment on one PID (retail with units above).
  # Commercial + single-family house is more often a home business or rural shop.
  # Peers only: the excl.-Minneapolis sample (~$109k/ac) is dominated by
  # first-ring podium buildings (Edina, St. Louis Park, Bloomington).
  summarise_benchmark(benchmark_sample[has_com & has_apt & city_id %in% mu_peer_ids], "Mixed use",
                      "At least one commercial (C, LC, SL) and one apartment (A, AX) subrecord on the PID",
                      geography = paste("Peers:", paste(mu_peer_cities, collapse = ", ")))
)

# For comparison only (not written): wider geographies
print(summarise_benchmark(benchmark_sample[has_com & has_apt], "Mixed use, excl. Minneapolis", "")[, .(category, parcels, acres, ntc_per_ac)])
print(summarise_benchmark(benchmark_sample[only_mf == TRUE & city_id %in% mf_peer_ids], "All multifamily codes, peers", "")[, .(category, parcels, acres, ntc_per_ac)])
print(summarise_benchmark(benchmark_sample[only_mf == TRUE & city_id %in% outer_ring_ids], "Multifamily, full outer ring", "")[, .(category, parcels, acres, ntc_per_ac)])
print(summarise_benchmark(benchmark_sample[only_mf == TRUE], "Multifamily, excl. Minneapolis", "")[, .(category, parcels, acres, ntc_per_ac)])

# Looser mixed-use definition, printed for comparison only (not written)
print(summarise_benchmark(benchmark_sample[has_com & (has_apt | has_res)], "Mixed use (loose)",
                          "commercial + apartment or single-family")[, .(category, parcels, acres, ntc_per_ac)])
print(offset_benchmarks[, .(category, parcels, acres = round(acres), ntc_per_ac = round(ntc_per_ac))])

fwrite(offset_benchmarks, out_file)

# Dayton raw property type codes ----
# The app's RDS only carries grouped types (main.R SUPER_MAP), which fold
# townhouse (Y) into residential and leave common area (K) unmapped. The app
# rebuilds built types from these raw codes instead.
dayton_id    <- as.numeric(city_names[NAME == "Dayton", CITY_ID])
dayton_codes <- subrecords_2024[CITY_ID == dayton_id,
                                .(codes = paste(sort(unique(PROPERTY_TYPE)), collapse = ",")), by = PID]
print(dayton_codes[, .(pids = .N, multi_code = sum(grepl(",", codes)))])
fwrite(dayton_codes, file.path(proj_root, "dayton_easement_app", "dayton_parcel_codes.csv"))

# Dayton plat names ----
# DaytonZoning.shp also carries PLAT_NAME, but only ~12% of its non-zoning
# attributes belong to their own PID (mismatched spatial join), so plat names
# come from the regional parcel file instead. Slow: GDAL scans the whole GDB.
library(sf)
plats_out <- file.path(proj_root, "dayton_easement_app", "dayton_parcel_plats.csv")
dayton_plats <- st_read(file.path(proj_root, "data", "fgdb_plan_parcels_open", "plan_parcels_open.gdb"),
  query = "SELECT county_pin, plat_name FROM plan_parcels_open WHERE co_name = 'Hennepin' AND ctu_name = 'Dayton'",
  quiet = TRUE)
dayton_plats <- as.data.table(st_drop_geometry(dayton_plats))
dayton_plats[, PID := trimws(as.character(county_pin))]
dayton_plats[nchar(PID) == 12, PID := paste0("0", PID)]   # restore the leading zero
dayton_plats <- unique(dayton_plats[, .(PID, plat_name)])
stopifnot(!anyDuplicated(dayton_plats$PID))
print(dayton_plats[, .(pids = .N, with_plat = sum(!is.na(plat_name)))])
fwrite(dayton_plats, plats_out)
