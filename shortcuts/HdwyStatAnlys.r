
# Packages ----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(stringr)
})

# Load worker_exposure_data from targets pipeline if not already in environment
if (!exists("worker_exposure_data")) {
  targets::tar_load(worker_exposure_data)
}

# Validate expected columns
required_cols <- c("site", "date", "time", "event")
missing_cols <- setdiff(required_cols, names(worker_exposure_data))
if (length(missing_cols) > 0) {
  stop("worker_exposure_data is missing columns: ", paste(missing_cols, collapse = ", "))
}

# 1) Parse timestamps (with milliseconds) and order ----
events <- worker_exposure_data %>%
  mutate(
    # Extract the HH:MM:SS part and the millisecond part
    sec_part = str_extract(time, "^\\d{2}:\\d{2}:\\d{2}"),
    ms_part  = str_extract(time, ":(\\d{1,3})$") %>% str_remove("^:"),

    # Normalize to "HH:MM:SS.mmm" and pad ms to 3 digits if needed
    time_norm = paste0(sec_part, ".", str_pad(ms_part, width = 3, side = "left", pad = "0")),

    # Combine date + normalized time; parse milliseconds via %OS
    ts = as.POSIXct(
      paste(date, time_norm),
      tz = "UTC",
      format = "%Y-%m-%d %H:%M:%OS"
    ),
    
    # Create a formatted display column with millisecond precision
    ts_display = paste(date, time_norm)
  ) %>%
  arrange(site, date, ts)

# 2) Compute headways (time between consecutive Vehicle Passing events) ----
vp <- events %>%
  filter(event == "Vehicle Passing") %>%
  group_by(site, date) %>%
  arrange(ts, .by_group = TRUE) %>%
  mutate(
    prev_vp_ts_numeric = lag(ts),
    prev_vp_ts_display = lag(ts_display),
    current_vp_ts_numeric = ts,
    current_vp_ts_display = ts_display,
    headway_s  = as.numeric(ts - prev_vp_ts_numeric, units = "secs")
  ) %>%
  ungroup()

# Pre-compute Waiting for Gap timestamps per site/date (as list column)
wfg_events <- events %>%
  filter(event == "Waiting for Gap") %>%
  group_by(site, date) %>%
  summarise(wfg_ts_vec = list(ts), .groups = "drop")

# Helper: For each current VP, find the most recent Waiting for Gap before it
# (strictly earlier than the current VP timestamp)
vp_wfg <- vp %>%
  left_join(wfg_events, by = c("site", "date")) %>%
  group_by(site, date) %>%
  arrange(current_vp_ts_numeric, .by_group = TRUE) %>%
  mutate(
    last_wfg_ts = {
      map2_dbl(current_vp_ts_numeric, wfg_ts_vec, ~ {
        if (is.null(.y) || length(.y) == 0) {
          NA_real_
        } else {
          prior_wfg <- .y[.y < .x]
          if (length(prior_wfg) == 0) NA_real_ else as.numeric(max(prior_wfg))
        }
      }) %>% as.POSIXct(origin = "1970-01-01", tz = "UTC")
    }
  ) %>%
  select(-wfg_ts_vec) %>%
  ungroup()

# Pre-compute Entering Roadway timestamps per site/date (as list column)
er_events <- events %>%
  filter(event == "Entering Roadway") %>%
  group_by(site, date) %>%
  summarise(er_ts_vec = list(ts), .groups = "drop")

# 3) Classify headways (Accepted/Rejected/NotTracked) ----
# Rules implemented:
# - Headways are between consecutive Vehicle Passing events (prev_vp_ts, ts).
# - If there is a last Waiting for Gap before current VP:
#     bound_start = max(prev_vp_ts, last_wfg_ts)
#     Accepted if any Entering Roadway occurs in (bound_start, ts); else Rejected.
# - If there is NO prior Waiting for Gap:
#     Accepted if any Entering Roadway occurs in (prev_vp_ts, ts);
#     else NotTracked (we keep the headway time but don't classify).
headways_all <- vp_wfg %>%
  left_join(er_events, by = c("site", "date")) %>%
  mutate(
    has_er = map_lgl(er_ts_vec, ~ !is.null(.x) && length(.x) > 0),
    # Define interval start depending on whether a WFG exists
    bound_start = case_when(
      !is.na(last_wfg_ts) & !is.na(prev_vp_ts_numeric) ~ as.POSIXct(pmax(as.numeric(prev_vp_ts_numeric),
                                                                 as.numeric(last_wfg_ts)),
                                                            origin = "1970-01-01", tz = "UTC"),
      !is.na(last_wfg_ts) &  is.na(prev_vp_ts_numeric) ~ last_wfg_ts,  # First VP after WFG (no couplet yet)
      is.na(last_wfg_ts)                        ~ prev_vp_ts_numeric   # No WFG: interval starts at previous VP
    ),
    # Check for ER strictly inside (bound_start, ts)
    er_in_interval = pmap_lgl(list(bound_start, current_vp_ts_numeric, er_ts_vec), ~ {
      if (is.null(..3) || length(..3) == 0) FALSE
      else any(..3 > ..1 & ..3 < ..2)
    }),
    headway_status = case_when(
      # No WFG prior: accept only if ER occurs between the VP pair; else NotTracked
      is.na(last_wfg_ts) & er_in_interval ~ "Accepted",
      is.na(last_wfg_ts) & !er_in_interval ~ "NotTracked",

      # WFG exists: accept if ER in (max(prev_vp_ts, last_wfg_ts), ts); else Rejected
      !is.na(last_wfg_ts) & er_in_interval ~ "Accepted",
      !is.na(last_wfg_ts) & !er_in_interval ~ "Rejected",

      TRUE ~ "NotTracked"
    )
  ) %>%
  select(
    site, date,
    prev_vp_ts = prev_vp_ts_display,
    current_vp_ts = current_vp_ts_display,
    last_wfg_ts,
    bound_start,
    headway_s,
    headway_status
  )

# Keep only real "Vehicle Passing" couplets (i.e., previous VP exists -> headway defined)
headways_couplets <- headways_all %>% filter(!is.na(prev_vp_ts))

# 4) Event combination summaries (pairs & triplets) ----
key_events <- c("Waiting for Gap", "Entering Roadway", "Exiting Roadway")

events_key <- events %>%
  filter(event %in% key_events) %>%
  group_by(site, date) %>%
  arrange(ts, .by_group = TRUE) %>%
  mutate(
    next_event1 = lead(event, 1),
    next_event2 = lead(event, 2),
    pair_label  = if_else(!is.na(next_event1),
                          paste(event, "→", next_event1),
                          NA_character_),
    triplet_label = if_else(!is.na(next_event2),
                            paste(event, "→", next_event1, "→", next_event2),
                            NA_character_)
  ) %>%
  ungroup()

event_pair_counts <- events_key %>%
  filter(!is.na(pair_label)) %>%
  count(pair_label, sort = TRUE, name = "n")

event_triplet_counts <- events_key %>%
  filter(!is.na(triplet_label)) %>%
  count(triplet_label, sort = TRUE, name = "n")

# 5) Example summaries you might want ----
headway_summary_by_site <- headways_couplets %>%
  group_by(site, headway_status) %>%
  summarise(
    n = n(),
    mean_headway_s = mean(headway_s, na.rm = TRUE),
    median_headway_s = median(headway_s, na.rm = TRUE),
    .groups = "drop"
  )

# 6) Critical headway components (Raff) ------------------------------------

# Use only couplets and only Accepted/Rejected classifications
gaps <- headways_couplets %>%
  filter(headway_status %in% c("Accepted", "Rejected")) %>%
  select(site, date, headway_s, headway_status)

# Helper to compute t1, t2, A(t1), R(t1), A(t2), R(t2), and t_c for a group
compute_raff_metrics <- function(df) {
  acc <- df %>% filter(headway_status == "Accepted") %>% pull(headway_s)
  rej <- df %>% filter(headway_status == "Rejected") %>% pull(headway_s)

  n_acc <- length(acc)
  n_rej <- length(rej)
  n_tot <- n_acc + n_rej

  # t1 = largest rejected; t2 = smallest accepted
  t1 <- if (n_rej > 0) max(rej, na.rm = TRUE) else NA_real_
  t2 <- if (n_acc > 0) min(acc, na.rm = TRUE) else NA_real_

  # A(t) and R(t) as proportions (0..1) within their own subsets
  A_t1 <- if (n_acc > 0 && !is.na(t1)) mean(acc <= t1) else NA_real_
  R_t1 <- if (n_rej > 0 && !is.na(t1)) mean(rej >= t1) else NA_real_
  A_t2 <- if (n_acc > 0 && !is.na(t2)) mean(acc <= t2) else NA_real_
  R_t2 <- if (n_rej > 0 && !is.na(t2)) mean(rej >= t2) else NA_real_

  # Overall acceptance/rejection rates for context
  A_overall <- if (n_tot > 0) n_acc / n_tot else NA_real_
  R_overall <- if (n_tot > 0) n_rej / n_tot else NA_real_

  # t_c per the provided formula:
  # t_c = ((t2 - t1) * [R(t1) - A(t1)]) / ([A(t2) - R(t2)] + [R(t1) - A(t1)]) + t1
  denom <- (A_t2 - R_t2) + (R_t1 - A_t1)
  tc <- if (is.na(t1) || is.na(t2) || is.na(denom) || denom == 0) NA_real_ else {
    ((t2 - t1) * (R_t1 - A_t1)) / denom + t1
  }

  tibble::tibble(
    n_accepted = n_acc,
    n_rejected = n_rej,
    t1_largest_rejected_s = t1,
    t2_smallest_accepted_s = t2,
    A_t1 = A_t1, R_t1 = R_t1,
    A_t2 = A_t2, R_t2 = R_t2,
    A_overall = A_overall,   # overall % accepted (0..1)
    R_overall = R_overall,   # overall % rejected (0..1)
    t_c_critical_s = tc
  )
}

# Compute metrics overall (all sites/dates combined)
raff_overall <- compute_raff_metrics(gaps)

# Compute metrics by site (you can also do by site+date if desired)
raff_by_site <- gaps %>%
  group_by(site) %>%
  group_modify(~ compute_raff_metrics(.x)) %>%
  ungroup()

# Optional: compute by site+date
raff_by_site_date <- gaps %>%
  group_by(site, date) %>%
  group_modify(~ compute_raff_metrics(.x)) %>%
  ungroup()

# Output Previews (open in tabs) ----
utils::View(headways_all,      title = "Headways: ALL Vehicle Passing rows")
raff_overall_col <- raff_overall %>%
  tidyr::pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(
    # try to coerce to numeric and round to 3 decimals when possible
    value_num = suppressWarnings(as.numeric(value)),
    value = ifelse(!is.na(value_num), format(round(value_num, 3), nsmall = 3), as.character(value))
  ) %>%
  select(metric, value)

utils::View(raff_overall_col, title = "Critical headway (Raff): OVERALL")

#export raff_overall_col to CSV titled Critical headway (Raff): OVERALL to box drive
readr::write_csv(raff_overall_col, "/Users/benjaminhailstone/Library/CloudStorage/Box-Box/2024-tprs/output/Critical_headway_Raff_OVERALL.csv")

#Note: We have a critical time! Now, we will compare that t_c value against each site/day distribution.
# We're taking a value representing the overall behavior of worker headway acceptance/rejection, 
# and comparing it against the distributions of headways for each site/day.

#utils::View(headways_couplets, title = "Headways: Vehicle Passing couplets only")
#utils::View(headway_summary_by_site, title = "Summary by site & status")
#utils::View(event_pair_counts, title = "Event pair counts")
#utils::View(event_triplet_counts, title = "Event triplet counts")
#utils::View(raff_overall,       title = "Critical headway (Raff): OVERALL")
#utils::View(raff_by_site,       title = "Critical headway (Raff): by SITE")
#utils::View(raff_by_site_date,  title = "Critical headway (Raff): by SITE + DATE")
