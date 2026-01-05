
# -------------------------------------------
# Critical time (t_c) per site/date/sequence
# -------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

tar_make(worker_exposure_data)
we_data <- tar_read(worker_exposure_data)
utils::View(we_data)

# Check structure and first few rows
str(we_data)
head(we_data, 20)

# Check what events are in the data
table(we_data$event)

# Check the time column format
head(we_data$time)
class(we_data$time)

# ---- 0) Column name shim (edit here if your names differ) ----
# Time is character in "HH:MM:SS:mmm" format, date is already Date
time_col  <- "time"        # character format "HH:MM:SS:mmm"
site_col  <- "site"
date_col  <- "date"        # already a Date column
event_col <- "event"

# Harmonize into a working tibble
wd <- we_data %>%
  mutate(
    # Combine date + time: convert last colon to decimal point for milliseconds
    # "13:38:28:668" becomes "13:38:28.668" for sub-second precision
    ts = lubridate::ymd_hms(paste(date, gsub(":([0-9]{3})$", ".\\1", time)))
  ) %>%
  select(site, date, ts, event, everything())

# ---- 1) Split into sequences: new sequence when gap > 1 hour ----
wd_seq <- wd %>%
  arrange(site, date, ts) %>%
  group_by(site, date) %>%
  mutate(
    gap_s  = as.numeric(difftime(ts, dplyr::lag(ts), units = "secs")),
    seq_id = cumsum(is.na(gap_s) | gap_s > 3600)  # 1-hour threshold
  ) %>%
  ungroup()

# ---- 2) Compute headways between consecutive Vehicle Passing events within each sequence ----
# We'll do this per (site, date, seq_id) and keep it scoped to each group.
we_data_processed <- wd_seq %>%
  group_by(site, date, seq_id) %>%
  group_modify(~ {
    df <- .x %>% arrange(ts)

    # Vehicle Passing rows
    pass <- df %>%
      filter(event == "Vehicle Passing") %>%
      arrange(ts)

    # If fewer than 2 passings, no headways to compute
    if (nrow(pass) < 2) return(tibble())

    # Headways and interval endpoints
    pass <- pass %>%
      mutate(
        start_ts  = ts,
        end_ts    = dplyr::lead(ts),
        headway_s = as.numeric(difftime(end_ts, start_ts, units = "secs"))
      ) %>%
      filter(!is.na(headway_s))

    # ---- 3) Inspect events between start_ts and end_ts (within the same group only) ----
    # Acceptance rule: at least one Entering → Exiting occurs in order.
    count_patterns <- function(events_vec) {
      ev <- as.character(events_vec)
      n_enter <- sum(ev == "Entering Roadway")
      n_exit  <- sum(ev == "Exiting Roadway")
      has_wait <- any(ev == "Waiting for Gap")

      # Simple ordered check: does any Enter occur before any Exit?
      enter_idx <- which(ev == "Entering Roadway")
      exit_idx  <- which(ev == "Exiting Roadway")
      has_enter_exit_ordered <- length(enter_idx) > 0 && length(exit_idx) > 0 &&
        min(exit_idx) > min(enter_idx)

      list(
        has_enter = n_enter > 0,
        has_exit  = n_exit > 0,
        has_wait  = has_wait,
        has_enter_exit_ordered = has_enter_exit_ordered
      )
    }

    pass %>%
      rowwise() %>%
      mutate(
        events_between = list(
          df %>%
            filter(ts > start_ts, ts <= end_ts) %>%
            arrange(ts) %>%
            pull(event)
        ),
        patt = list(count_patterns(events_between)),
        has_entry = patt$has_enter,
        has_exit  = patt$has_exit,
        has_wait  = patt$has_wait,
        accepted  = patt$has_enter_exit_ordered,   # your acceptance criterion
      ) %>%
      ungroup() %>%
      select(start_ts, end_ts, headway_s,
             has_entry, has_exit, has_wait, accepted)
  }) %>%
  ungroup() %>%
  arrange(site, date, seq_id, headway_s) %>%
  group_by(site, date, seq_id) %>%
  mutate(
    # ---- 4) Critical time per sequence: shortest accepted headway ----
    critical_time_s = suppressWarnings(min(headway_s[accepted], na.rm = TRUE))
  ) %>%
  ungroup()

# ---- 5) Display results ----
utils::View(we_data_processed)

# Also show a one-row-per-sequence summary if you like:
tc_sum <- we_data_processed %>%
  group_by(site, date, seq_id) %>%
  summarise(
    t_c_seconds = suppressWarnings(min(headway_s[accepted], na.rm = TRUE)),
    .groups = "drop"
  )

utils::View(tc_sum)
