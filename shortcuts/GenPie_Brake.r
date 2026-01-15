
# Faceted brake pies (one pie per vehicle class), showing within-class percentages
suppressPackageStartupMessages({
  library(tidyverse)
  library(svglite)
  library(cowplot)
  library(scales)
})

# ==== CONFIG ====
out_dir <- "/Users/benjaminhailstone/Library/CloudStorage/Box-Box/2024-tprs/output/brake_pie_charts_faceted"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Validate `cb`
if (!exists("cb")) {
  stop("Object `cb` not found. Make sure `cb <- tar_read(camera_back_data)` has been run and is in the environment.")
}
req_cols <- c("class", "site", "date")
missing <- setdiff(req_cols, names(cb))
if (length(missing) > 0) {
  stop("Missing required columns in `cb`: ", paste(missing, collapse = ", "))
}

# Detect brake column (expects values like 'before', 'no brake', 'after')
candidate_cols <- names(cb)
detect_brake_col <- function(df, cols) {
  for (nm in cols) {
    vals <- tolower(as.character(df[[nm]]))
    has_before    <- any(grepl("^\\s*before\\s*$",    vals))
    has_no_brake  <- any(grepl("^\\s*no\\s+brake\\s*$", vals))
    has_after     <- any(grepl("^\\s*after\\s*$",     vals))
    if (has_before || has_no_brake || has_after) return(nm)
  }
  NA_character_
}
brake_col <- detect_brake_col(cb, candidate_cols)
if (is.na(brake_col)) {
  stop("Could not auto-detect the brake column in `cb`. Expected values include 'before', 'no brake', and 'after' (case-insensitive).")
}

# Normalize and filter to expected values
valid_sites <- c("sr12", "us6", "i70", "us191")

format_date_vec <- function(x) {
  # Return YYYY-MM-DD strings wherever possible
  if (inherits(x, "Date")) {
    format(x, "%Y-%m-%d")
  } else if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    format(as.Date(x), "%Y-%m-%d")
  } else {
    # try parsing, otherwise return as character
    dx <- suppressWarnings(as.Date(x))
    ifelse(!is.na(dx), format(dx, "%Y-%m-%d"), as.character(x))
  }
}

cb_clean <- cb %>%
  mutate(
    class_raw = tolower(.data[["class"]]),
    # Standardize 'motercycle' -> 'motorcycle'
    class = case_when(
      class_raw %in% c("motorcycle", "motercycle") ~ "motorcycle",
      class_raw %in% c("passenger", "truck") ~ class_raw,
      TRUE ~ class_raw
    ),
    site     = tolower(as.character(.data[["site"]])),
    brake    = tolower(as.character(.data[[brake_col]])),
    date_str = format_date_vec(.data[["date"]])
  ) %>%
  filter(
    class %in% c("passenger", "motorcycle", "truck"),
    brake %in% c("before", "no brake", "after"),
    site %in% valid_sites
  )

# Levels & palette
class_levels <- c("passenger", "motorcycle", "truck")
brake_levels <- c("before", "no brake", "after")

pal <- c(
  "before"   = "#2C7FB8",  # blue
  "no brake" = "#66C2A4",  # green
  "after"    = "#D7301F"   # red
)

# ---- Utilities ----

nice_class <- function(x) str_to_title(x)
nice_brake <- function(x) x %>% str_replace_all("_", " ") %>% str_to_title()

# Summarize subset: counts & within-class percentages
summarize_subset_classwise <- function(df_sub) {
  if (nrow(df_sub) == 0) return(NULL)

  # Complete grid to keep legend/table stable
  df <- df_sub %>%
    mutate(
      class = factor(class, levels = class_levels),
      brake = factor(brake, levels = brake_levels)
    ) %>%
    count(class, brake, name = "n") %>%
    complete(class = factor(class_levels, levels = class_levels),
             brake = factor(brake_levels, levels = brake_levels),
             fill = list(n = 0)) %>%
    group_by(class) %>%
    mutate(
      total_class = sum(n),
      perc_class  = ifelse(total_class > 0, n / total_class, 0)
    ) %>%
    ungroup()

  df
}

# Build faceted pies (one pie per class), no slice labels; legend shows brake categories only
make_faceted_pies <- function(df_sum, title, subtitle, caption) {
  if (is.null(df_sum) || nrow(df_sum) == 0) {
    return(NULL)
  }

  # Only plot classes that have at least one observation
  df_plot <- df_sum %>%
    group_by(class) %>%
    filter(sum(n) > 0) %>%
    ungroup()

  if (nrow(df_plot) == 0) return(NULL)

  p <- ggplot(df_plot, aes(x = 1, y = perc_class, fill = brake)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    facet_wrap(~ class, nrow = 1, labeller = labeller(class = function(x) str_to_title(x))) +
    scale_fill_manual(
      values = pal,
      breaks = brake_levels,
      labels = c("Before", "No Brake", "After"),
      name   = "Brake"
    ) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    # Force radius to 1 so every facet is a full pie:
    ylim(c(0, 1)) +
    theme_void(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold"),
      strip.text       = element_text(face = "bold"),
      legend.position  = "right",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.margin      = margin(10, 10, 10, 10)
    )

  p
}

# Companion table (SVG + CSV) with within-class percentages & counts
make_table_plot <- function(df_sum, title, subtitle, caption) {
  if (is.null(df_sum) || nrow(df_sum) == 0) return(NULL)

  tbl <- df_sum %>%
    transmute(
      Class   = nice_class(as.character(class)),
      Brake   = nice_brake(as.character(brake)),
      Percent = scales::percent(perc_class, accuracy = 0.1),
      Count   = n
    ) %>%
    arrange(factor(Class, levels = c("Passenger", "Motorcycle", "Truck")),
            factor(Brake, levels = c("Before", "No Brake", "After")))

  n_rows <- nrow(tbl)
  tbl_plot <- tbl %>% mutate(row = rev(seq_len(n_rows)))  # 1..n from bottom to top

  # Column positions (0..1)
  x_class   <- 0.02
  x_brake   <- 0.40
  x_percent <- 0.75
  x_count   <- 0.98
  y_header  <- n_rows + 1  # place header one unit above top row

  p_tbl <- ggplot(tbl_plot, aes(y = row)) +
    annotate("text", x = x_class,   y = y_header, label = "Class",   hjust = 0, fontface = "bold", size = 4) +
    annotate("text", x = x_brake,   y = y_header, label = "Brake",   hjust = 0, fontface = "bold", size = 4) +
    annotate("text", x = x_percent, y = y_header, label = "Percent", hjust = 1, fontface = "bold", size = 4) +
    annotate("text", x = x_count,   y = y_header, label = "Count",   hjust = 1, fontface = "bold", size = 4) +

    geom_text(aes(x = x_class,   label = Class),   hjust = 0, size = 3.8) +
    geom_text(aes(x = x_brake,   label = Brake),   hjust = 0, size = 3.8) +
    geom_text(aes(x = x_percent, label = Percent), hjust = 1, size = 3.8) +
    geom_text(aes(x = x_count,   label = Count),   hjust = 1, size = 3.8) +

    # Optional faint row separators to aid readability
    geom_segment(data = tibble(y = seq_len(n_rows)),
                 aes(x = 0.02, xend = 0.98, y = y, yend = y),
                 inherit.aes = FALSE, color = "grey90", size = 0.3) +

    labs(title = "Brake Event Distribution — Percentages by Class",
         subtitle = subtitle, caption = caption) +
    xlim(0, 1) +
    scale_y_continuous(limits = c(0, y_header + 1), expand = c(0.02, 0.02)) +
    theme_void(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold"),
      legend.position  = "none",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.margin      = margin(15, 20, 15, 20)
    )

  p_tbl
}


# Helper: save combined pie + table into single SVG
save_combined_svg <- function(file, p_top = NULL, p_bottom = NULL, width = 12, height = 10, top_frac = 0.5) {
  if (is.null(p_top) && is.null(p_bottom)) {
    message("Skipping (no plots to save): ", file)
    return(invisible(NULL))
  }

  # If one of them is NULL, just save the other
  if (is.null(p_top)) combined <- p_bottom
  else if (is.null(p_bottom)) combined <- p_top
  else {
    rel_heights <- c(top_frac, 1 - top_frac)
    combined <- cowplot::plot_grid(p_top, p_bottom, ncol = 1, rel_heights = rel_heights)
  }

  ggplot2::ggsave(file, plot = combined, device = svglite::svglite,
                  width = width, height = height, bg = "white")
  message("Saved combined SVG: ", file)
}

# ==== RENDERING ====

overall_caption <- paste0("Source: cb | Brake column: `", brake_col, "`")

# 1) Overall
df_overall <- summarize_subset_classwise(cb_clean)

combined_overall <- file.path(out_dir, "brake_faceted__overall.svg")

p_overall <- make_faceted_pies(
  df_overall,
  title    = "Brake Event Distribution Within Each Vehicle Class",
  subtitle = "Overall (three pies — Passenger, Motorcycle, Truck)",
  caption  = overall_caption
)
tbl_overall <- make_table_plot(
  df_overall,
  title    = "Brake Event Distribution — Percentages by Class",
  subtitle = "Overall",
  caption  = overall_caption
)

save_combined_svg(combined_overall, p_overall, tbl_overall, width = 10, height = 7, top_frac = 0.5)

# 2) Per site
for (s in sort(unique(cb_clean$site))) {
  site_df <- cb_clean %>% filter(site == s)
  df_site <- summarize_subset_classwise(site_df)

  combined_site <- file.path(out_dir, paste0("brake_faceted__site_", s, ".svg"))

  p_site <- make_faceted_pies(
    df_site,
    title    = "Brake Event Distribution Within Each Vehicle Class",
    subtitle = paste0("Site: ", toupper(s)),
    caption  = overall_caption
  )
  tbl_site <- make_table_plot(
    df_site,
    title    = "Brake Event Distribution — Percentages by Class",
    subtitle = paste0("Site: ", toupper(s)),
    caption  = overall_caption
  )

  save_combined_svg(combined_site, p_site, tbl_site, width = 10, height = 7, top_frac = 0.5)
}

# 3) Per date × site
for (s in sort(unique(cb_clean$site))) {
  site_dates <- cb_clean %>%
    filter(site == s) %>%
    distinct(date_str) %>%
    pull(date_str)

  for (d in sort(site_dates)) {
    df_sd <- cb_clean %>% filter(site == s, date_str == d)
    df_sum <- summarize_subset_classwise(df_sd)
    safe_d <- gsub("[^0-9A-Za-z_-]", "_", d)

    combined_sd <- file.path(out_dir, paste0("brake_faceted__date_", safe_d, "__site_", s, ".svg"))

    p_sd <- make_faceted_pies(
      df_sum,
      title    = "Brake Event Distribution Within Each Vehicle Class",
      subtitle = paste0("Date: ", d, " | Site: ", toupper(s)),
      caption  = overall_caption
    )
    tbl_sd <- make_table_plot(
      df_sum,
      title    = "Brake Event Distribution — Percentages by Class",
      subtitle = paste0("Date: ", d, " | Site: ", toupper(s)),
      caption  = overall_caption
    )

    save_combined_svg(combined_sd, p_sd, tbl_sd, width = 10, height = 7, top_frac = 0.5)
  }
}