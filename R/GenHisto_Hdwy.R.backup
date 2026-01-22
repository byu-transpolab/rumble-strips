
# Dependencies (explicitly load tidyverse parts used)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(svglite)   # for ggsave(..., device = svglite)

# Optional: remove if not used
# library(patchwork)
# library(gridExtra)
# Configure output directory (portable across machines)
out_dir <- file.path(path.expand("~"), "Library/CloudStorage/Box-Box/2024-tprs/output/headway_histograms/")
cat("\n=== DIAGNOSTICS ===")
cat("\nConfigured output directory:", out_dir)
cat("\nDirectory exists before creation?", dir.exists(out_dir))
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  cat("\nDirectory created.")
}
cat("\nDirectory exists after creation?", dir.exists(out_dir))
cat("\nDirectory writable?", file.access(out_dir, mode = 2) == 0)
cat("\n===================\n")

# Load critical headway value from HdwyStatAnlys.r analysis
if (!exists("raff_overall")) {
  stop("\n❌ ERROR: 'raff_overall' not found in environment.\n",
       "Please run 'HdwyStatAnlys.r' first to generate the critical headway Raff table.\n",
       "Then run this script to create histograms with the critical headway line.\n")
}

t_c_critical <- raff_overall$t_c_critical_s
cat("✓ Using critical headway t_c =", round(t_c_critical, 2), "seconds\n")

# Inspect input (as in your original)
str(cb)
head(cb)

# --- 1) Compute headways & combined grouping --------------------------------
cb_processed <- cb %>%
  mutate(
    combined_group = case_when(
      site == "sr12" & date %in% as.Date(c("2025-07-07", "2025-07-11")) ~ "sr12_2025-07-07_and_11",
      TRUE ~ paste0(site, "_", date)
    )
  ) %>%
  group_by(site, date) %>%
  arrange(time, .by_group = TRUE) %>%
  # Use difftime for clarity; returns seconds as numeric
  mutate(headway = as.numeric(difftime(time, lag(time), units = "secs"))) %>%
  ungroup()

# --- 2) Build summary stats tibble (per histogram group) ---------------------
stats_tbl <- cb_processed %>%
  group_by(combined_group) %>%
  summarise(
    n = sum(!is.na(headway)),
    mean_headway = ifelse(n == 0, NA_real_, mean(headway, na.rm = TRUE)),
    sd_headway   = ifelse(n <= 1, NA_real_, sd(headway, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(combined_group)

# Preview stats tibble
print(stats_tbl)

# --- 3) Nest data and attach stats for plotting ------------------------------
groups_nested <- cb_processed %>%
  group_by(combined_group) %>%
  nest() %>%
  left_join(stats_tbl, by = "combined_group") %>%
  arrange(combined_group)

# --- 4) Create & save histograms (SVG) with stats and outliers ---------------
purrr::pwalk(groups_nested, function(combined_group, data, n, mean_headway, sd_headway) {
  filename <- paste0("headway_", combined_group, ".svg")

  # X-axis limit rule (kept from your script)
  x_limit <- if (grepl("us191", combined_group)) 500 else 200

  # Outliers (above x_limit)
  outliers <- data %>%
    filter(headway > x_limit) %>%
    select(headway) %>%
    arrange(desc(headway)) %>%
    mutate(headway = round(headway, 1))

  # Histogram + density
  p_hist <- data %>%
    ggplot(aes(x = headway)) +
    geom_histogram(
      binwidth = 2,
      fill = "steelblue",
      alpha = 0.7,
      aes(y = after_stat(density)),
      na.rm = TRUE
    ) +
    geom_density(color = "steelblue", linewidth = 1, na.rm = TRUE) +
    scale_x_continuous(limits = c(0, x_limit)) +
    labs(
      title = paste0("Headway Distribution - ", combined_group),
      x = "Headway (seconds between cars)",
      y = "Density"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))

  # --- Stats annotation (top-right) ---
  fmt_val <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f s", x))
  stats_text <- paste0(
    "n = ", n, "\n",
    "mean = ", fmt_val(mean_headway), "\n",
    "sd = ", fmt_val(sd_headway)
  )

  p_hist <- p_hist +
    annotate(
      "label",
      x = x_limit * 0.95, y = Inf,
      label = stats_text,
      vjust = 1.1, hjust = 1, size = 3.5,
      fill = "white", color = "black", label.size = 0.2
    )

  # --- Outlier annotation (top-right), if any ---
  if (nrow(outliers) > 0) {
    outlier_text <- paste(paste0(outliers$headway, " s"), collapse = "\n")
    full_text <- paste0("Outliers (>", x_limit, " s):\n", outlier_text)

    p_hist <- p_hist +
      annotate(
        "label",
        x = x_limit * 0.70, y = Inf,
        label = full_text,
        vjust = 1.1, hjust = 0.5, size = 3.5,
        fill = "white", color = "black", label.size = 0.2
      )
  }

  # --- Add critical headway vertical line ---
  if (!is.na(t_c_critical) && t_c_critical <= x_limit) {
    p_hist <- p_hist +
      geom_vline(
        xintercept = t_c_critical,
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +
      annotate(
        "text",
        x = t_c_critical,
        y = Inf,
        label = sprintf("Critical headway\nt_c = %.1f s", t_c_critical),
        vjust = 1.5,
        hjust = ifelse(t_c_critical < x_limit * 0.5, -0.1, 1.1),
        color = "red",
        size = 3.5,
        fontface = "bold"
      )
  }

  # Save SVG
  full_path <- file.path(out_dir, filename)
  ggsave(full_path, plot = p_hist, device = svglite, width = 10, height = 6)
  cat("\n✓ Saved:", full_path)
  cat("\n  File exists?", file.exists(full_path), "| Size:", file.size(full_path), "bytes")
})

cat("\n\n=== All histograms saved to:", out_dir, "===")

utils::View(stats_tbl)

# --- 5) (Optional) Write stats to CSV for reporting --------------------------
# readr::write_csv(stats_tbl, "headway_stats_by_group.csv")
