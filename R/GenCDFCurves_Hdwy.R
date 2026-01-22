
# Dependencies (explicitly load tidyverse parts used)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(svglite)   # for ggsave(..., device = svglite)
library(readr)     # for reading observation_data.csv
library(patchwork) # for combining plots

## Load camera_back data from targets
targets::tar_load(camera_back_data)
cb <- camera_back_data

# Configure output directory (portable across machines)
# Use relative path from project root
out_dir <- "output"
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
       "Then run this script to create CDFs with the critical headway line.\n")
}

t_c_critical <- raff_overall$t_c_critical_s
cat("✓ Using critical headway t_c =", round(t_c_critical, 2), "seconds\n")

# Load observation data to get spacing_type information
obs_data <- read_csv("data/observation_data.csv", show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    spacing_type = trimws(spacing_type)  # Remove any whitespace
  )

# Inspect input
str(cb)
head(cb)

# --- 1) Compute headways and join with spacing_type -------------------------
cb_processed <- cb %>%
  group_by(site, date) %>%
  arrange(time, .by_group = TRUE) %>%
  # Use difftime for clarity; returns seconds as numeric
  mutate(headway = as.numeric(difftime(time, lag(time), units = "secs"))) %>%
  ungroup() %>%
  # Join with observation data to get spacing_type
  left_join(obs_data %>% select(date, site, spacing_type), by = c("date", "site"))

# --- 2) Function to create CDF plot ------------------------------------------
create_cdf_plot <- function(data, group_name, group_type = "site") {
  # X-axis limit rule
  x_limit <- if (grepl("us191", group_name, ignore.case = TRUE)) 500 else 200
  
  # Filter out NAs and calculate CDF
  clean_data <- data %>%
    filter(!is.na(headway)) %>%
    arrange(headway)
  
  n <- nrow(clean_data)
  
  if (n == 0) {
    cat("\n⚠ Warning: No data for group", group_name, "- skipping\n")
    return(NULL)
  }
  
  # Create empirical CDF
  clean_data <- clean_data %>%
    mutate(
      cumulative_prob = (row_number()) / n
    )
  
  # Calculate stats
  mean_headway <- mean(clean_data$headway, na.rm = TRUE)
  sd_headway <- sd(clean_data$headway, na.rm = TRUE)
  
  # Calculate CDF value at t_c (for horizontal line)
  cdf_at_tc <- clean_data %>%
    filter(headway <= t_c_critical) %>%
    summarise(cdf = max(cumulative_prob, na.rm = TRUE)) %>%
    pull(cdf)
  
  if (length(cdf_at_tc) == 0) cdf_at_tc <- 0
  
  # Create CDF plot with shaded area
  plot_data <- clean_data %>% filter(headway <= x_limit)
  
  p_cdf <- plot_data %>%
    ggplot(aes(x = headway, y = cumulative_prob)) +
    geom_ribbon(aes(ymin = 0, ymax = cumulative_prob), fill = "steelblue", alpha = 0.5) +
    geom_step(color = "steelblue", linewidth = 1) +
    scale_x_continuous(limits = c(0, x_limit)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(
      title = paste0("Cumulative Distribution Function - ", group_name),
      x = "Headway (seconds between cars)",
      y = "Cumulative Proportion"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
  
  # --- Stats annotation (bottom-right) ---
  fmt_val <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f s", x))
  stats_text <- paste0(
    "n = ", n, "\n",
    "mean = ", fmt_val(mean_headway), "\n",
    "sd = ", fmt_val(sd_headway)
  )
  
  p_cdf <- p_cdf +
    annotate(
      "label",
      x = x_limit * 0.95, y = 0.05,
      label = stats_text,
      vjust = 0, hjust = 1, size = 3.5,
      fill = "white", color = "black"
    )
  
  # --- Add critical headway horizontal line ---
  if (!is.na(t_c_critical) && !is.na(cdf_at_tc)) {
    p_cdf <- p_cdf +
      geom_hline(
        yintercept = cdf_at_tc,
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +
      annotate(
        "text",
        x = x_limit * 0.95,
        y = cdf_at_tc,
        label = sprintf("CDF at t_c = %.1f s\n(%.1f%%)", t_c_critical, cdf_at_tc * 100),
        vjust = -0.5,
        hjust = 1,
        color = "red",
        size = 3.5,
        fontface = "bold"
      )
  }
  
  return(p_cdf)
}

# --- 3) Create CDFs by SITE --------------------------------------------------
cat("\n\n=== Creating CDFs by SITE ===\n")

sites <- c("sr12", "us6", "i70", "us191")
site_plots <- list()

for (site_name in sites) {
  site_data <- cb_processed %>%
    filter(site == site_name)
  
  if (nrow(site_data) > 0) {
    p <- create_cdf_plot(site_data, site_name, "site")
    
    if (!is.null(p)) {
      # Save individual plot
      filename <- paste0("cdf_site_", site_name, ".svg")
      full_path <- file.path(out_dir, filename)
      ggsave(full_path, plot = p, device = svglite, width = 10, height = 6)
      cat("✓ Saved:", full_path, "\n")
      
      # Store for combined plot
      site_plots[[site_name]] <- p
    }
  }
}

# Create combined site plot
if (length(site_plots) > 0) {
  library(patchwork)
  combined_site <- wrap_plots(site_plots, ncol = 2, nrow = 2)
  combined_path <- file.path(out_dir, "cdf_sites_combined.svg")
  ggsave(combined_path, plot = combined_site, device = svglite, width = 16, height = 12)
  cat("✓ Saved combined site plot:", combined_path, "\n")
}

# --- 4) Create CDFs by SPACING_TYPE ------------------------------------------
cat("\n\n=== Creating CDFs by SPACING_TYPE ===\n")

spacing_types <- c("NO TPRS", "UDOT", "PSS", "LONG")
spacing_plots <- list()

for (spacing in spacing_types) {
  spacing_data <- cb_processed %>%
    filter(spacing_type == spacing)
  
  if (nrow(spacing_data) > 0) {
    # Create safe filename (replace spaces with underscores)
    safe_name <- gsub(" ", "_", spacing)
    p <- create_cdf_plot(spacing_data, spacing, "spacing_type")
    
    if (!is.null(p)) {
      # Save individual plot
      filename <- paste0("cdf_spacing_", safe_name, ".svg")
      full_path <- file.path(out_dir, filename)
      ggsave(full_path, plot = p, device = svglite, width = 10, height = 6)
      cat("✓ Saved:", full_path, "\n")
      
      # Store for combined plot
      spacing_plots[[spacing]] <- p
    }
  }
}

# Create combined spacing plot
if (length(spacing_plots) > 0) {
  combined_spacing <- wrap_plots(spacing_plots, ncol = 2, nrow = 2)
  combined_path <- file.path(out_dir, "cdf_spacing_combined.svg")
  ggsave(combined_path, plot = combined_spacing, device = svglite, width = 16, height = 12)
  cat("✓ Saved combined spacing plot:", combined_path, "\n")
}

cat("\n\n=== All CDFs saved to:", out_dir, "===\n")

# --- 5) Create summary statistics table --------------------------------------
summary_by_site <- cb_processed %>%
  group_by(site) %>%
  summarise(
    n = sum(!is.na(headway)),
    mean_headway = mean(headway, na.rm = TRUE),
    sd_headway = sd(headway, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group_type = "site") %>%
  rename(group_name = site)

summary_by_spacing <- cb_processed %>%
  group_by(spacing_type) %>%
  summarise(
    n = sum(!is.na(headway)),
    mean_headway = mean(headway, na.rm = TRUE),
    sd_headway = sd(headway, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group_type = "spacing_type") %>%
  rename(group_name = spacing_type)

summary_stats <- bind_rows(summary_by_site, summary_by_spacing) %>%
  arrange(group_type, group_name)

print(summary_stats)

# Safe View function that doesn't require X11
tryCatch({
  if (interactive() && capabilities("X11")) {
    utils::View(summary_stats)
  }
}, error = function(e) {
  cat("\n(View window not available - summary printed above)\n")
})

# --- 6) (Optional) Write stats to CSV for reporting --------------------------
# write_csv(summary_stats, "headway_cdf_stats.csv")
