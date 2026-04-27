library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(DT)

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
}

metric_labels <- c(
  cmj_height = "CMJ Height (cm)",
  cmj_rsi = "CMJ RSI-modified (m/s)",
  con_imp = "CMJ Concentric Impulse (N s)",
  relative_pvf = "IMTP Relative PVF (N/kg)",
  jh_flight_time = "RJT Jump Height (cm)",
  rsi_jh_ct = "RJT RSI JH/CT (m/s)"
)

metric_test_map <- c(
  cmj_height = "CMJ",
  cmj_rsi = "CMJ",
  con_imp = "CMJ",
  relative_pvf = "IMTP",
  jh_flight_time = "RJT",
  rsi_jh_ct = "RJT"
)

pretty_metric <- function(x) {
  lbl <- unname(metric_labels[x])
  ifelse(is.na(lbl), x, lbl)
}

classify_t_score_band <- function(t_score) {
  case_when(
    is.na(t_score) ~ "No Group Data",
    t_score < 45 ~ "Below Group Average",
    t_score <= 55 ~ "Around Group Average",
    TRUE ~ "Above Group Average"
  )
}

classify_t_score_class <- function(t_score) {
  case_when(
    is.na(t_score) ~ "status-neutral",
    t_score < 45 ~ "status-low",
    t_score <= 55 ~ "status-amber",
    TRUE ~ "status-good"
  )
}

classify_trend_status <- function(slope_per_year, practical_change) {
  case_when(
    is.na(slope_per_year) ~ "Need Data",
    is.na(practical_change) ~ "Need Data",
    slope_per_year > practical_change ~ "Improving",
    slope_per_year < (-practical_change) ~ "Declining",
    TRUE ~ "Stable"
  )
}

trend_status_class <- function(trend_status) {
  case_when(
    trend_status == "Improving" ~ "status-good",
    trend_status == "Stable" ~ "status-amber",
    trend_status == "Declining" ~ "status-low",
    TRUE ~ "status-neutral"
  )
}

classify_maturity_status <- function(maturity_offset) {
  case_when(
    is.na(maturity_offset) ~ NA_character_,
    maturity_offset < -1 ~ "Pre-PHV",
    maturity_offset <= 1 ~ "Circa-PHV",
    TRUE ~ "Post-PHV"
  )
}

normalize_group_label <- function(group_label) {
  x <- trimws(as.character(group_label))
  x_lower <- tolower(x)

  case_when(
    is.na(group_label) ~ NA_character_,
    x_lower %in% c("", "na") ~ NA_character_,
    x_lower %in% c("u12", "u12s") ~ "u12s",
    x_lower %in% c("u14", "u14s") ~ "u14s",
    x_lower %in% c("u16", "u16s") ~ "u16s",
    TRUE ~ x
  )
}

dashboard_plot_theme <- function() {
  theme_minimal(base_size = 13, base_family = "Montserrat") +
    theme(
      plot.title = element_text(face = "bold", size = 14, color = "#1F2937"),
      plot.subtitle = element_text(color = "#4B5563"),
      axis.title = element_text(face = "bold", color = "#1F2937"),
      axis.text = element_text(color = "#4B5563"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E7EB")
    )
}

empty_progress_metric_data <- function() {
  tibble(
    assess_date = as.Date(character()),
    time_point = character(),
    athlete_age = numeric(),
    athlete_value = numeric(),
    athlete_t_score = numeric(),
    session_no = integer(),
    athlete_pct_from_first = numeric(),
    group_date = as.Date(character()),
    group_time_point = character(),
    group_age = numeric(),
    group_mean = numeric(),
    group_sd = numeric(),
    group_n = numeric(),
    group_gap_days = numeric(),
    group_pct_from_first = numeric(),
    diff_vs_group = numeric()
  )
}

match_by_nearest_date <- function(x_tbl, y_tbl, max_gap_days = Inf) {
  if (nrow(x_tbl) == 0 || nrow(y_tbl) == 0) {
    return(tibble())
  }

  x_tbl <- x_tbl |>
    transmute(
      x_date = as.Date(assess_date),
      x_value = as.numeric(value)
    ) |>
    arrange(x_date) |>
    mutate(x_id = row_number())

  y_tbl <- y_tbl |>
    transmute(
      y_date = as.Date(assess_date),
      y_value = as.numeric(value)
    ) |>
    arrange(y_date) |>
    mutate(y_id = row_number())

  candidates <- tidyr::crossing(x_tbl, y_tbl) |>
    mutate(date_gap_days = abs(as.numeric(x_date - y_date))) |>
    filter(date_gap_days <= max_gap_days) |>
    arrange(date_gap_days, x_date, y_date)

  if (nrow(candidates) == 0) {
    return(tibble())
  }

  used_x <- rep(FALSE, nrow(x_tbl))
  used_y <- rep(FALSE, nrow(y_tbl))
  keep <- logical(nrow(candidates))

  for (i in seq_len(nrow(candidates))) {
    xi <- candidates$x_id[i]
    yi <- candidates$y_id[i]
    if (!used_x[xi] && !used_y[yi]) {
      keep[i] <- TRUE
      used_x[xi] <- TRUE
      used_y[yi] <- TRUE
    }
  }

  matched <- candidates[keep, c("x_date", "x_value", "y_date", "y_value", "date_gap_days")]

  tibble(
    x_date = as.Date(matched$x_date),
    x_value = as.numeric(matched$x_value),
    y_date = as.Date(matched$y_date),
    y_value = as.numeric(matched$y_value),
    date_gap_days = as.numeric(matched$date_gap_days)
  ) |>
    arrange(x_date, y_date)
}

parse_mixed_date <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))
  is_num <- grepl("^[0-9]+([.][0-9]+)?$", x)

  out[is_num] <- as.Date(as.numeric(x[is_num]), origin = "1899-12-30")
  out[!is_num] <- suppressWarnings(
    parse_date_time(x[!is_num], orders = c("mdy", "dmy", "ymd")) |> as.Date()
  )
  out
}

compute_time_point <- function(date) {
  date <- as.Date(date)

  case_when(
    between(date, as.Date("2024-02-01"), as.Date("2024-02-28")) ~ "Feb 2024",
    between(date, as.Date("2024-04-03"), as.Date("2024-04-15")) ~ "April 2024",
    between(date, as.Date("2024-08-27"), as.Date("2024-08-30")) ~ "August 2024",
    between(date, as.Date("2025-04-15"), as.Date("2025-04-25")) ~ "April 2025",
    between(date, as.Date("2025-09-08"), as.Date("2025-09-10")) ~ "September 2025",
    between(date, as.Date("2026-01-01"), as.Date("2026-01-14")) ~ "Jan 2026",
    TRUE ~ NA_character_
  )
}

safe_read_test <- function(path, label) {
  if (!file.exists(path)) {
    return(tibble())
  }

  raw <- read_csv(path, show_col_types = FALSE)
  raw_date <- dmy(raw$Date)

  get_num <- function(df, col_name) {
    if (col_name %in% names(df)) {
      suppressWarnings(as.numeric(df[[col_name]]))
    } else {
      rep(NA_real_, nrow(df))
    }
  }

  out <- raw |>
    transmute(
      ID = Name,
      test = label,
      assess_date = raw_date,
      time_point = compute_time_point(raw_date),
      cmj_height = NA_real_,
      cmj_rsi = NA_real_,
      con_imp = NA_real_,
      relative_pvf = NA_real_,
      jh_flight_time = NA_real_,
      rsi_jh_ct = NA_real_
    )

  if (identical(label, "CMJ")) {
    out <- out |>
      mutate(
        cmj_height = get_num(raw, "Jump Height (Imp-Mom) [cm]"),
        cmj_rsi = get_num(raw, "RSI-modified (Imp-Mom) [m/s]"),
        con_imp = get_num(raw, "Concentric Impulse [N s]")
      )
  }

  if (identical(label, "IMTP")) {
    out <- out |>
      mutate(
        relative_pvf = get_num(raw, "Peak Vertical Force / BM [N/kg]")
      )
  }

  if (identical(label, "RJT")) {
    out <- out |>
      mutate(
        jh_flight_time = get_num(raw, "Mean Jump Height (Flight Time) [cm]"),
        rsi_jh_ct = get_num(raw, "Mean RSI (Jump Height/Contact Time) [m/s]")
      )
  }

  out
}

load_data <- function(data_dir) {
  data_dir <- if (is.null(data_dir) || !nzchar(data_dir)) "." else data_dir
  ids_path <- file.path(data_dir, "IDs-2.xlsx")
  stature_path <- file.path(data_dir, "Stature.xlsx")

  req_files <- c(
    ids_path,
    stature_path,
    file.path(data_dir, "CMJ-2.csv"),
    file.path(data_dir, "RJT-2.csv"),
    file.path(data_dir, "imtp-2.csv")
  )

  missing <- req_files[!file.exists(req_files)]
  if (length(missing) > 0) {
    stop(paste("Missing file(s):", paste(basename(missing), collapse = ", ")))
  }

  ids <- read_excel(ids_path) |>
    mutate(DOB_date = parse_mixed_date(DOB)) |>
    mutate(Group = normalize_group_label(Group)) |>
    select(ID, Group, DOB_date)

  stature <- read_excel(stature_path) |>
    transmute(
      ID,
      stature_date = as.Date(Date),
      Height,
      SH_raw,
      Mass
    )

  tests <- bind_rows(
    safe_read_test(file.path(data_dir, "CMJ-2.csv"), "CMJ"),
    safe_read_test(file.path(data_dir, "RJT-2.csv"), "RJT"),
    safe_read_test(file.path(data_dir, "imtp-2.csv"), "IMTP")
  ) |>
    mutate(row_id = row_number())

  nearest_stature <- tests |>
    select(row_id, ID, assess_date) |>
    left_join(stature, by = "ID", relationship = "many-to-many") |>
    mutate(day_gap = abs(as.numeric(assess_date - stature_date))) |>
    group_by(row_id) |>
    slice_min(day_gap, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(row_id, stature_date, day_gap, Height, SH_raw, Mass)

  combined <- tests |>
    left_join(ids, by = "ID") |>
    left_join(nearest_stature, by = "row_id") |>
    mutate(
      age_decimal = as.numeric(assess_date - DOB_date) / 365.2425,
      leg_length = Height - SH_raw,
      maturity_offset = if_else(
        !is.na(age_decimal) & !is.na(Height) & !is.na(SH_raw) & !is.na(Mass),
        -9.376 +
          0.0001882 * (leg_length * SH_raw) +
          0.0022 * (age_decimal * leg_length) +
          0.005841 * (age_decimal * SH_raw) -
          0.002658 * (age_decimal * Mass) +
          0.07693 * ((Mass / Height) * 100),
        NA_real_
      ),
      age_at_phv = age_decimal - maturity_offset
    )

  metric_cols <- c(
    "cmj_height",
    "cmj_rsi",
    "con_imp",
    "relative_pvf",
    "jh_flight_time",
    "rsi_jh_ct"
  )

  long_metrics <- combined |>
    select(ID, Group, test, assess_date, time_point, all_of(metric_cols), age_decimal, maturity_offset, age_at_phv) |>
    pivot_longer(cols = all_of(metric_cols), names_to = "metric", values_to = "value") |>
    filter(!is.na(value))

  list(
    combined = combined,
    long_metrics = long_metrics,
    missing_dob = combined |>
      filter(is.na(DOB_date)) |>
      distinct(ID) |>
      arrange(ID)
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@400;500;600;700;800&display=swap');

      :root {
        --bg-1: #f7f7f8;
        --bg-2: #eceef1;
        --card: #ffffff;
        --ink: #111827;
        --muted: #6b7280;
        --teal: #b91c1c;
        --teal-soft: #fee2e2;
        --amber: #4b5563;
        --border: #d1d5db;
      }

      body {
        font-family: 'Montserrat', 'Avenir Next', 'Segoe UI', sans-serif;
        color: var(--ink);
        background:
          radial-gradient(850px 440px at -10% -20%, #f5d0d0 0%, transparent 70%),
          radial-gradient(700px 360px at 110% -15%, #d1d5db 0%, transparent 70%),
          linear-gradient(180deg, var(--bg-1), var(--bg-2));
      }

      .hero-banner {
        margin: 8px 0 14px 0;
        padding: 18px 22px;
        border-radius: 18px;
        background: linear-gradient(120deg, #111827 0%, #b91c1c 55%, #6b7280 100%);
        color: #ffffff;
        box-shadow: 0 14px 30px rgba(15, 23, 42, 0.26);
        animation: fadein 0.45s ease-out both;
      }

      .hero-title {
        font-family: 'Montserrat', 'Avenir Next', 'Segoe UI', sans-serif;
        font-weight: 800;
        font-size: 2rem;
        line-height: 1.15;
        margin: 0;
      }

      .hero-subtitle {
        margin-top: 6px;
        font-size: 1rem;
        opacity: 0.95;
      }

      .sidebar-panel {
        background: rgba(255, 255, 255, 0.92);
        border: 1px solid var(--border);
        border-radius: 14px;
        box-shadow: 0 8px 18px rgba(17, 46, 50, 0.08);
        padding: 14px 14px 8px 14px;
        position: sticky;
        top: 12px;
        max-height: calc(100vh - 24px);
        overflow-y: auto;
      }

      .sidebar-panel .form-group > label {
        font-size: 0.76rem;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        color: var(--muted);
        font-weight: 700;
        margin-bottom: 5px;
      }

      .sidebar-panel .form-control,
      .sidebar-panel .selectize-input {
        border-radius: 10px;
        border: 1px solid #cbd5e1;
        box-shadow: none;
        min-height: 42px;
      }

      .sidebar-panel .btn-default {
        background: var(--teal);
        border: 1px solid var(--teal);
        color: #ffffff;
        border-radius: 10px;
        font-weight: 600;
      }

      .sidebar-panel .btn-default:hover {
        background: #991b1b;
        border-color: #991b1b;
        color: #ffffff;
      }

      .sidebar-panel hr {
        margin: 10px 0;
        border-color: #e5e7eb;
      }

      .sidebar-group {
        background: rgba(248, 250, 252, 0.92);
        border: 1px solid #e5e7eb;
        border-radius: 12px;
        padding: 12px;
        margin-bottom: 10px;
      }

      .sidebar-group.tight {
        padding-bottom: 8px;
      }

      .sidebar-heading {
        font-size: 0.76rem;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #6b7280;
        font-weight: 800;
        margin: 0 0 8px 0;
      }

      .sidebar-subtext {
        margin: -2px 0 10px 0;
        font-size: 0.82rem;
        color: #6b7280;
        line-height: 1.35;
      }

      .sidebar-inline {
        display: flex;
        gap: 8px;
        align-items: flex-end;
      }

      .sidebar-inline > div {
        flex: 1;
      }

      .sidebar-actions {
        display: grid;
        grid-template-columns: 1fr;
        gap: 8px;
      }

      .sidebar-panel .shiny-download-link.btn {
        width: 100%;
      }

      .sidebar-panel .radio-inline {
        margin-right: 12px;
        font-weight: 600;
        color: #374151;
      }

      .sidebar-panel details {
        border: 1px solid #e5e7eb;
        border-radius: 12px;
        background: #ffffff;
        padding: 0;
        margin-bottom: 10px;
        overflow: hidden;
      }

      .sidebar-panel details[open] {
        box-shadow: inset 0 0 0 1px #f3f4f6;
      }

      .sidebar-panel summary {
        list-style: none;
        cursor: pointer;
        padding: 11px 12px;
        font-size: 0.82rem;
        font-weight: 700;
        color: #111827;
        background: #f8fafc;
      }

      .sidebar-panel summary::-webkit-details-marker {
        display: none;
      }

      .sidebar-panel details > :not(summary) {
        padding: 10px 12px 12px 12px;
      }

      .main-panel .nav-tabs > li > a {
        border-radius: 999px;
        font-weight: 700;
        color: #4b5563;
        border: 1px solid transparent;
        background: rgba(255, 255, 255, 0.7);
        margin-right: 8px;
        padding: 10px 16px;
      }

      .main-panel .nav-tabs > li.active > a,
      .main-panel .nav-tabs > li.active > a:hover,
      .main-panel .nav-tabs > li.active > a:focus {
        color: var(--teal);
        border-color: #f1c4c4;
        background: #ffffff;
        box-shadow: 0 8px 18px rgba(15, 23, 42, 0.08);
      }

      .main-panel .tab-content {
        background: transparent;
        border: 0;
        padding: 18px 0 0 0;
        box-shadow: none;
      }

      .tab-shell {
        display: flex;
        flex-direction: column;
        gap: 14px;
      }

      .tab-intro {
        background: rgba(255, 255, 255, 0.82);
        border: 1px solid #e5e7eb;
        border-radius: 14px;
        padding: 14px 16px;
        box-shadow: 0 8px 20px rgba(15, 23, 42, 0.06);
      }

      .tab-title {
        margin: 0;
        font-size: 1.2rem;
        font-weight: 800;
        color: #111827;
      }

      .tab-subtitle {
        margin: 6px 0 0 0;
        color: #4b5563;
        line-height: 1.45;
      }

      .page-grid-2 {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 14px;
      }

      .page-grid-3 {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 14px;
      }

      .panel-card {
        background: rgba(255, 255, 255, 0.94);
        border: 1px solid #dbe1e8;
        border-radius: 16px;
        padding: 16px 18px;
        box-shadow: 0 10px 24px rgba(15, 23, 42, 0.08);
      }

      .panel-card.tight {
        padding: 14px 16px;
      }

      .panel-card .section-title {
        margin: 0 0 10px 0;
      }

      .panel-card .chart-guide,
      .panel-card .coach-compare-guide,
      .panel-card .details-note,
      .panel-card .plot-note pre.shiny-text-output {
        margin-bottom: 0;
      }

      .table-card .dataTables_wrapper {
        margin-top: 4px;
      }

      .kpi-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
      }

      .kpi-card {
        background: var(--card);
        border: 1px solid var(--border);
        border-left: 6px solid var(--teal);
        border-radius: 12px;
        min-height: 120px;
        padding: 12px 14px;
        margin-bottom: 12px;
      }

      .kpi-card.card-age {
        border-left-color: #dc2626;
      }

      .kpi-card.card-maturity {
        border-left-color: #6b7280;
      }

      .kpi-card.card-date {
        border-left-color: #9ca3af;
      }

      .kpi-label {
        font-size: 0.8rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        color: var(--muted);
        font-weight: 600;
      }

      .kpi-card pre.shiny-text-output {
        border: 0;
        background: transparent;
        padding: 2px 0 0 0;
        margin: 0;
        line-height: 1.15;
        font-size: 1.75rem;
        font-weight: 700;
        color: var(--ink);
        font-family: 'Montserrat', 'Avenir Next', 'Segoe UI', sans-serif;
      }

      .section-title {
        margin: 6px 0 10px 0;
        font-weight: 700;
        color: var(--ink);
      }

      .chart-guide {
        background: #f5f5f5;
        border: 1px solid #d1d5db;
        border-left: 5px solid #b91c1c;
        border-radius: 10px;
        padding: 10px 12px;
        margin: 2px 0 12px 0;
        color: #374151;
      }

      .chart-guide strong {
        display: block;
        margin-bottom: 4px;
      }

      .chart-guide ul {
        margin: 0;
        padding-left: 18px;
      }

      .plot-note pre.shiny-text-output {
        background: #ffffff;
        border: 1px solid #dbe4f2;
        border-radius: 8px;
        padding: 10px 12px;
        color: #334155;
        font-size: 0.95rem;
        line-height: 1.35;
      }

      .metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(190px, 1fr));
        gap: 10px;
        margin-bottom: 12px;
      }

      .metric-tile {
        background: #ffffff;
        border: 1px solid var(--border);
        border-left: 6px solid #b91c1c;
        border-radius: 12px;
        padding: 10px 12px;
        min-height: 108px;
        box-shadow: 0 5px 12px rgba(15, 23, 42, 0.08);
      }

      .metric-tile.cmj {
        border-left-color: #b91c1c;
      }

      .metric-tile.rjt {
        border-left-color: #4b5563;
      }

      .metric-tile.imtp {
        border-left-color: #9ca3af;
      }

      .tile-name {
        font-size: 0.76rem;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        color: var(--muted);
        font-weight: 700;
        margin-bottom: 6px;
      }

      .tile-value {
        font-size: 1.45rem;
        font-weight: 800;
        color: #0f172a;
        line-height: 1.1;
      }

      .tile-date {
        margin-top: 6px;
        font-size: 0.8rem;
        color: #64748b;
      }

      .child-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
        gap: 10px;
        margin-bottom: 14px;
      }

      .child-card {
        background: #ffffff;
        border: 1px solid #cbd5e1;
        border-radius: 12px;
        padding: 14px 16px;
        box-shadow: 0 5px 12px rgba(15, 23, 42, 0.08);
      }

      .child-test-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        gap: 12px;
        margin-bottom: 14px;
      }

      .child-test-card {
        background: #ffffff;
        border: 1px solid #cbd5e1;
        border-radius: 14px;
        padding: 16px 16px 12px 16px;
        box-shadow: 0 6px 14px rgba(15, 23, 42, 0.08);
      }

      .child-test-head {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 12px;
        margin-bottom: 12px;
      }

      .child-test-name {
        font-size: 1.1rem;
        font-weight: 800;
        color: #0f172a;
      }

      .child-test-date {
        font-size: 0.82rem;
        color: #64748b;
      }

      .child-test-sub {
        font-size: 0.78rem;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        color: #64748b;
        font-weight: 700;
      }

      .child-metric-list {
        display: flex;
        flex-direction: column;
        gap: 10px;
      }

      .child-metric-row {
        border-top: 1px solid #e5e7eb;
        padding-top: 10px;
      }

      .child-metric-row:first-child {
        border-top: 0;
        padding-top: 0;
      }

      .child-metric-top {
        display: flex;
        justify-content: space-between;
        align-items: baseline;
        gap: 12px;
      }

      .child-metric-name {
        font-size: 0.9rem;
        font-weight: 700;
        color: #334155;
      }

      .child-metric-value {
        font-size: 1.15rem;
        font-weight: 800;
        color: #0f172a;
      }

      .child-metric-meta {
        margin-top: 6px;
        font-size: 0.88rem;
        color: #475569;
        line-height: 1.45;
      }

      .child-title {
        font-size: 0.9rem;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        color: #64748b;
        font-weight: 700;
        margin-bottom: 10px;
      }

      .child-main {
        font-size: 1.8rem;
        font-weight: 800;
        color: #0f172a;
        line-height: 1.2;
      }

      .child-sub {
        margin-top: 8px;
        font-size: 1.05rem;
        color: #475569;
      }

      .status-pill {
        display: inline-block;
        margin-top: 6px;
        padding: 6px 12px;
        border-radius: 999px;
        font-size: 0.95rem;
        font-weight: 700;
        letter-spacing: 0.02em;
      }

      .status-good {
        background: #dcfce7;
        color: #166534;
      }

      .status-mid {
        background: #e5e7eb;
        color: #374151;
      }

      .status-amber {
        background: #fef3c7;
        color: #92400e;
      }

      .status-low {
        background: #fee2e2;
        color: #991b1b;
      }

      .status-neutral {
        background: #f3f4f6;
        color: #4b5563;
      }

      .data-banner {
        border-radius: 10px;
        border: 1px solid #fca5a5;
        background: #fef2f2;
        color: #7f1d1d;
        padding: 10px 12px;
        margin: 0 0 10px 0;
        font-weight: 600;
      }

      .details-note {
        border: 1px dashed #d1d5db;
        border-radius: 8px;
        padding: 10px 12px;
        color: #4b5563;
        background: #f9fafb;
      }

      .priority-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
        gap: 10px;
        margin-top: 8px;
      }

      .priority-card {
        background: #ffffff;
        border: 1px solid #d1d5db;
        border-left: 6px solid #6b7280;
        border-radius: 10px;
        padding: 12px 14px;
      }

      .priority-card.low {
        border-left-color: #b91c1c;
      }

      .priority-card.mid {
        border-left-color: #d97706;
      }

      .priority-card.high {
        border-left-color: #16a34a;
      }

      .priority-title {
        font-size: 0.92rem;
        font-weight: 700;
        margin-bottom: 4px;
        color: #111827;
      }

      .priority-meta {
        font-size: 0.84rem;
        color: #4b5563;
        line-height: 1.45;
      }

      .priority-badge {
        display: inline-block;
        margin: 4px 0 8px 0;
        padding: 5px 10px;
        border-radius: 999px;
        font-size: 0.78rem;
        font-weight: 800;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      .priority-badge.low {
        background: #fee2e2;
        color: #991b1b;
      }

      .priority-badge.mid {
        background: #fef3c7;
        color: #92400e;
      }

      .priority-badge.high {
        background: #dcfce7;
        color: #166534;
      }

      .priority-badge.neutral {
        background: #f3f4f6;
        color: #4b5563;
      }

      .coach-compare-guide {
        background: #f5f5f5;
        border: 1px solid #d1d5db;
        border-left: 5px solid #6b7280;
        border-radius: 10px;
        padding: 10px 12px;
        margin: 2px 0 12px 0;
        color: #374151;
      }

      .coach-compare-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(210px, 1fr));
        gap: 10px;
        margin-bottom: 12px;
      }

      .coach-compare-card {
        background: #ffffff;
        border: 1px solid var(--border);
        border-left: 6px solid #6b7280;
        border-radius: 12px;
        padding: 10px 12px;
      }

      .coach-compare-card.good {
        border-left-color: #16a34a;
      }

      .coach-compare-card.warn {
        border-left-color: #b91c1c;
      }

      .coach-compare-card.amber {
        border-left-color: #d97706;
      }

      .coach-compare-card.neutral {
        border-left-color: #9ca3af;
      }

      .cc-label {
        font-size: 0.76rem;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        color: #6b7280;
        font-weight: 700;
        margin-bottom: 6px;
      }

      .cc-value {
        font-size: 1.35rem;
        font-weight: 800;
        color: #111827;
        line-height: 1.15;
      }

      .cc-sub {
        margin-top: 6px;
        font-size: 0.82rem;
        color: #4b5563;
      }

      @keyframes fadein {
        from { opacity: 0; transform: translateY(8px); }
        to { opacity: 1; transform: translateY(0); }
      }

      @media (max-width: 1199px) {
        .page-grid-2,
        .page-grid-3,
        .kpi-grid {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  div(
    class = "hero-banner",
    div(class = "hero-title", "Athlete Performance and Longitudinal Development"),
    div(class = "hero-subtitle", "Explore CMJ, RJT, IMTP, and maturation changes across time for each athlete.")
  ),
 sidebarLayout(
    sidebarPanel(
      div(
        class = "sidebar-group tight",
        div(class = "sidebar-heading", "Session"),
        div(class = "sidebar-subtext", "Choose the view and reload the bundled app data."),
        radioButtons(
          "audience_mode",
          "Audience",
          choices = list("Child View" = "child", "Coach View" = "coach"),
          selected = "child",
          inline = TRUE
        ),
        checkboxInput("show_details", "Show details tables", value = FALSE),
        actionButton("reload", "Reload Data")
      ),
      div(
        class = "sidebar-group",
        div(class = "sidebar-heading", "Athlete Filter"),
        selectInput("athlete", "Athlete ID", choices = NULL),
        selectInput("test", "Test", choices = list("All" = "All", "CMJ" = "CMJ", "RJT" = "RJT", "IMTP" = "IMTP"), selected = "All"),
        selectInput("metric", "Metric", choices = NULL)
      ),
      tags$details(
        open = TRUE,
        tags$summary("Progress & Scatter"),
        radioButtons(
          "progress_view",
          "Progress View",
          choices = list(
            "Raw values" = "raw",
            "% change from first test" = "pct",
            "Difference vs cohort mean (time point)" = "diff"
          ),
          selected = "raw"
        ),
        checkboxInput("smooth_progress", "Smooth progress lines", value = FALSE),
        checkboxInput("show_trend", "Show linear trend", value = TRUE),
        checkboxInput("show_group_mean", "Overlay cohort mean", value = TRUE),
        checkboxInput("show_group_sd", "Show cohort SD band", value = FALSE)
      ),
      tags$details(
        tags$summary("Coach Compare"),
        div(class = "sidebar-subtext", "Only used in the Coach Compare tab."),
        selectizeInput(
          "compare_athletes",
          "Coach Compare Athletes",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 12)
        )
      ),
      tags$details(
        tags$summary("Display"),
        numericInput("display_decimals", "Decimal places", value = 2, min = 0, max = 4, step = 1),
        selectInput(
          "display_date_fmt",
          "Date format",
          choices = list(
            "DD Mon YYYY" = "%d %b %Y",
            "YYYY-MM-DD" = "%Y-%m-%d",
            "MM/DD/YYYY" = "%m/%d/%Y"
          ),
          selected = "%d %b %Y"
        )
      ),
      div(
        class = "sidebar-group tight",
        div(class = "sidebar-heading", "Export"),
        div(class = "sidebar-actions",
          downloadButton("download_snapshot_png", "Download Snapshot PNG"),
          downloadButton("download_snapshot_pdf", "Download Snapshot PDF")
        )
      ),
      class = "sidebar-panel",
      width = 3
    ),
    mainPanel(
      uiOutput("data_quality_banner"),
      tabsetPanel(
        tabPanel(
          "Snapshot",
          div(
            class = "tab-shell",
            div(
              class = "tab-intro",
              tags$h3(class = "tab-title", "Snapshot"),
              div(class = "tab-subtitle", "Use this page for a quick read on latest scores, progress, and the main coaching priorities for the selected athlete.")
            ),
            conditionalPanel(
              "input.audience_mode == 'child'",
              div(
                class = "panel-card",
                div(class = "section-title", "Athlete-Friendly Snapshot"),
                uiOutput("child_snapshot_cards")
              ),
              div(
                class = "panel-card",
                div(class = "section-title", "Top 2 Priorities"),
                uiOutput("top_priorities_cards_child")
              ),
              div(
                class = "panel-card table-card",
                div(class = "section-title", "Session Snapshot"),
                DTOutput("athlete_progress_table_snapshot")
              )
            ),
            conditionalPanel(
              "input.audience_mode == 'coach'",
              div(
                class = "panel-card",
                div(class = "section-title", "Latest Record"),
                div(
                  class = "kpi-grid",
                  div(
                    class = "kpi-card card-score",
                    div(class = "kpi-label", "Selected Metric"),
                    verbatimTextOutput("latest_score")
                  ),
                  div(
                    class = "kpi-card card-age",
                    div(class = "kpi-label", "Latest Age"),
                    verbatimTextOutput("latest_age")
                  ),
                  div(
                    class = "kpi-card card-maturity",
                    div(class = "kpi-label", "Estimated Age At PHV"),
                    verbatimTextOutput("latest_offset")
                  ),
                  div(
                    class = "kpi-card card-date",
                    div(class = "kpi-label", "Latest Session Date"),
                    verbatimTextOutput("latest_session_date")
                  )
                )
              ),
              div(
                class = "panel-card",
                div(class = "section-title", "Latest Scores Across Metrics"),
                uiOutput("snapshot_metric_cards")
              ),
              div(
                class = "panel-card",
                div(class = "section-title", "Top 2 Priorities"),
                uiOutput("top_priorities_cards_coach")
              )
            )
          )
        ),
        tabPanel(
          "Progress Over Time",
          div(
            class = "tab-shell",
            div(
              class = "tab-intro",
              tags$h3(class = "tab-title", "Progress Over Time"),
              div(class = "tab-subtitle", "Track one metric across sessions and switch the view between raw values, percentage change, and difference versus the cohort mean at the same time point.")
            ),
            div(
              class = "panel-card",
              div(
                class = "chart-guide",
                tags$strong("How To Read This"),
                tags$ul(
                  tags$li("Use 'Progress View' in the sidebar to switch between raw values, % change, and difference vs cohort mean at the same time point."),
                  tags$li("Red = selected athlete. Grey = cohort mean for the same time point (if enabled)."),
                  tags$li("In 'Difference vs cohort mean (time point)', values above 0 mean above the cohort mean.")
                )
              )
            ),
            div(
              class = "panel-card",
              plotOutput("metric_plot", height = 460)
            ),
            div(
              class = "panel-card tight plot-note",
              div(class = "section-title", "Interpretation"),
              verbatimTextOutput("metric_interpretation")
            ),
            conditionalPanel(
              "input.show_details",
              div(
                class = "panel-card table-card",
                div(class = "section-title", "Session Breakdown"),
                DTOutput("progress_table")
              )
            ),
            conditionalPanel(
              "!input.show_details",
              div(
                class = "panel-card tight",
                div(class = "details-note", "Enable 'Show details tables' in the sidebar to open full session tables.")
              )
            )
          )
        ),
        tabPanel(
          "Coach Compare",
          div(
            class = "tab-shell",
            div(
              class = "tab-intro",
              tags$h3(class = "tab-title", "Coach Compare"),
              div(class = "tab-subtitle", "Compare selected athletes using their latest result, cohort gap, and recent trend for the current metric.")
            ),
            div(
              class = "panel-card",
              div(
                class = "coach-compare-guide",
                tags$strong("Coach Compare Summary"),
                tags$ul(
                  tags$li("Rank is based on latest score for the selected metric."),
                  tags$li("Cohort colour uses T-score bands: below 45 = red, 45 to 55 = amber, above 55 = green."),
                  tags$li("Trend uses slope against age in years across the full data period for each athlete."),
                  tags$li("Improving or declining means the yearly slope is larger than 0.2 x between-athlete SD in that direction.")
                )
              )
            ),
            div(
              class = "panel-card",
              uiOutput("coach_compare_cards")
            ),
            div(
              class = "panel-card table-card",
              DTOutput("coach_compare_table")
            )
          )
        ),
        tabPanel(
              "All Sessions",
              div(
                class = "tab-shell",
                div(
                  class = "tab-intro",
                  tags$h3(class = "tab-title", "All Sessions"),
                  div(class = "tab-subtitle", "Use this table for full-session review across the selected athlete, metric, and test.")
                ),
            conditionalPanel(
              "input.show_details",
              div(class = "panel-card table-card", DTOutput("all_sessions_table"))
            ),
            conditionalPanel(
              "!input.show_details",
              div(
                class = "panel-card tight",
                div(class = "details-note", "Details are hidden. Enable 'Show details tables' in the sidebar.")
              )
            )
          )
        ),
        tabPanel(
          "Data Quality",
          div(
            class = "tab-shell",
            div(
              class = "tab-intro",
              tags$h3(class = "tab-title", "Data Quality"),
              div(class = "tab-subtitle", "Check whether any athletes are missing date of birth information needed for age and maturation outputs.")
            ),
            conditionalPanel(
              "input.show_details",
              div(
                class = "panel-card table-card",
                div(class = "section-title", "Athletes Missing DOB"),
                DTOutput("missing_dob_table")
              )
            ),
            conditionalPanel(
              "!input.show_details",
              div(
                class = "panel-card tight",
                div(class = "details-note", "Details are hidden. Enable 'Show details tables' in the sidebar.")
              )
            )
          )
        )
      ),
      class = "main-panel"
    )
  )
)

server <- function(input, output, session) {
  data_store <- reactiveVal(NULL)
  data_error <- reactiveVal(NULL)
  app_data_dir <- Sys.getenv("MSc_DATA_DIR", unset = ".")

  fmt_num <- function(x) {
    d <- input$display_decimals %||% 2
    ifelse(is.na(x), NA_character_, format(round(as.numeric(x), d), nsmall = d, trim = TRUE))
  }

  fmt_t_score <- function(x) {
    ifelse(is.na(x), NA_character_, format(round(as.numeric(x), 0), nsmall = 0, trim = TRUE))
  }

  fmt_date <- function(x) {
    fmt <- input$display_date_fmt %||% "%d %b %Y"
    ifelse(is.na(x), NA_character_, format(as.Date(x), fmt))
  }

  do_load <- function() {
    tryCatch({
      loaded <- load_data(app_data_dir)
      data_store(loaded)
      data_error(NULL)
    }, error = function(e) {
      data_store(NULL)
      data_error(e$message)
    })
  }

  observeEvent(TRUE, {
    do_load()
  }, once = TRUE)

  observeEvent(input$reload, {
    do_load()
  })

  observe({
    req(data_store())
    ids <- data_store()$combined |>
      distinct(ID) |>
      arrange(ID) |>
      pull(ID)
    athlete_choices <- as.list(ids)
    names(athlete_choices) <- ids

    updateSelectInput(
      session,
      "athlete",
      choices = athlete_choices,
      selected = ids[1] %||% NULL
    )
    updateSelectizeInput(
      session,
      "compare_athletes",
      choices = athlete_choices,
      selected = ids[seq_len(min(4, length(ids)))] %||% NULL,
      server = TRUE
    )
  })

  filtered_long <- reactive({
    req(data_store(), input$athlete)
    x <- data_store()$long_metrics |>
      filter(ID == input$athlete)

    if (!identical(input$test, "All")) {
      x <- x |> filter(test == input$test)
    }

    x
  })

  observe({
    x <- filtered_long()
    metrics <- x |>
      distinct(metric) |>
      arrange(metric) |>
      pull(metric)
    metric_choices <- as.list(metrics)
    names(metric_choices) <- pretty_metric(metrics)

    updateSelectInput(session, "metric", choices = metric_choices, selected = metrics[1] %||% NULL)
    updateSelectizeInput(
      session,
      "multi_metrics",
      choices = metric_choices,
      selected = metrics[seq_len(min(2, length(metrics)))] %||% NULL,
      server = TRUE
    )
  })

  selected_metric_data <- reactive({
    req(filtered_long(), input$metric)
    filtered_long() |>
      filter(metric == input$metric) |>
      arrange(assess_date)
  })

  athlete_group <- reactive({
    req(data_store(), input$athlete)
    grp <- data_store()$combined |>
      filter(ID == input$athlete) |>
      distinct(Group) |>
      filter(!is.na(Group), Group != "") |>
      pull(Group)
    grp[1] %||% NA_character_
  })

  selected_group_metric_data <- reactive({
    req(data_store(), input$metric, athlete_group())
    if (is.na(athlete_group())) {
      return(
        tibble(
          assess_date = as.Date(character()),
          time_point = character(),
          group_age = numeric(),
          group_mean = numeric(),
          group_sd = numeric(),
          group_n = numeric(),
          group_date = as.Date(character())
        )
      )
    }

    x <- data_store()$long_metrics |>
      filter(Group == athlete_group(), metric == input$metric)

    if (!identical(input$test, "All")) {
      x <- x |> filter(test == input$test)
    }

    x |>
      group_by(ID, time_point, assess_date) |>
      summarise(
        value = mean(value, na.rm = TRUE),
        age_decimal = mean(age_decimal, na.rm = TRUE),
        .groups = "drop"
      ) |>
      group_by(time_point) |>
      summarise(
        group_date = min(assess_date, na.rm = TRUE),
        group_age = mean(age_decimal, na.rm = TRUE),
        group_mean = mean(value, na.rm = TRUE),
        group_sd = sd(value, na.rm = TRUE),
        group_n = n(),
        .groups = "drop"
      ) |>
      arrange(group_date)
  })

  progress_metric_data <- reactive({
    req(selected_metric_data())

    athlete <- selected_metric_data() |>
      group_by(assess_date, time_point) |>
      summarise(
        athlete_value = mean(value, na.rm = TRUE),
        athlete_age = mean(age_decimal, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(assess_date) |>
      mutate(session_no = row_number())

    if (nrow(athlete) == 0) {
      return(empty_progress_metric_data())
    }

    first_athlete <- athlete$athlete_value[1]
    athlete_pct_from_first_vec <- if (is.na(first_athlete) || first_athlete == 0) {
      rep(NA_real_, nrow(athlete))
    } else {
      ((athlete$athlete_value / first_athlete) - 1) * 100
    }
    athlete <- athlete |>
      mutate(
        athlete_pct_from_first = athlete_pct_from_first_vec
      )

    g <- selected_group_metric_data() |>
      select(time_point, group_date, group_age, group_mean, group_sd, group_n)

    if (nrow(g) > 0) {
      athlete <- athlete |>
        left_join(
          g |>
            rename(group_time_point = time_point),
          by = c("time_point" = "group_time_point")
        ) |>
        mutate(
          group_time_point = time_point,
          group_gap_days = NA_real_
        )

      first_group <- athlete |>
        filter(!is.na(group_mean)) |>
        slice_head(n = 1) |>
        pull(group_mean)
      first_group <- first_group[1] %||% NA_real_
      group_pct_from_first_vec <- if (is.na(first_group) || first_group == 0) {
        rep(NA_real_, nrow(athlete))
      } else {
        ((athlete$group_mean / first_group) - 1) * 100
      }

      athlete <- athlete |>
        mutate(
          group_pct_from_first = group_pct_from_first_vec,
          diff_vs_group = athlete_value - group_mean,
          athlete_t_score = case_when(
            is.na(group_mean) ~ NA_real_,
            is.na(group_sd) ~ NA_real_,
            group_sd <= 0 ~ NA_real_,
            group_n < 5 ~ NA_real_,
            TRUE ~ 50 + 10 * ((athlete_value - group_mean) / group_sd)
          )
        )
    } else {
      athlete <- athlete |>
        mutate(
          group_date = as.Date(NA),
          group_time_point = NA_character_,
          group_age = NA_real_,
          group_mean = NA_real_,
          group_sd = NA_real_,
          group_n = NA_real_,
          group_gap_days = NA_real_,
          group_pct_from_first = NA_real_,
          diff_vs_group = NA_real_,
          athlete_t_score = NA_real_
        )
    }

    athlete
  })

  progress_trend_stats <- reactive({
    req(progress_metric_data(), input$progress_view)
    d <- input$display_decimals %||% 2
    fnum <- function(v) ifelse(is.na(v), "NA", format(round(v, d), nsmall = d, trim = TRUE))
    x <- progress_metric_data() |>
      arrange(athlete_age)

    if (identical(input$progress_view, "diff")) {
      y <- x$diff_vs_group
      slope_unit <- " per year (difference units)"
    } else if (identical(input$progress_view, "pct")) {
      y <- x$athlete_pct_from_first
      slope_unit <- " % per year"
    } else {
      y <- x$athlete_value
      slope_unit <- " units per year"
    }

    keep <- complete.cases(x$athlete_age, y)
    x2 <- x[keep, , drop = FALSE]
    y2 <- y[keep]

    if (nrow(x2) < 2) {
      return(list(
        n = nrow(x2),
        subtitle = "n < 2: not enough data for slope/correlation."
      ))
    }

    age_years <- x2$athlete_age
    pearson_r <- suppressWarnings(cor(age_years, y2, method = "pearson"))
    spearman_rho <- suppressWarnings(cor(age_years, y2, method = "spearman"))

    pearson_test <- tryCatch(
      cor.test(age_years, y2, method = "pearson"),
      error = function(e) NULL
    )

    slope <- tryCatch(
      as.numeric(coef(lm(y2 ~ age_years))[2]),
      error = function(e) NA_real_
    )

    ci_txt <- if (!is.null(pearson_test) && length(pearson_test$conf.int) == 2) {
      paste0("[", fnum(pearson_test$conf.int[1]), ", ", fnum(pearson_test$conf.int[2]), "]")
    } else {
      "NA"
    }

    p_txt <- if (!is.null(pearson_test)) {
      format.pval(pearson_test$p.value, digits = 2, eps = 0.001)
    } else {
      "NA"
    }

    list(
      n = nrow(x2),
      subtitle = paste0(
        "n = ", nrow(x2),
        " | Pearson r = ", fnum(pearson_r),
        " (95% CI ", ci_txt, ", p = ", p_txt, ")",
        "\nSpearman rho = ", fnum(spearman_rho),
        " | Slope = ", fnum(slope), slope_unit
      )
    )
  })

  selected_multi_metric_data <- reactive({
    req(filtered_long(), input$multi_metrics, input$display_date_fmt)
    metric_pair <- unique(input$multi_metrics)
    validate(need(length(metric_pair) == 2, "Select exactly 2 metrics in 'Scatter Metrics (choose 2)'."))
    date_fmt <- input$display_date_fmt %||% "%d %b %Y"

    x_tbl <- filtered_long() |>
      filter(metric == metric_pair[1]) |>
      group_by(assess_date) |>
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

    y_tbl <- filtered_long() |>
      filter(metric == metric_pair[2]) |>
      group_by(assess_date) |>
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

    matched <- match_by_nearest_date(x_tbl, y_tbl)
    if (nrow(matched) == 0) {
      return(tibble())
    }

    matched |>
      mutate(
        metric_x = metric_pair[1],
        metric_y = metric_pair[2],
        x_label = pretty_metric(metric_pair[1]),
        y_label = pretty_metric(metric_pair[2]),
        pair_date = pmin(x_date, y_date),
        point_label = if_else(
          x_date == y_date,
          format(x_date, date_fmt),
          paste0(format(x_date, date_fmt), " vs ", format(y_date, date_fmt))
        )
      )
  })

  coach_compare_data <- reactive({
    req(data_store(), input$compare_athletes, input$metric)
    ids <- unique(input$compare_athletes)
    validate(need(length(ids) > 0, "Select at least one athlete in 'Coach Compare Athletes'."))

    x <- data_store()$long_metrics |>
      filter(ID %in% ids, metric == input$metric)

    if (!identical(input$test, "All")) {
      x <- x |> filter(test == input$test)
    }

    x |>
      group_by(ID, time_point, assess_date) |>
      summarise(
        Group = dplyr::first(Group),
        value = mean(value, na.rm = TRUE),
        athlete_age = mean(age_decimal, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(ID, assess_date)
  })

  coach_compare_practical_change <- reactive({
    req(coach_compare_data())
    x <- coach_compare_data() |>
      group_by(ID) |>
      summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

    sd_between <- suppressWarnings(sd(x$mean_value, na.rm = TRUE))
    if (!is.finite(sd_between) || is.na(sd_between) || sd_between <= 0) {
      return(NA_real_)
    }

    0.2 * sd_between
  })

  coach_compare_summary <- reactive({
    req(coach_compare_data())
    practical_change <- coach_compare_practical_change()
    x <- coach_compare_data() |>
      arrange(ID, assess_date)
    validate(need(nrow(x) > 0, "No rows available for selected athletes/metric/filter."))

    per_id <- x |>
      group_by(ID) |>
      group_modify(~{
        d <- .x |>
          arrange(assess_date)
        n <- nrow(d)
        athlete_group <- d$Group[1] %||% NA_character_
        first_date <- d$assess_date[1]
        latest_date <- d$assess_date[n]
        latest_time_point <- d$time_point[n]
        first_value <- d$value[1]
        latest_value <- d$value[n]
        span_days <- as.numeric(latest_date - first_date)
        first_age <- d$athlete_age[1]
        latest_age <- d$athlete_age[n]
        span_years <- latest_age - first_age

        slope_per_year <- if (n >= 2 && sum(!is.na(d$athlete_age)) >= 2 && length(unique(d$athlete_age[!is.na(d$athlete_age)])) >= 2) {
          suppressWarnings(
            tryCatch(
              as.numeric(coef(lm(value ~ athlete_age, data = d))[2]),
              error = function(e) NA_real_
            )
          )
        } else {
          NA_real_
        }

        tibble(
          Group = athlete_group,
          sessions = n,
          first_date = first_date,
          latest_date = latest_date,
          latest_time_point = latest_time_point,
          first_value = first_value,
          latest_value = latest_value,
          span_days = span_days,
          first_age = first_age,
          latest_age = latest_age,
          span_years = span_years,
          slope_per_year = slope_per_year
        )
      }) |>
      ungroup()

    group_reference <- data_store()$long_metrics |>
      filter(metric == input$metric)

    if (!identical(input$test, "All")) {
      group_reference <- group_reference |> filter(test == input$test)
    }

    group_summary <- group_reference |>
      group_by(Group, ID, time_point, assess_date) |>
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      group_by(Group, time_point) |>
      summarise(
        group_date = min(assess_date, na.rm = TRUE),
        group_mean = mean(value, na.rm = TRUE),
        group_sd = sd(value, na.rm = TRUE),
        group_n = n(),
        .groups = "drop"
      )

    per_id_matched <- if (nrow(group_summary) == 0) {
      per_id |>
        mutate(
          matched_group_date = as.Date(NA),
          cohort_mean_latest = NA_real_,
          cohort_sd_latest = NA_real_,
          cohort_n_latest = NA_real_,
          diff_vs_cohort = NA_real_
        )
    } else {
      per_id |>
        select(ID, Group, latest_time_point, latest_value) |>
        left_join(group_summary, by = c("Group", "latest_time_point" = "time_point")) |>
        transmute(
          ID,
          matched_group_date = group_date,
          cohort_mean_latest = group_mean,
          cohort_sd_latest = group_sd,
          cohort_n_latest = group_n,
          diff_vs_cohort = latest_value - group_mean
        ) |>
        right_join(per_id, by = "ID")
    }

    per_id_matched |>
      mutate(
        pct_change_from_first = if_else(
          is.na(first_value) | abs(first_value) < 1e-9,
          NA_real_,
          ((latest_value - first_value) / abs(first_value)) * 100
        ),
        cohort_t_score = case_when(
          is.na(cohort_mean_latest) ~ NA_real_,
          is.na(cohort_sd_latest) ~ NA_real_,
          cohort_sd_latest <= 0 ~ NA_real_,
          cohort_n_latest < 5 ~ NA_real_,
          TRUE ~ 50 + 10 * ((latest_value - cohort_mean_latest) / cohort_sd_latest)
        ),
        rank_latest = min_rank(desc(latest_value)),
        cohort_status = case_when(
          is.na(cohort_t_score) ~ "No Cohort Data",
          cohort_t_score < 45 ~ "Below Cohort",
          cohort_t_score <= 55 ~ "Around Cohort",
          TRUE ~ "Above Cohort"
        ),
        trend_status = classify_trend_status(slope_per_year, practical_change)
      ) |>
      arrange(rank_latest, ID)
  })

  filtered_combined <- reactive({
    req(data_store(), input$athlete)
    x <- data_store()$combined |>
      filter(ID == input$athlete)

    if (!identical(input$test, "All")) {
      x <- x |> filter(test == input$test)
    }

    x |>
      arrange(assess_date)
  })

  output$data_quality_banner <- renderUI({
    req(filtered_combined(), filtered_long(), input$metric)

    all_rows <- filtered_combined()
    metric_rows <- selected_metric_data()
    metric_present <- filtered_long() |>
      distinct(metric) |>
      pull(metric)
    missing_metrics <- setdiff(names(metric_labels), metric_present)

    warnings <- character(0)
    if (length(missing_metrics) > 0) {
      warnings <- c(
        warnings,
        paste0("Missing metrics in current filter: ", paste(pretty_metric(missing_metrics), collapse = ", "), ".")
      )
    }

    if (nrow(all_rows) > 0 && nrow(metric_rows) > 0) {
      latest_all <- suppressWarnings(max(all_rows$assess_date, na.rm = TRUE))
      latest_metric <- suppressWarnings(max(metric_rows$assess_date, na.rm = TRUE))
      if (is.finite(as.numeric(latest_all)) && is.finite(as.numeric(latest_metric))) {
        stale_gap <- as.numeric(latest_all - latest_metric)
        if (is.finite(stale_gap) && stale_gap > 30) {
          warnings <- c(
            warnings,
            paste0(
              "Selected metric is stale for this athlete (last updated ",
              stale_gap, " days before latest session)."
            )
          )
        }
      }
    }

    if (length(warnings) == 0) {
      return(NULL)
    }

    div(
      class = "data-banner",
      paste(warnings, collapse = " ")
    )
  })

  output$latest_score <- renderText({
    req(selected_metric_data())
    x <- selected_metric_data()
    if (nrow(x) == 0) return("No data")

    latest <- x |> slice_tail(n = 1)
    paste0(fmt_num(latest$value), " (", fmt_date(latest$assess_date), ")")
  })

  output$latest_age <- renderText({
    req(filtered_combined())
    x <- filtered_combined() |> filter(!is.na(age_decimal))
    if (nrow(x) == 0) return("No data")

    latest <- x |> slice_tail(n = 1)
    paste0(fmt_num(latest$age_decimal), " years")
  })

  output$latest_offset <- renderText({
    req(filtered_combined())
    x <- filtered_combined() |> filter(!is.na(age_at_phv))
    if (nrow(x) == 0) return("No data")

    latest <- x |> slice_tail(n = 1)
    paste0(fmt_num(latest$age_at_phv), " years")
  })

  output$latest_session_date <- renderText({
    req(filtered_combined())
    x <- filtered_combined() |> filter(!is.na(assess_date))
    if (nrow(x) == 0) return("No data")
    fmt_date(x |> slice_tail(n = 1) |> pull(assess_date))
  })

  child_snapshot_tests <- reactive({
    req(data_store(), input$athlete)

    athlete_all <- data_store()$long_metrics |>
      filter(ID == input$athlete)

    validate(need(nrow(athlete_all) > 0, "No data available for this athlete yet."))

    athlete_latest <- athlete_all |>
      group_by(metric) |>
      arrange(assess_date, .by_group = TRUE) |>
      summarise(
        athlete_date = dplyr::last(assess_date),
        athlete_time_point = dplyr::last(time_point),
        athlete_value = dplyr::last(value),
        n_tests = dplyr::n(),
        previous_value = if (dplyr::n() >= 2) dplyr::nth(value, dplyr::n() - 1) else NA_real_,
        pct_from_last = if (dplyr::n() >= 2) {
          prev_val <- dplyr::nth(value, dplyr::n() - 1)
          last_val <- dplyr::last(value)
          if (is.na(prev_val) || prev_val == 0 || is.na(last_val)) {
            NA_real_
          } else {
            ((last_val - prev_val) / abs(prev_val)) * 100
          }
        } else {
          NA_real_
        },
        slope_per_year = if (dplyr::n() >= 2 && sum(!is.na(age_decimal)) >= 2 && length(unique(age_decimal[!is.na(age_decimal)])) >= 2) {
          suppressWarnings(
            tryCatch(
              as.numeric(coef(lm(value ~ age_decimal))[2]),
              error = function(e) NA_real_
            )
          )
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      mutate(
        test = unname(metric_test_map[metric])
      )

    grp <- athlete_group()
    group_stats <- if (is.na(grp)) {
      tibble(
        metric = character(),
        group_date = as.Date(character()),
        group_mean = numeric(),
        group_sd = numeric(),
        group_n = numeric(),
        trend_threshold = numeric()
      )
    } else {
      group_pool <- data_store()$long_metrics |>
        filter(Group == grp)

      group_summary <- group_pool |>
        group_by(metric, ID, time_point, assess_date) |>
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
        group_by(metric, time_point) |>
        summarise(
          group_date = min(assess_date, na.rm = TRUE),
          group_mean = mean(value, na.rm = TRUE),
          group_sd = sd(value, na.rm = TRUE),
          group_n = n(),
          .groups = "drop"
        )

      practical_change <- group_pool |>
        group_by(metric, ID) |>
        summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") |>
        group_by(metric) |>
        summarise(
          trend_threshold = {
            sd_between <- suppressWarnings(sd(mean_value, na.rm = TRUE))
            if (is.finite(sd_between) && !is.na(sd_between) && sd_between > 0) 0.2 * sd_between else NA_real_
          },
          .groups = "drop"
        )

      if (nrow(group_summary) == 0) {
        tibble(
          metric = character(),
          time_point = character(),
          group_date = as.Date(character()),
          group_mean = numeric(),
          group_sd = numeric(),
          group_n = numeric(),
          trend_threshold = numeric()
        )
      } else {
        athlete_latest |>
          select(metric, athlete_time_point) |>
          left_join(group_summary, by = c("metric", "athlete_time_point" = "time_point")) |>
          transmute(metric, group_date, group_mean, group_sd, group_n) |>
          left_join(practical_change, by = "metric")
      }
    }

    athlete_latest |>
      left_join(group_stats, by = "metric") |>
      mutate(
        diff_vs_group = athlete_value - group_mean,
        athlete_t_score = case_when(
          is.na(group_mean) ~ NA_real_,
          is.na(group_sd) ~ NA_real_,
          group_sd <= 0 ~ NA_real_,
          group_n < 5 ~ NA_real_,
          TRUE ~ 50 + 10 * ((athlete_value - group_mean) / group_sd)
        ),
        group_label = classify_t_score_band(athlete_t_score),
        group_class = classify_t_score_class(athlete_t_score),
        trend_status = classify_trend_status(slope_per_year, trend_threshold),
        progress_label = case_when(
          trend_status == "Improving" ~ "Moving Up",
          trend_status == "Stable" ~ "Holding Steady",
          trend_status == "Declining" ~ "Needs Work",
          TRUE ~ "Need More Data"
        ),
        progress_class = trend_status_class(trend_status),
        progress_note = case_when(
          trend_status == "Need Data" ~ "More tests are needed to show your trend over time.",
          trend_status == "Improving" ~ "Your results are trending up across all of your tests.",
          trend_status == "Declining" ~ "This metric needs some extra attention across your tests so far.",
          TRUE ~ "Your results are staying fairly steady across all of your tests."
        ),
        last_test_note = case_when(
          n_tests < 2 ~ "No previous test to compare yet.",
          is.na(pct_from_last) ~ "Change since last test is not available.",
          pct_from_last > 0 ~ paste0("+", fmt_num(pct_from_last), "% since last test"),
          TRUE ~ paste0(fmt_num(pct_from_last), "% since last test")
        )
      ) |>
      arrange(factor(test, levels = c("CMJ", "IMTP", "RJT")), metric)
  })

  priority_recommendations <- reactive({
    req(filtered_long(), data_store())
    athlete_latest <- filtered_long() |>
      group_by(metric) |>
      slice_max(assess_date, n = 1, with_ties = FALSE) |>
      ungroup() |>
      transmute(metric, athlete_date = assess_date, athlete_time_point = time_point, athlete_value = value)

    if (nrow(athlete_latest) == 0) {
      return(tibble())
    }

    joined <- if (is.na(athlete_group())) {
      athlete_latest |>
        mutate(
          group_date = as.Date(NA),
          group_mean = NA_real_,
          diff_vs_group = NA_real_
        )
    } else {
      group_pool <- data_store()$long_metrics |>
        filter(Group == athlete_group())

      if (!identical(input$test, "All")) {
        group_pool <- group_pool |> filter(test == input$test)
      }

      group_summary <- group_pool |>
        group_by(metric, ID, time_point, assess_date) |>
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
        group_by(metric, time_point) |>
        summarise(
          group_date = min(assess_date, na.rm = TRUE),
          group_mean = mean(value, na.rm = TRUE),
          group_sd = sd(value, na.rm = TRUE),
          group_n = n(),
          .groups = "drop"
        )

      if (nrow(group_summary) == 0) {
        athlete_latest |>
          mutate(
            group_date = as.Date(NA),
            group_mean = NA_real_,
            group_sd = NA_real_,
            group_n = NA_real_,
            diff_vs_group = NA_real_,
            athlete_t_score = NA_real_
          )
      } else {
        athlete_latest |>
          left_join(group_summary, by = c("metric", "athlete_time_point" = "time_point")) |>
          mutate(
            diff_vs_group = athlete_value - group_mean,
            athlete_t_score = case_when(
              is.na(group_mean) ~ NA_real_,
              is.na(group_sd) ~ NA_real_,
              group_sd <= 0 ~ NA_real_,
              group_n < 5 ~ NA_real_,
              TRUE ~ 50 + 10 * ((athlete_value - group_mean) / group_sd)
            )
          ) |>
          select(metric, athlete_date, athlete_value, group_date, group_mean, group_sd, group_n, diff_vs_group, athlete_t_score)
      }
    }

    actions <- c(
      cmj_height = "Focus on jump mechanics and lower-body strength sessions.",
      cmj_rsi = "Emphasize reactive strength and faster transition drills.",
      con_imp = "Prioritize concentric force work with loaded jumps.",
      relative_pvf = "Build maximal force capacity with heavy strength lifts.",
      jh_flight_time = "Improve take-off quality and countermovement control.",
      rsi_jh_ct = "Use plyometric drills targeting quick ground contacts."
    )

    joined |>
      mutate(
        status = case_when(
          is.na(athlete_t_score) ~ "no_group",
          athlete_t_score < 45 ~ "below",
          athlete_t_score > 55 ~ "above",
          TRUE ~ "on_track"
        ),
        priority_rank = case_when(
          status == "below" ~ athlete_t_score,
          status == "on_track" ~ 100 + abs(athlete_t_score - 50),
          status == "above" ~ 200 + abs(athlete_t_score - 50),
          TRUE ~ 300
        ),
        action = unname(actions[metric])
      ) |>
      arrange(priority_rank, metric) |>
      slice_head(n = 2)
  })

  output$child_snapshot_cards <- renderUI({
    req(child_snapshot_tests())
    x <- child_snapshot_tests()

    test_cards <- lapply(c("CMJ", "IMTP", "RJT"), function(test_name) {
      d <- x |>
        filter(test == test_name)

      if (nrow(d) == 0) {
        return(
          tags$div(
            class = "child-test-card",
            tags$div(class = "child-test-head",
              tags$div(
                tags$div(class = "child-test-sub", "Test Summary"),
                tags$div(class = "child-test-name", test_name)
              )
            ),
            tags$div(class = "details-note", "No results available in the current date range.")
          )
        )
      }

      latest_test_date <- max(d$athlete_date, na.rm = TRUE)

      metric_rows <- lapply(seq_len(nrow(d)), function(i) {
        r <- d[i, ]
        t_txt <- if (is.na(r$athlete_t_score)) {
          "Group Score not available"
        } else {
          paste0("Group Score ", fmt_t_score(r$athlete_t_score))
        }

        progress_txt <- r$progress_note
        last_test_txt <- r$last_test_note

        tags$div(
          class = "child-metric-row",
          tags$div(
            class = "child-metric-top",
            tags$div(class = "child-metric-name", pretty_metric(r$metric)),
            tags$div(class = "child-metric-value", fmt_num(r$athlete_value))
          ),
          tags$span(class = paste("status-pill", r$group_class), r$group_label),
          tags$div(class = "child-metric-meta", t_txt),
          tags$div(class = "child-metric-meta", "Trend Over Time"),
          tags$span(class = paste("status-pill", r$progress_class), r$progress_label),
          tags$div(class = "child-metric-meta", progress_txt),
          tags$div(class = "child-metric-meta", last_test_txt)
        )
      })

      tags$div(
        class = "child-test-card",
        tags$div(
          class = "child-test-head",
          tags$div(
            tags$div(class = "child-test-sub", "Test Summary"),
            tags$div(class = "child-test-name", test_name)
          ),
          tags$div(class = "child-test-date", paste0("Latest: ", fmt_date(latest_test_date)))
        ),
        tags$div(class = "child-metric-list", metric_rows)
      )
    })

    tags$div(class = "child-test-grid", test_cards)
  })

  build_priority_cards <- function(rec) {
    if (is.null(rec) || nrow(rec) == 0) {
      return(div(class = "details-note", "No priority recommendations available for this filter."))
    }

    cards <- lapply(seq_len(nrow(rec)), function(i) {
      r <- rec[i, ]
      card_class <- if (r$status == "below") "priority-card low" else if (r$status == "on_track") "priority-card mid" else if (r$status == "above") "priority-card high" else "priority-card"
      badge_class <- if (r$status == "below") "priority-badge low" else if (r$status == "on_track") "priority-badge mid" else if (r$status == "above") "priority-badge high" else "priority-badge neutral"
      status_label <- switch(
        r$status,
        below = "Needs Work",
        on_track = "On Track",
        above = "Strength",
        "No Group Data"
      )

      reason_txt <- if (is.na(r$diff_vs_group)) {
        "Reason: group comparison is not available for this metric yet."
      } else {
        paste0(
          "Reason: T-score is ",
          fmt_t_score(r$athlete_t_score),
          " and the latest score is ",
          ifelse(r$diff_vs_group >= 0, "+", ""),
          fmt_num(r$diff_vs_group),
          " versus the group mean."
        )
      }

      div(
        class = card_class,
        div(class = "priority-title", paste0(i, ". ", pretty_metric(r$metric))),
        tags$span(class = badge_class, status_label),
        div(class = "priority-meta", paste0("Latest test: ", fmt_date(r$athlete_date))),
        div(class = "priority-meta", reason_txt),
        div(class = "priority-meta", paste0("Your score: ", fmt_num(r$athlete_value), " | Group mean: ", ifelse(is.na(r$group_mean), "NA", fmt_num(r$group_mean)))),
        div(class = "priority-meta", paste0("Action: ", r$action))
      )
    })

    div(class = "priority-grid", cards)
  }

  output$top_priorities_cards_child <- renderUI({
    rec <- priority_recommendations()
    build_priority_cards(rec)
  })

  output$top_priorities_cards_coach <- renderUI({
    rec <- priority_recommendations()
    build_priority_cards(rec)
  })

  output$snapshot_metric_cards <- renderUI({
    req(filtered_long())
    x <- filtered_long() |>
      arrange(assess_date)

    metrics_order <- names(metric_labels)

    tiles <- lapply(metrics_order, function(m) {
      latest <- x |>
        filter(metric == m) |>
        slice_tail(n = 1)

      value_txt <- if (nrow(latest) == 0 || is.na(latest$value[1])) {
        "No data"
      } else {
        fmt_num(latest$value[1])
      }

      date_txt <- if (nrow(latest) == 0 || is.na(latest$assess_date[1])) {
        ""
      } else {
        fmt_date(latest$assess_date[1])
      }

      tile_class <- if (startsWith(m, "cmj_") || m == "con_imp") {
        "metric-tile cmj"
      } else if (startsWith(m, "jh_") || startsWith(m, "rsi_jh")) {
        "metric-tile rjt"
      } else if (m == "relative_pvf") {
        "metric-tile imtp"
      } else {
        "metric-tile"
      }

      tags$div(
        class = tile_class,
        tags$div(class = "tile-name", pretty_metric(m)),
        tags$div(class = "tile-value", value_txt),
        tags$div(class = "tile-date", ifelse(date_txt == "", "", paste0("Latest: ", date_txt)))
      )
    })

    tags$div(class = "metric-grid", tiles)
  })

  build_snapshot_export_plot <- function(test_tbl) {
    tests <- c("CMJ", "IMTP", "RJT")
    y_pos <- c(0.84, 0.56, 0.28)
    p <- ggplot() +
      xlim(0, 1) +
      ylim(0, 1) +
      annotate("text", x = 0.02, y = 0.98, hjust = 0, vjust = 1, size = 6, fontface = "bold",
               label = paste("Athlete Snapshot:", input$athlete)) +
      theme_void(base_family = "Montserrat") +
      theme(plot.margin = margin(10, 10, 10, 10))

    for (i in seq_along(tests)) {
      test_name <- tests[i]
      y_top <- y_pos[i]
      y_bottom <- y_top - 0.2
      d <- test_tbl |>
        filter(test == test_name)

      p <- p +
        annotate("rect", xmin = 0.02, xmax = 0.98, ymin = y_bottom, ymax = y_top, fill = "#ffffff", color = "#d1d5db") +
        annotate("text", x = 0.04, y = y_top - 0.03, hjust = 0, size = 4.8, fontface = "bold", label = test_name)

      if (nrow(d) == 0) {
        p <- p + annotate("text", x = 0.04, y = y_top - 0.09, hjust = 0, size = 3.7, color = "#6b7280", label = "No results available")
      } else {
        row_y <- seq(y_top - 0.08, y_bottom + 0.04, length.out = nrow(d))
        for (j in seq_len(nrow(d))) {
          r <- d[j, ]
          summary_txt <- paste0(
            pretty_metric(r$metric), ": ", fmt_num(r$athlete_value),
            " | ", classify_t_score_band(r$athlete_t_score),
            " | Group Score ", ifelse(is.na(r$athlete_t_score), "NA", fmt_t_score(r$athlete_t_score)),
            " | ", r$progress_label
          )
          p <- p + annotate("text", x = 0.04, y = row_y[j], hjust = 0, size = 3.5, color = "#374151", label = summary_txt)
        }
      }
    }

    p
  }

  output$download_snapshot_png <- downloadHandler(
    filename = function() {
      paste0("snapshot_", gsub(" ", "_", input$athlete), "_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      s <- child_snapshot_tests()
      p <- build_snapshot_export_plot(s)
      ggsave(file, plot = p, width = 11, height = 7, dpi = 300)
    }
  )

  output$download_snapshot_pdf <- downloadHandler(
    filename = function() {
      paste0("snapshot_", gsub(" ", "_", input$athlete), "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      s <- child_snapshot_tests()
      p <- build_snapshot_export_plot(s)
      ggsave(file, plot = p, width = 11, height = 7)
    }
  )

  output$metric_interpretation <- renderText({
    req(progress_metric_data(), input$metric)
    x <- progress_metric_data() |>
      arrange(assess_date)
    if (nrow(x) == 0) {
      return("No data available for the selected metric and date range.")
    }

    first_row <- x |> slice_head(n = 1)
    latest_row <- x |> slice_tail(n = 1)
    change_val <- latest_row$athlete_value - first_row$athlete_value
    change_dir <- ifelse(abs(change_val) < 1e-9, "no overall change", ifelse(change_val > 0, "an increase", "a decrease"))
    pct_change <- ifelse(
      is.na(first_row$athlete_value) || first_row$athlete_value == 0,
      NA_real_,
      (change_val / abs(first_row$athlete_value)) * 100
    )

    base_txt <- paste0(
      "View: ",
      switch(
        input$progress_view,
        raw = "Raw values",
        pct = "% change from first test",
        diff = "Difference vs cohort mean (time point)",
        "Raw values"
      ),
      ". ",
      "Latest ", pretty_metric(input$metric), ": ", fmt_num(latest_row$athlete_value), " at age ", fmt_num(latest_row$athlete_age), " years. ",
      "Since age ", fmt_num(first_row$athlete_age), ", there is ", change_dir, " of ",
      ifelse(change_val >= 0, "+", ""), fmt_num(change_val),
      ifelse(is.na(pct_change), ".", paste0(" (", ifelse(pct_change >= 0, "+", ""), fmt_num(pct_change), "%)."))
    )

    if (all(is.na(x$group_mean))) {
      return(paste0(base_txt, " Group mean is not available for this selection."))
    }

    group_diff <- latest_row$diff_vs_group
    group_time_point <- latest_row$group_time_point
    comp_txt <- paste0(
      " Compared with ", athlete_group(), " group mean at ", group_time_point, ", ",
      "the athlete is ", ifelse(group_diff >= 0, "above", "below"), " by ",
      fmt_num(abs(group_diff)), "."
    )

    paste0(base_txt, comp_txt)
  })

  output$metric_plot <- renderPlot({
    req(progress_metric_data(), input$progress_view, progress_trend_stats())
    x <- progress_metric_data()
    stats <- progress_trend_stats()
    validate(need(nrow(x) > 0, "No rows for selected athlete/test/metric."))
    metric_label <- pretty_metric(input$metric)

    if (identical(input$progress_view, "diff")) {
      validate(need(any(!is.na(x$diff_vs_group)), "No group mean available for difference view."))
      x <- x |>
        mutate(
          cohort_band = case_when(
            is.na(athlete_t_score) ~ "no_data",
            athlete_t_score < 45 ~ "below",
            athlete_t_score <= 55 ~ "around",
            TRUE ~ "above"
          )
        )

      p <- ggplot(x, aes(x = athlete_age, y = diff_vs_group)) +
        geom_hline(yintercept = 0, color = "#9CA3AF", linetype = 2) +
        geom_line(linewidth = 0.9, color = "#374151") +
        geom_point(aes(color = cohort_band), size = 2.6) +
        scale_color_manual(
          values = c(below = "#B91C1C", around = "#D97706", above = "#16A34A", no_data = "#6B7280"),
          guide = "none"
        ) +
        labs(
          title = paste("Progress Over Time:", metric_label),
          subtitle = paste0("Difference vs ", athlete_group(), " mean\n", stats$subtitle),
          x = "Age (years)",
          y = "Athlete - Group Mean",
          caption = "Green = T-score above 55 | Amber = T-score 45 to 55 | Red = T-score below 45"
        ) +
        dashboard_plot_theme()

      if (isTRUE(input$show_trend) && sum(!is.na(x$diff_vs_group)) > 2) {
        p <- p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "#DC2626")
      }

      return(p)
    }

    if (identical(input$progress_view, "pct")) {
      y_col <- "athlete_pct_from_first"
      y_lab <- "% Change From First Test"
      g_col <- "group_pct_from_first"
      subtitle_txt <- paste0("Athlete ", input$athlete, " | Relative change over time")
      ref_line <- TRUE
    } else {
      y_col <- "athlete_value"
      y_lab <- metric_label
      g_col <- "group_mean"
      subtitle_txt <- paste0("Athlete ", input$athlete, " | Age Group: ", athlete_group())
      ref_line <- FALSE
    }

    p <- ggplot(x, aes(x = athlete_age, y = .data[[y_col]])) +
      geom_point(size = 2.2, color = "#B91C1C")

    if (isTRUE(input$smooth_progress) && nrow(x) >= 4) {
      p <- p + geom_smooth(method = "loess", se = FALSE, span = 0.8, linewidth = 1.0, color = "#374151")
    } else {
      p <- p + geom_line(linewidth = 0.9, color = "#374151")
    }

    if (isTRUE(ref_line)) {
      p <- p + geom_hline(yintercept = 0, color = "#9CA3AF", linetype = 2)
    }

    if (isTRUE(input$show_group_mean) && any(!is.na(x[[g_col]]))) {
      if (identical(input$progress_view, "raw") && isTRUE(input$show_group_sd) && any(!is.na(x$group_sd))) {
        p <- p + geom_ribbon(
          data = x,
          aes(
            x = athlete_age,
            ymin = group_mean - group_sd,
            ymax = group_mean + group_sd
          ),
          inherit.aes = FALSE,
          fill = "#E5E7EB",
          alpha = 0.2
        )
      }

      if (isTRUE(input$smooth_progress) && sum(!is.na(x[[g_col]])) >= 4) {
        p <- p + geom_smooth(
          aes(y = .data[[g_col]]),
          method = "loess",
          se = FALSE,
            span = 0.8,
            color = "#6B7280",
            linewidth = 1.0,
            linetype = 2
          )
      } else {
        p <- p + geom_line(
          aes(y = .data[[g_col]]),
          color = "#6B7280",
          linewidth = 1.0,
          linetype = 2
        )
      }

      p <- p + geom_point(
        aes(y = .data[[g_col]]),
        color = "#6B7280",
        size = 1.7
      )
    }

    if (isTRUE(input$show_trend) && sum(!is.na(x[[y_col]])) > 2) {
      p <- p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "#DC2626")
    }

    p +
      labs(
        title = paste("Progress Over Time:", metric_label),
        subtitle = paste0(subtitle_txt, "\n", stats$subtitle),
        x = "Age (years)",
        y = y_lab,
        caption = ifelse(
          isTRUE(input$show_group_mean),
          "Red = selected athlete. Dashed grey = cohort mean for the same time point.",
          "Red = selected athlete."
        )
      ) +
      dashboard_plot_theme()
  })

  output$athlete_progress_plot_snapshot <- renderPlot({
    req(progress_metric_data(), input$metric)
    x <- progress_metric_data()
    validate(need(nrow(x) > 0, "No athlete progress data for current filter."))

    p <- ggplot(x, aes(x = assess_date, y = athlete_value)) +
      geom_line(linewidth = 0.9, color = "#B91C1C") +
      geom_point(size = 2.2, color = "#B91C1C")

    if (isTRUE(input$show_trend) && sum(!is.na(x$athlete_value)) > 2) {
      p <- p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "#111827")
    }

    p +
      labs(
        title = paste("Progress:", pretty_metric(input$metric)),
        subtitle = paste0("Athlete ", input$athlete, " | Individual progress only"),
        x = "Assessment Date",
        y = pretty_metric(input$metric)
      ) +
      dashboard_plot_theme()
  })

  output$athlete_scatter_plot_snapshot <- renderPlot({
    req(selected_multi_metric_data())
    x_plot <- selected_multi_metric_data() |>
      drop_na(x_value, y_value)

    validate(need(nrow(x_plot) >= 2, "Not enough paired points for athlete scatter."))

    d <- input$display_decimals %||% 2
    pearson_r <- suppressWarnings(cor(x_plot$x_value, x_plot$y_value, method = "pearson"))
    subtitle_txt <- paste0("n = ", nrow(x_plot), " | Pearson r = ", format(round(pearson_r, d), nsmall = d, trim = TRUE))

    p <- ggplot(x_plot, aes(x = x_value, y = y_value)) +
      geom_point(color = "#B91C1C", size = 2.6, alpha = 0.9) +
      labs(
        title = paste("Metric Relationship:", unique(x_plot$x_label), "vs", unique(x_plot$y_label)),
        subtitle = subtitle_txt,
        x = unique(x_plot$x_label),
        y = unique(x_plot$y_label)
      ) +
      dashboard_plot_theme()

    if (isTRUE(input$show_trend)) {
      p <- p + geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "#111827", fill = "#D1D5DB", alpha = 0.25)
    }

    p
  })

  output$athlete_progress_table_snapshot <- renderDT({
    req(progress_metric_data())
    d <- input$display_decimals %||% 2

    out <- progress_metric_data() |>
      transmute(
        Session = session_no,
        `Assessment Date` = fmt_date(assess_date),
        `Athlete Value` = athlete_value,
        `T-score` = athlete_t_score,
        `% Change From First` = athlete_pct_from_first
      )

    names(out)[names(out) == "Athlete Value"] <- pretty_metric(input$metric)

    datatable(
      out,
      options = list(
        pageLength = 8,
        lengthChange = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    ) |>
      formatRound(
        columns = "T-score",
        digits = 0
      ) |>
      formatRound(
        columns = setdiff(names(out), c("Session", "Assessment Date", "T-score")),
        digits = d
      ) |>
      formatStyle(
        "% Change From First",
        color = styleInterval(0, c("#991B1B", "#166534")),
        backgroundColor = styleInterval(0, c("#FEE2E2", "#DCFCE7")),
        fontWeight = "700"
      )
  })

  output$progress_table <- renderDT({
    req(progress_metric_data(), input$metric)
    x <- progress_metric_data()
    validate(need(nrow(x) > 0, "No sessions available for selected filters."))

    out <- x |>
      transmute(
        Session = session_no,
        `Assessment Date` = fmt_date(assess_date),
        `Athlete Value` = fmt_num(athlete_value),
        `T-score` = fmt_t_score(athlete_t_score),
        `% Change From First` = fmt_num(athlete_pct_from_first),
        `Group Mean (Time Point)` = fmt_num(group_mean),
        `Difference vs Group` = fmt_num(diff_vs_group),
        `Group Time Point` = group_time_point,
        `Group n` = as.integer(group_n)
      )

    names(out)[names(out) == "Athlete Value"] <- pretty_metric(input$metric)

    datatable(
      out,
      filter = "top",
      options = list(
        pageLength = 10,
        lengthMenu = list(c(10, 20, 50), c("10", "20", "50")),
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })

  output$multi_metric_plot <- renderPlot({
    req(selected_multi_metric_data())
    d <- input$display_decimals %||% 2
    fnum <- function(v) ifelse(is.na(v), "NA", format(round(v, d), nsmall = d, trim = TRUE))
    x_plot <- selected_multi_metric_data() |>
      drop_na(x_value, y_value)

    validate(need(nrow(x_plot) >= 2, "Not enough paired observations. Choose different metrics or widen the date range."))

    pearson_r <- suppressWarnings(cor(x_plot$x_value, x_plot$y_value, method = "pearson"))
    spearman_rho <- suppressWarnings(cor(x_plot$x_value, x_plot$y_value, method = "spearman"))

    pearson_test <- tryCatch(
      cor.test(x_plot$x_value, x_plot$y_value, method = "pearson"),
      error = function(e) NULL
    )

    slope <- tryCatch(
      as.numeric(coef(lm(y_value ~ x_value, data = x_plot))[2]),
      error = function(e) NA_real_
    )

    ci_txt <- if (!is.null(pearson_test) && length(pearson_test$conf.int) == 2) {
      paste0("[", fnum(pearson_test$conf.int[1]), ", ", fnum(pearson_test$conf.int[2]), "]")
    } else {
      "NA"
    }

    p_txt <- if (!is.null(pearson_test)) {
      format.pval(pearson_test$p.value, digits = 2, eps = 0.001)
    } else {
      "NA"
    }

    subtitle_txt <- paste0(
      "n = ", nrow(x_plot),
      " | Pearson r = ", fnum(pearson_r),
      " (95% CI ", ci_txt, ", p = ", p_txt, ")",
      "\nSpearman rho = ", fnum(spearman_rho),
      " | Slope = ", fnum(slope),
      " | Median date gap = ", fnum(median(x_plot$date_gap_days, na.rm = TRUE)), " days"
    )

    p <- ggplot(x_plot, aes(x = x_value, y = y_value)) +
      geom_point(aes(color = date_gap_days), size = 2.8, alpha = 0.9) +
      scale_color_gradient(low = "#9CA3AF", high = "#B91C1C", name = "Date gap (days)") +
      labs(
        title = paste("Scatter:", unique(x_plot$x_label), "vs", unique(x_plot$y_label)),
        subtitle = subtitle_txt,
        x = unique(x_plot$x_label),
        y = unique(x_plot$y_label)
      ) +
      dashboard_plot_theme()

    if (isTRUE(input$show_trend)) {
      p <- p + geom_smooth(method = "lm", se = TRUE, linewidth = 0.85, color = "#DC2626", fill = "#FECACA", alpha = 0.25)
    }

    if (isTRUE(input$show_scatter_labels)) {
      p <- p + geom_text(
        aes(label = point_label),
        size = 3.1,
        color = "#4B5563",
        nudge_y = 0.02 * diff(range(x_plot$y_value, na.rm = TRUE))
      )
    }

    p
  })

  output$scatter_hover_info <- renderText({
    req(selected_multi_metric_data())
    x <- selected_multi_metric_data() |>
      drop_na(x_value, y_value)

    if (nrow(x) == 0) {
      return("No paired observations in this date range/window.")
    }

    if (is.null(input$scatter_hover)) {
      return("Hover over a point to view paired session dates and values.")
    }

    pt <- nearPoints(
      x,
      coordinfo = input$scatter_hover,
      xvar = "x_value",
      yvar = "y_value",
      threshold = 12,
      maxpoints = 1
    )

    if (nrow(pt) == 0) {
      return("Hover over a point to view paired session dates and values.")
    }

    paste0(
      "X metric: ", pt$x_label[1], " = ", fmt_num(pt$x_value[1]), " (", fmt_date(pt$x_date[1]), ")\n",
      "Y metric: ", pt$y_label[1], " = ", fmt_num(pt$y_value[1]), " (", fmt_date(pt$y_date[1]), ")\n",
      "Date gap: ", fmt_num(pt$date_gap_days[1]), " days"
    )
  })

  output$coach_compare_cards <- renderUI({
    req(coach_compare_summary(), input$metric)
    x <- coach_compare_summary()
    validate(need(nrow(x) > 0, "No rows available for selected athletes/metric/filter."))

    d <- input$display_decimals %||% 2
    fnum <- function(v) ifelse(is.na(v), "NA", format(round(v, d), nsmall = d, trim = TRUE))
    practical_change <- coach_compare_practical_change()
    best <- x |>
      arrange(rank_latest, ID) |>
      slice_head(n = 1)

    total_n <- nrow(x)
    improving_n <- sum(x$trend_status == "Improving", na.rm = TRUE)
    stable_n <- sum(x$trend_status == "Stable", na.rm = TRUE)
    above_n <- sum(x$cohort_status == "Above Cohort", na.rm = TRUE)
    around_n <- sum(x$cohort_status == "Around Cohort", na.rm = TRUE)
    mean_latest <- mean(x$latest_value, na.rm = TRUE)

    summary_state <- function(rate_good, rate_amber = 0) {
      if (is.na(rate_good) || is.na(rate_amber)) {
        return("neutral")
      }
      if (rate_good >= 0.6) {
        return("good")
      }
      if (rate_amber >= 0.3 || rate_good >= 0.3) {
        return("amber")
      }
      "warn"
    }

    card <- function(label, value, sub = "", card_class = "neutral") {
      tags$div(
        class = paste("coach-compare-card", card_class),
        tags$div(class = "cc-label", label),
        tags$div(class = "cc-value", value),
        tags$div(class = "cc-sub", sub)
      )
    }

    tags$div(
      class = "coach-compare-grid",
      card(
        label = "Athletes Compared",
        value = as.character(nrow(x)),
        sub = paste("Metric:", pretty_metric(input$metric)),
        card_class = "neutral"
      ),
      card(
        label = "Cohort Mean (Latest)",
        value = fnum(mean_latest),
        sub = "Average latest score across selected athletes",
        card_class = "neutral"
      ),
      card(
        label = "Top Latest Score",
        value = paste0(best$ID, " | ", fnum(best$latest_value)),
        sub = paste("Latest date:", fmt_date(best$latest_date)),
        card_class = "good"
      ),
      card(
        label = "Improving Trends",
        value = paste0(improving_n, " / ", total_n),
        sub = if (is.na(practical_change)) {
          "Trend threshold unavailable for current filter"
        } else {
          paste0(
            stable_n, " athlete", ifelse(stable_n == 1, " is", "s are"),
            " inside +/-", fnum(practical_change), " units per year"
          )
        },
        card_class = summary_state(improving_n / total_n, stable_n / total_n)
      ),
      card(
        label = "Above Cohort Band",
        value = paste0(above_n, " / ", total_n),
        sub = paste0(around_n, " athlete", ifelse(around_n == 1, " is", "s are"), " between T-scores 45 and 55"),
        card_class = summary_state(above_n / total_n, around_n / total_n)
      )
    )
  })

  output$coach_compare_table <- renderDT({
    req(coach_compare_summary())
    d <- input$display_decimals %||% 2
    practical_change <- coach_compare_practical_change()
    trend_breaks <- if (is.finite(practical_change) && !is.na(practical_change) && practical_change > 0) {
      c(-practical_change, practical_change)
    } else {
      c(-1e-9, 1e-9)
    }

    out <- coach_compare_summary() |>
      transmute(
        Rank = rank_latest,
        `Athlete ID` = ID,
        `Latest Score` = latest_value,
        `Cohort T-score` = cohort_t_score,
        `% Change From First` = pct_change_from_first,
        `Trend / Year` = slope_per_year,
        Sessions = sessions,
        `Latest Date` = fmt_date(latest_date),
        `Cohort Status` = cohort_status,
        `Trend Status` = trend_status
      ) |>
      arrange(Rank, `Athlete ID`)

    datatable(
      out,
      options = list(
        pageLength = 12,
        lengthChange = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE,
        order = list(list(0, "asc"))
      ),
      rownames = FALSE
    ) |>
      formatRound(
        columns = c("Latest Score", "% Change From First", "Trend / Year"),
        digits = d
      ) |>
      formatRound(
        columns = "Cohort T-score",
        digits = 0
      ) |>
      formatStyle(
        "Cohort T-score",
        color = styleInterval(c(45, 55), c("#991B1B", "#92400E", "#166534")),
        backgroundColor = styleInterval(c(45, 55), c("#FEE2E2", "#FEF3C7", "#DCFCE7")),
        fontWeight = "700"
      ) |>
      formatStyle(
        "Trend / Year",
        color = styleInterval(trend_breaks, c("#991B1B", "#92400E", "#166534")),
        backgroundColor = styleInterval(trend_breaks, c("#FEE2E2", "#FEF3C7", "#DCFCE7")),
        fontWeight = "700"
      ) |>
      formatStyle(
        "Cohort Status",
        color = styleEqual(
          c("Above Cohort", "Around Cohort", "Below Cohort", "No Cohort Data"),
          c("#166534", "#92400E", "#991B1B", "#4B5563")
        ),
        fontWeight = "700"
      ) |>
      formatStyle(
        "Trend Status",
        color = styleEqual(c("Improving", "Stable", "Declining", "Need Data"), c("#166534", "#92400E", "#991B1B", "#4B5563")),
        fontWeight = "700"
      )
  })

  output$all_sessions_table <- renderDT({
    req(filtered_combined(), input$metric)
    d <- input$display_decimals %||% 2

    base <- filtered_combined() |>
      select(ID, Group, test, assess_date, age_decimal, maturity_offset, age_at_phv)

    metric_values <- selected_metric_data() |>
      group_by(test, assess_date) |>
      summarise(metric_value = mean(value, na.rm = TRUE), .groups = "drop")

    out <- base |>
      left_join(metric_values, by = c("test", "assess_date")) |>
      mutate(
        assess_date = fmt_date(assess_date),
        across(where(is.numeric), ~ round(.x, d))
      ) |>
      rename(
        `Athlete ID` = ID,
        `Group` = Group,
        `Test` = test,
        `Assessment Date` = assess_date,
        `Age (years)` = age_decimal,
        `Maturity Offset (years)` = maturity_offset,
        `Estimated Age at PHV (years)` = age_at_phv
      )

    metric_col_name <- pretty_metric(input$metric)
    names(out)[names(out) == "metric_value"] <- metric_col_name

    datatable(
      out,
      filter = "top",
      options = list(
        pageLength = 15,
        lengthMenu = list(c(10, 15, 25, 50, 100), c("10", "15", "25", "50", "100")),
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })

  output$missing_dob_table <- renderDT({
    req(data_store())
    data_store()$missing_dob |>
      rename(`Athlete ID` = ID) |>
      datatable(
        options = list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE),
        rownames = FALSE
      )
  })

  outputOptions(output, "latest_score", suspendWhenHidden = FALSE)
  outputOptions(output, "latest_age", suspendWhenHidden = FALSE)
  outputOptions(output, "latest_offset", suspendWhenHidden = FALSE)
  outputOptions(output, "latest_session_date", suspendWhenHidden = FALSE)

  observe({
    msg <- data_error()
    if (!is.null(msg)) {
      showNotification(msg, type = "error", duration = 8)
    }
  })
}


shinyApp(ui, server)
