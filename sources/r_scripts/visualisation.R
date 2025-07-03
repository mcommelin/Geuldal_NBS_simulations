
# Precip and discharge for 10 & 14 --------------------------------------------


# points
points_id <- c(10, 14)
# event_dates
event_dates <- c("2023-06-22")

# the function
graph_subcatch_qp <- function(points = NULL,
                              event_dates = NULL) {

# events
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end)) %>%
  filter(use == "cal")

points <- read_csv("LISEM_data/setup/outpoints_description.csv")

# rain all events
rain_5min <- read_csv("data/raw_data/neerslag/KNMI_rain_5min.csv")

## precipitation 5 minute resolution

# make loop over subcatch
rain_sub <- vector("list", length = length(points_id))
for (i in seq_along(points_id)) {
  point_id <- points_id[i]
  # select subcatchment
  subcatch <- points %>%
    filter(point == point_id) %>%
    filter(cell_size == 5)
  subcatch_name <- subcatch$subcatch_name
  wdir <- paste0("LISEM_runs/", subcatch_name, "_5m/maps/")
  
  # load rain id's from discharge
  rainIDs <- raster(paste0(wdir, "rain_ID.asc"))
  id <- as.vector(rainIDs)
  freq <- as_tibble(table(id)) %>%
    mutate(id_nm = paste0("gauge_", id))
  
  rain_sub[[i]] <- rain_5min %>%
    select(timestamp, all_of(freq$id))  %>%
    pivot_longer(cols = -timestamp,
                 values_to = "P",
                 names_to = "id") %>%
    left_join(freq, by = "id") %>%
    mutate(Ptmp = P * n) %>%
    group_by(timestamp) %>%
    summarize(P = round(sum(Ptmp) / sum(n), digits = 2)) %>%
    mutate(point = point_id,
           timestamp = timestamp - minutes(5)) # correct KNMI timestamp now data is for the coming 5 minutes
  
}
rain_sub <- bind_rows(rain_sub)

rain <- map2_dfr(events$ts_start, events$ts_end,
                 ~filter(rain_sub, timestamp >= .x & timestamp <= .y)) %>%
  left_join(events, join_by(
    closest(timestamp <= ts_end)
  )) %>%
  mutate(ev_name = as.character(date(ts_start)))

# add discharge
# first make 'dat' in Q_measurements_Geuldal
q_obs <- read_csv("data/processed_data/obs_discharge/observed_discharge_high_res.csv") %>%
  left_join(events, b = c("ev_num", "use")) %>%
  mutate(timestamp = timestamp - minutes(60), # correct to GMT from GMT+1
         ev_name = as.character(date(ts_start)))

# make figures for both subcatch

for (i in seq_along(points_id)) {
  p <- filter(rain, point == points_id[i]) %>%
    filter(ev_num == 9)
  q <- filter(q_obs, point == points_id[i])%>%
    filter(ev_num == 9)
  
  coeff <- 6
  
  ggplot() +
    geom_bar(data = p, aes(x = timestamp, y = P/coeff), stat = "identity") +
    geom_line(data = q, aes(x = timestamp, y = Q, color = code)) +
    facet_wrap(~ ev_name, scales = "free", nrow = 3) +
    labs(color = "Meetpunt") +
    scale_y_continuous("Q m3/sec",
                       sec.axis = sec_axis(~ . * coeff, name = "P mm/h")) +
    theme_classic()
  ggsave(paste0("images/subcatch_observations/q_and_p_", points_id[i], ".png"))
}
} # end graph_subcatch_qp function

# test figure code Miguel

# plot
# axis constants
q_max_round <- ceiling(max(c(q$Q), na.rm = TRUE) / 10) * 10
p_max       <- max(p$P, na.rm = TRUE)
k           <- q_max_round / (p_max * 2)
y_top       <- q_max_round

# plot regualr and inverted y-axis
ggplot() +
  geom_linerange(data = p, aes(x = timestamp, ymin = y_top,
                               ymax = y_top - P * k),
                 fill = "#8D8DAA", alpha = 0.7) +
  #geom_ribbon(aes(ymin = qmin, ymax = qmax), fill = "grey60", alpha = 0.3) +
  geom_line(data = q, aes(x = timestamp, y = Q, color = code), linewidth = 0.3) +
  # geom_line(aes(y = sel_run),           colour = "red", linetype = "dashed", linewidth = 0.8) +
  # axis
  scale_y_continuous(
    name     = "Discharge (m³ s⁻¹)",
    limits   = c(0, y_top),
    sec.axis = sec_axis(
      ~ (y_top - .) / k,
      name = "Precipitation (mm)"
    )
  ) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# load packages
library(tidyverse)
library(scales)
library(lemon)


# data
> head(q_sel)
# A tibble: 6 × 6
date        qmin  qmax   best surfaceflow_obs sel_run
<date>     <dbl> <dbl>  <dbl>           <dbl>   <dbl>
  1 2004-10-01     0 0.310 0.0710               0 0.0127 
2 2004-10-02     0 0.199 0.0705               0 0.0122 
3 2004-10-03     0 0.383 0.0687               0 0.0112 
4 2004-10-04     0 0.284 0.0692               0 0.0102 
5 2004-10-05     0 0.261 0.0683               0 0.00967
6 2004-10-06     0 0.194 0.0680               0 0.00882

> head(precip_130)
# A tibble: 6 × 2
date       precip
<date>      <dbl>
    1 2004-10-01      0
2 2004-10-02      0
3 2004-10-03      0
4 2004-10-04      0
5 2004-10-05      0
6 2004-10-06      0


# plot
# axis constants
q_max_round <- ceiling(max(c(q_sel$surfaceflow_obs, q_sel$qmax, q_sel$sel_run), na.rm = TRUE) / 10) * 10
p_max       <- max(precip_130$precip, na.rm = TRUE)
k           <- q_max_round / (p_max * 2)
y_top       <- q_max_round

# plot regualr and inverted y-axis
q_sel %>%
  left_join(precip_130, by = "date") %>%
  ggplot(aes(date)) +
  geom_linerange(aes(ymin = y_top,
                     ymax = y_top - precip * k),
                 fill = "#8D8DAA", alpha = 0.7) +
  geom_ribbon(aes(ymin = qmin, ymax = qmax), fill = "grey60", alpha = 0.3) +
  geom_line(aes(y = surfaceflow_obs), colour = "#041562", linewidth = 0.3) +
  geom_line(aes(y = sel_run),           colour = "red", linetype = "dashed", linewidth = 0.8) +
  # axis
  scale_y_continuous(
    name     = "Discharge (m³ s⁻¹)",
    limits   = c(0, y_top),
    sec.axis = sec_axis(
      ~ (y_top - .) / k,
      name = "Precipitation (mm)"
    )
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))