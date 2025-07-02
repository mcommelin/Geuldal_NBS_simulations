
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