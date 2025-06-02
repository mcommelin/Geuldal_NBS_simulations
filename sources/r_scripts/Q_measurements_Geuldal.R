#' Calculations of discharge based on cross-sections etc.
#' Based on the book 'Hydrometry' by Boiten.
#' 
#' Velocity measurement based on 1 point method (p82).
#' Assuming that the waterdepth at .6 of the depth is about 
#' the mean.
#' 
#' The following calculation of the discharge is done with the
#' mid section method (p85). Since the cross section bins are not equally spaced.
#'
#'The flow speed is calculated from the equations provided with the flow meter
#'v = velocity of water (m/s)
#'n = number of revolutions per second
#'rotor 2: 
#'n < 5.00 => v = 0.1034 * n + 0.013
#'n > 5.00 => v = 0.1022 * n + 0.019
#'rotor 3:
#'n < 0.69 => v = 0.2330 * n + 0.019
#'n > 0.69 => v = 0.2520 * n + 0.006

# Eyserbeek meetgoot snelle som

# sectie 1 brede deel voor brug
vp1 <- (96/30) * 0.1304 + 0.013
vp2 <- (117.2/30) * 0.1304 + 0.013
vp3 <- (88.2/30) * 0.1304 + 0.013
vp4 <- (91.5/30) * 0.1304 + 0.013
vp5 <- (74/30) * 0.1304 + 0.013
vp6 <- (65/30) * 0.1304 + 0.013

qp1 <- vp1 * 0.15 * 0.5 * (0.6 + 0.5)
qp2 <- vp2 * 0.17 * 0.5 * (0.5 + 0.5)
qp3 <- vp3 * 0.16 * 0.5 * (0.5 + 0.5)
qp4 <- vp4 * 0.18 * 0.5 * (0.5 + 0.5)
qp5 <- vp5 * 0.21 * 0.5 * (0.5 + 0.5)
qp6 <- vp6 * 0.12 * 0.5 * (0.5 + 0.5)

qsum <- (qp1 + qp2 + qp3 + qp4 + qp5 + qp6)

# sectie 2 smalle deel onder brug
a2 <- (1.08 - 0.2) * 0.22
v1 <- (111.3/30) * 0.2520 + 0.006
v2 <- (97/30) * 0.2520 + 0.006
v3 <- (103.6/30) * 0.2520 + 0.006

vmean <- (v1 + v2 + v3) / 3
Q2 <- a2 * vmean

