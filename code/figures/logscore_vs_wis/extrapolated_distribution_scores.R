library(tidyverse)
library(ggpubr)

#' Extrapolate density
d_ext_factory <- function(ps, qs, dist) {
  ddst <- get(paste0("d", dist))
  qdst <- get(paste0("q", dist))
  b <- (qs[2] - qs[1]) / (qdst(ps[2]) - qdst(ps[1]))
  a <- qs[1] - b * qdst(ps[1])
  
  d_ext <- function(x, log) {
    result <- ddst((x - a) / b, log = TRUE) - log(b)
    if (log) {
        return(result)
    } else {
        return(exp(result))
    }
  }
  
  return(d_ext)
}

#' Extrapolate cdf
p_ext_factory <- function(ps, qs, dist) {
  pdst <- get(paste0("p", dist))
  qdst <- get(paste0("q", dist))
  b <- (qs[2] - qs[1]) / (qdst(ps[2]) - qdst(ps[1]))
  a <- qs[1] - b * qdst(ps[1])
  
  p_ext <- function(x) {
    return(pdst((x - a) / b))
  }
  
  return(p_ext)
}

#' Extrapolate quantiles
q_ext_factory <- function(ps, qs, dist) {
  qdst <- get(paste0("q", dist))
  b <- (qs[2] - qs[1]) / (qdst(ps[2]) - qdst(ps[1]))
  a <- qs[1] - b * qdst(ps[1])
  
  q_ext <- function(p) {
    return(a + b * qdst(p))
  }
  
  return(q_ext)
}

# Suppose X = a + b * Z
# P(X <= q) = P(Z <= (q - a) / b)

# Therefore, if q_x(p) is the quantile function for X and q_z(p) is the quantile function for Z,
# then q_z(p) = [q_x(p) - a] / b, and rearranging, q_x(p) = a + b * q_z(p)

# Therefore, for two probability levels p1 and p2,
# (q_x(p2) - q_x(p1)) / (q_z(p2) - q_z(p1))
#  = [a + b * q_z(p2) - (a + b * q_z(p1))] / (q_z(p2) - q_z(p1))
#  = b * [q_z(p2) - q_z(p1)] / (q_z(p2) - q_z(p1))
#  = b

# Also, q_x(p1) - b * q_z(p1) = a + b * q_z(p1) - b * q_z(p1) = a

# P(X <= q_x(p1) + (q_z(p) - q_z(p1)) * b)
#  = P(X <= a + b * q_z(p1) + b * q_z(p) - b * q_z(p1))
#  = P(X <= a + b * q_z(p))
#  = P(X <= q_x(p))
#  = p


# P(Z <= [q1 + (q_z - q1) * b - a] / b)
# P(Z <= (q_z*s - a) / b) + (q1 - a) / b + (q1 * s - a) / b) )



# #' Extrapolate into the tail of a distribution via
# q_ext <- function(prob, pl1, pl2, qv1, qv2, dist) {
#   qdst <- get(paste0("q", dist))
#   q_new <- qv1 + (qdst(prob) - qdst(pl1)) * (qv2 - qv1) / (qdst(pl2) - qdst(pl1))
#   return(q_new)
# }


#' Calculates approximate probability density function values by interpolating
#' and extrapolating from a set of quantiles from the distribution.
#'
#' @param x vector of values at which to calculate the pdf
#' @param ps vector of probability levels at which quantiles are observed
#' @param qs vector of observed quantile values correponding to ps
#' @param tail_dist_lower name of parametric distribution for the lower tail
#' @param tail_dist_upper name of parametric distribution for the upper tail
#'
#' @return values of the approximate density function at the points x
approx_pdf_from_quantiles <- function(x, ps, qs, tail_dist_lower, tail_dist_upper, log = FALSE) {
    # drop missing values for qs
    na_idx = is.na(qs)
    if (any(na_idx)) {
        ps = ps[!na_idx]
        qs = qs[!na_idx]
    }
    
    # short-circuit if less than two unique value in qs
    if (length(unique(qs)) < 2) {
        return(rep(qs, length(p)))
    }
    
    # sort ps and qs
    ps <- sort(ps)
    qs <- sort(qs)
    
    # instantiate result
    result <- rep(NA_real_, length(p))
    
    # interior points
    interior_idx <- (x >= qs[1]) & (x <= tail(qs, 1))
    if (any(interior_idx)) {
        interior_pdf <- stats::splinefun(qs, ps, method = "monoH.FC")
        # interior_pdf <- stats::splinefun(qs, ps, method = "hyman")
        result[interior_idx] <- interior_pdf(x[interior_idx], deriv = 1)
        if (log) {
            result[interior_idx] <- log(result[interior_idx])
        }
    }
    
    # lower points
    lower_idx <- (x < qs[1])
    if (any(lower_idx)) {
        lower_pdf <- d_ext_factory(head(ps, 2), head(qs, 2), tail_dist_lower)
        result[lower_idx] <- lower_pdf(x[lower_idx], log = log)
    }

    # upper points
    upper_idx <- (x > tail(qs, 1))
    if (any(upper_idx)) {
        upper_pdf <- d_ext_factory(tail(ps, 2), tail(qs, 2), tail_dist_upper)
        result[upper_idx] <- upper_pdf(x[upper_idx], log = log)
    }
    
    return(result)
}



#' Calculates approximate cumulative density function values by interpolating
#' and extrapolating from a set of quantiles from the distribution.
#'
#' @param x vector of values at which to calculate the cdf
#' @param ps vector of probability levels at which quantiles are observed
#' @param qs vector of observed quantile values correponding to ps
#' @param tail_dist_lower name of parametric distribution for the lower tail
#' @param tail_dist_upper name of parametric distribution for the upper tail
#'
#' @return values of the approximate density function at the points x
approx_cdf_from_quantiles <- function(x, ps, qs, tail_dist_lower, tail_dist_upper) {
    # drop missing values for qs
    na_idx = is.na(qs)
    if (any(na_idx)) {
        ps = ps[!na_idx]
        qs = qs[!na_idx]
    }
    
    # short-circuit if less than two unique value in qs
    if (length(unique(qs)) < 2) {
        return(rep(qs, length(p)))
    }
    
    # sort ps and qs
    ps <- sort(ps)
    qs <- sort(qs)
    
    # instantiate result
    result <- rep(NA_real_, length(p))
    
    # interior points
    interior_idx <- (x >= qs[1]) & (x <= tail(qs, 1))
    if (any(interior_idx)) {
        interior_cdf <- stats::splinefun(qs, ps, method = "monoH.FC")
        # interior_cdf <- stats::splinefun(qs, ps, method = "hyman")
        result[interior_idx] <- interior_cdf(x[interior_idx], deriv = 0)
    }
    
    # lower points
    lower_idx <- (x < qs[1])
    if (any(lower_idx)) {
        lower_cdf <- p_ext_factory(head(ps, 2), head(qs, 2), tail_dist_lower)
        result[lower_idx] <- lower_cdf(x[lower_idx])
    }

    # upper points
    upper_idx <- (x > tail(qs, 1))
    if (any(upper_idx)) {
        upper_cdf <- p_ext_factory(tail(ps, 2), tail(qs, 2), tail_dist_upper)
        result[upper_idx] <- upper_cdf(x[upper_idx])
    }
    
    return(result)
}



#' Calculates approximate quantile function values by interpolating
#' and extrapolating from a set of quantiles from the distribution.
#'
#' @param p vector of probability levels at which to calculate quantiles
#' @param ps vector of probability levels at which quantiles are observed
#' @param qs vector of observed quantile values correponding to ps
#' @param tail_dist_lower name of parametric distribution for the lower tail
#' @param tail_dist_upper name of parametric distribution for the upper tail
#'
#' @return a function with argument x that calculates approximated pdf values
approx_qf_from_quantiles <- function(p, ps, qs, tail_dist_lower, tail_dist_upper) {
    # drop missing values for qs
    na_idx = is.na(qs)
    if (any(na_idx)) {
        ps = ps[!na_idx]
        qs = qs[!na_idx]
    }
    
    # short-circuit if less than two unique value in qs
    if (length(unique(qs)) < 2) {
        return(rep(qs, length(p)))
    }
    
    # sort ps and qs
    ps <- sort(ps)
    qs <- sort(qs)
    
    # instantiate result
    result <- rep(NA_real_, length(p))
    
    # interior points
    interior_idx <- (p >= ps[1]) & (p <= tail(ps, 1))
    if (any(interior_idx)) {
        # interior_qf <- stats::splinefun(ps, qs, method = "hyman")
        interior_qf <- stats::splinefun(ps, qs, method = "monoH.FC")
        result[interior_idx] <- interior_qf(p[interior_idx])
    }
    
    # lower points
    lower_idx <- (p < ps[1])
    if (any(lower_idx)) {
        lower_qf <- q_ext_factory(head(ps, 2), head(qs, 2), tail_dist_lower)
        result[lower_idx] <- lower_qf(p[lower_idx])
    }

    # upper points
    upper_idx <- (p > tail(ps, 1))
    if (any(upper_idx)) {
        upper_qf <- q_ext_factory(tail(ps, 2), tail(qs, 2), tail_dist_upper)
        result[upper_idx] <- upper_qf(p[upper_idx])
    }
    
    return(result)
}

# p_grid <- seq(from = 0.001, to = 0.999, by = .0005)
# q_imputed <- approx_qf_from_quantiles(
#     p = p_grid,
#     ps = ps, qs = qs,
#     tail_dist_lower = "norm", tail_dist_upper = "norm")

# ggplot(data = data.frame(p = p_grid, q = q_imputed)) +
#     geom_line(mapping = aes(x = p, y = q)) +
#     geom_point(data = data.frame(p = ps, q = qs), mapping = aes(x = p, y = q))



# x <- seq(from = 0.001, to = 1000, length = 1000)
# pdf_imputed <- approx_pdf_from_quantiles(
#     x = x,
#     ps = ps, qs = qs,
#     tail_dist_lower = "norm", tail_dist_upper = "norm")

# ggplot(data = data.frame(x = x, y = pdf_imputed)) +
#     geom_line(mapping = aes(x = x, y = y))


quantile_probs <- c(0.01, 0.025, seq(from = 0.05, to = 0.95, by = 0.05), 0.975, 0.99)

meanlog <- 4.0
sdlog <- 0.5
q_lognormal <- qlnorm(quantile_probs, meanlog = meanlog, sdlog = sdlog)

x <- seq(from = 0.0, to = 400.0, length = 501)

cdf_lognormal <- plnorm(x, meanlog = meanlog, sdlog = sdlog)
cdf_imputed_normal <- approx_cdf_from_quantiles(
    x = x,
    ps = quantile_probs, qs = q_lognormal,
    tail_dist_lower = "norm", tail_dist_upper = "norm")
cdf_imputed_cauchy <- approx_cdf_from_quantiles(
    x = x,
    ps = quantile_probs, qs = q_lognormal,
    tail_dist_lower = "cauchy", tail_dist_upper = "cauchy")

p_cdf <- dplyr::bind_rows(
    data.frame(
        x = x,
        y = cdf_lognormal,
        density = "Log normal"
    ),
    data.frame(
        x = x,
        y = cdf_imputed_normal,
        density = "Spline interpolation,\nnormal tails"
    ),
    data.frame(
        x = x,
        y = cdf_imputed_cauchy,
        density = "Spline interpolation,\nCauchy tails"
    )
) %>%
    dplyr::mutate(facet_label = "Cumulative Distribution Function") %>%
    ggplot() +
        geom_line(
            mapping = aes(x = x, y = y, color = density, linetype = density),
            size = 0.8) +
        geom_point(
            data = data.frame(q = q_lognormal, p = quantile_probs),
            mapping = aes(x = q, y = p),
            size = 1.2
        ) +
        facet_wrap(~ facet_label) +
        scale_color_viridis_d(
            "Forecast Distribution",
            end = 0.9
        ) +
        scale_linetype_discrete("Forecast Distribution") +
        ylab("Probability") +
        xlab("") +
        theme_bw() +
        theme(
            # legend.position = "none",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank()
        )

p_legend <- ggpubr::get_legend(p_cdf, position = "bottom")
p_cdf <- p_cdf + theme(legend.position = "none")


pdf_lognormal <- dlnorm(x, meanlog = meanlog, sdlog = sdlog)
pdf_imputed_normal <- approx_pdf_from_quantiles(
    x = x,
    ps = quantile_probs, qs = q_lognormal,
    tail_dist_lower = "norm", tail_dist_upper = "norm")
pdf_imputed_cauchy <- approx_pdf_from_quantiles(
    x = x,
    ps = quantile_probs, qs = q_lognormal,
    tail_dist_lower = "cauchy", tail_dist_upper = "cauchy")

p_pdf <- dplyr::bind_rows(
    data.frame(
        x = x,
        y = pdf_lognormal,
        density = "Log normal"
    ),
    data.frame(
        x = x,
        y = pdf_imputed_normal,
        density = "Spline interpolation,\nnormal tails"
    ),
    data.frame(
        x = x,
        y = pdf_imputed_cauchy,
        density = "Spline interpolation,\nCauchy tails"
    )
) %>%
    dplyr::mutate(facet_label = "Probability Density Function") %>%
    ggplot() +
        geom_line(
            mapping = aes(x = x, y = y, color = density, linetype = density),
            size = 0.8
        ) +
        scale_color_viridis_d(
            "Forecast Distribution",
            end = 0.9
        ) +
        ylab("Density") +
        facet_wrap(~ facet_label) +
        theme_bw() +
        theme(
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank()
        )


logscore_lognormal <- dlnorm(x, meanlog = meanlog, sdlog = sdlog, log = TRUE)
logscore_imputed_normal <- approx_pdf_from_quantiles(
    x = x,
    ps = quantile_probs, qs = q_lognormal,
    tail_dist_lower = "norm", tail_dist_upper = "norm",
    log = TRUE)
logscore_imputed_cauchy <- approx_pdf_from_quantiles(
    x = x,
    ps = quantile_probs, qs = q_lognormal,
    tail_dist_lower = "cauchy", tail_dist_upper = "cauchy",
    log = TRUE)

p_logscore <- dplyr::bind_rows(
    data.frame(
        x = x,
        y = -1.0 * logscore_lognormal,
        density = "Log normal"
    ),
    data.frame(
        x = x,
        y = -1.0 * logscore_imputed_normal,
        density = "Spline interpolation,\nnormal tails"
    ),
    data.frame(
        x = x,
        y = -1.0 * logscore_imputed_cauchy,
        density = "Spline interpolation,\nCauchy tails"
    )
) %>%
    dplyr::mutate(facet_label = "Logarithmic Score") %>%
    ggplot() +
        geom_line(
            mapping = aes(x = x, y = y, color = density, linetype = density),
            size = 0.8
        ) +
        scale_color_viridis_d(
            "Forecast Distribution",
            end = 0.9
        ) +
        # coord_cartesian(ylim = c(-15.0, 0.0)) +
        # ylab("Negative Log Score") +
        facet_wrap(~ facet_label) +
        theme_bw() +
        theme(
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank()
        )


qfm <- matrix(q_lognormal, nrow = 1)
attributes(qfm) <- c(
    attributes(qfm),
    list(
        row_index = data.frame("a", stringsAsFactors = FALSE),
        col_index = data.frame(
            q_prob = as.character(quantile_probs),
            model = "all",
            stringsAsFactors = FALSE),
        model_col = "model",
        quantile_name_col = "q_prob",
        quantile_value_col = "q_val"
    )
)
wis_all <- sapply(x, function(one_x) { covidEnsembles::wis(y = one_x, qfm = qfm) })

p_wis <- dplyr::bind_rows(
    data.frame(
        x = x,
        y = wis_all,
        density = "Log Normal"
    ),
    data.frame(
        x = x,
        y = wis_all,
        density = "Spline interpolation,\nnormal tails"
    ),
    data.frame(
        x = x,
        y = wis_all,
        density = "Spline interpolation,\nCauchy tails"
    )
) %>%
    dplyr::mutate(facet_label = "Weighted Interval Score") %>%
    ggplot() +
        geom_line(mapping = aes(x = x, y = y, color = density, linetype = density), size = 0.8) +
        scale_color_viridis_d(
            "Forecast Distribution",
            end = 0.9
        ) +
        # coord_cartesian(ylim = c(-15.0, 0.0)) +
        facet_wrap(~ facet_label) +
        # ylab("Weighted Interval Score") +
        theme_bw() +
        theme(
            legend.position = "none",
            axis.title.y = element_blank()
        )


pdf("manuscript/figures/log_score_and_wis.pdf", width = 8, height = 9)
plot_layout <- grid.layout(
  nrow = 6, ncol = 5,
  widths = unit(c(1, 0.009, 0.006, 0.008, 1), c("lines", rep("null", 4))),
  heights = unit(c(1, 1, 1, 1.2, 0.1, 2), c("null", "null", "null", "null", "lines", "lines")))

grid.newpage()
pushViewport(viewport(layout = plot_layout))

# grid.text("(a) Forecasts of incident deaths in Ohio from February 15, 2021",
#   x = unit(0.0, "npc"),
#   just = "left",
#   gp = gpar(fontsize = 12),
#   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))


grid.text("Probability     ",
#   x = unit(0.0, "npc"),
#   just = "left",
  rot = 90,
  gp = gpar(fontsize = 10),
  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_cdf, vp = viewport(layout.pos.row = 1, layout.pos.col = 3:5))
grid.text("Probability Density     ",
#   x = unit(0.0, "npc"),
#   just = "left",
  rot = 90,
  gp = gpar(fontsize = 10),
  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_pdf, vp = viewport(layout.pos.row = 2, layout.pos.col = 2:5))
grid.text("Negative Log Score     ",
#   x = unit(0.0, "npc"),
#   just = "left",
  rot = 90,
  gp = gpar(fontsize = 10),
  vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(p_logscore, vp = viewport(layout.pos.row = 3, layout.pos.col = 5))
grid.text("Weighted Interval Score",
#   x = unit(0.0, "npc"),
#   just = "left",
  rot = 90,
  gp = gpar(fontsize = 10),
  vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(p_wis, vp = viewport(layout.pos.row = 4, layout.pos.col = 4:5))
print(as_ggplot(p_legend), vp = viewport(layout.pos.row = 6, layout.pos.col = 2:5))

dev.off()




