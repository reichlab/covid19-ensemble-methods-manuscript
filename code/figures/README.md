Files in this folder:

 * Figures used in main manuscript:
    * `plot_ensemble_motivation.R`: a plot with an example of outlying forecasts and instability of relative component WIS. Used as figure 1 in the manuscript.  Additionally, produces plots of the relative WIS ranks by forecast date for all component forecasters, used in the supplement.
    * `plot_data_eval_phases.R`: plots of the data and forecasts for some example locations. Used as figure 2 in the manuscript
    * `plot_wis_boxplots_calibration.R`: boxplots of WIS and line plots of quantile calibration. Used for figures 3 and 7 in the manuscript
    * `plot_weights_and_rel_wis_all_combos.R`: plots of component model and ensemble relative WIS over time and component model weights. Used as figures 4 and 5 in the main manuscript. Additionally, plots examining characteristics of the component forecaster weights from the post hoc weighted mean ensemble, used as Supplemental Figure 27
    * `plot_scores_by_max_weight_cap.R`: plot showing how WIS and 95% interval coverage varies as component forecaster weights are regularized by setting a limit on the maximum weight that can be assigned to any individual component. Figure 7 in the manuscript.
    * `plot_compare_EU_US_missingness.R`: multiple plots showing the impacts of forecast missingness on component weights. Used as figure 8 in the main manuscript and in the supplement
 * Figures used in supplement:
    * `plot_component_forecast_medians.R`" plot predictive medians from all component forecasters at selected forecast dates
    * `plot_calibration_quantile_group.R`: plots of forecast calibration varying strategies for sharing weights across different groups of quantile levels
    * `plot_wis_boxplots_horizon_group.R`: plots of ensemble WIS varying strategies for sharing weights across different forecast horizons
    * `plot_wis_boxplots_quantile_group.R`: plots of ensemble WIS varying strategies for sharing weights across different quantile levels
    * `plot_interval_widths.R`: plots of standardized ranks of 95% prediction interval widths.
    * `plot_peak_forecasts_trained_untrained.R`: the files `local_maxima_cases.csv` and `local_maxima_deaths.csv` with identified local peaks in cases and deaths, as well as plots of those local peaks and summaries of forecast skill around those peaks.
    * `plot_anomalies_example.R`: an example plot of identified reporting anomalies
    * `plot_ensemble_cdf_combination.R`: illustration of quantile averaging as a horizontal combination of component cdfs
    * `plot_rel_wis_by_target_end_date.R`: relative WIS over time for selected ensemble methods.
    * `plot_wis_boxplots_all_combination_methods.R`: boxplots comparing WIS scores for many ensemble variations
    * `plot_compare_wis_agg_methods.R`: plots component forecaster relative WIS scores when an arithmetic mean or a geometric mean is used to aggregate scores.
    * `extrapolated_distribution_scores.R`: compares WIS and log scores and illustrates sensitivity of log scores to assumptions about tail behavior.
 * The following files produce figures that we didn't end up using:
    * `plot_calibration_main.R`: some plots of forecast calibration for the primary ensemble variations
    * `plot_wis_boxplots_main.R`: boxplots of WIS for the main ensemble variations
    * `plot_wis_by_target_end_date.R` plots mean WIS for ensemble variations as a function of the forecast's target end date
    * `palette_test.R` was used to explore the color palette for other figures
