#######################################################################################
# --- What Drives local-level Tourism in Brazil? Insights from Open Data Analysis --- #
#   * please, open the braces for code snippets                                       #
#######################################################################################

## 1. Data loading
{

# Necessary packages
  {
  if(!require(tidyverse)){install.packages("tidyverse")} # Data manipulation and visualization
  if(!require(tidyr)){install.packages("tidyr")}       # Data tidying
  if(!require(plm)){install.packages("plm")}           # Panel data econometrics
  if(!require(urca)){install.packages("urca")}         # Unit root and cointegration tests
  if(!require(car)){install.packages("car")}           # Companion to Applied Regression
  if(!require(lmtest)){install.packages("lmtest")}     # Likelihood ratio tests and more
  if(!require(sandwich)){install.packages("sandwich")}   # Robust standard errors
  if(!require(tseries)){install.packages("tseries")}    # Time series analysis tools
  if(!require(patchwork)){install.packages("patchwork")} # Combining plots
  if(!require(ggcorrplot)){install.packages("ggcorrplot")}# Correlation plot
  if(!require(ggplot2)){install.packages("ggplot2")}    # Plotting package
  } 

# loading data, see supplementary file
{
#libraries
library(tidyverse)
library (dplyr)
#reading csv
  bra_tour <- read_csv2("C:/dados/bra_tour") #change according to your directory 
#inspecting dataframe
glimpse(bra_tour)
#variables summary
summary(bra_tour)
}

#logging data since it is skewed
{
bra_tour_log <- bra_tour |> 
  mutate(across(-c(year, id),  ~log(.+0.01)))
glimpse(bra_tour_log)
summary(bra_tour_log)
}

#ploting yearly aggregated mean time series

{ # Calculate yearly aggregated mean
  agg_series_mean <- bra_tour |> 
    group_by(year)  |> 
    summarize(mean_htl=mean(htl, na.rm=TRUE),
              mean_htl_esp=mean(htl_esp, na.rm=TRUE),
              mean_gmp = mean(gmp, na.rm=TRUE),
              mean_gmp_srv_pct = mean(gmp_srv_pct, na.rm=TRUE),
              mean_tax_adm = mean(tax_adm, na.rm=TRUE),
              mean_bgt = mean(bgt, na.rm=TRUE),
              mean_tourbgt = mean(tourbgt, na.rm=TRUE),
              mean_tnf_pct = mean(tnf_pct, na.rm=TRUE),
              mean_loans = mean(loans, na.rm=TRUE),
              mean_n_bsns = mean(n_bsns, na.rm=TRUE),
              mean_flypax = mean(flypax, na.rm=TRUE),
              mean_w_inet  = mean(w_inet , na.rm=TRUE),
              mean_educ = mean(educ, na.rm=TRUE),
              mean_tca_hhi = mean(tca_hhi, na.rm=TRUE),
              .groups = 'drop')
  glimpse(agg_series_mean)
  
  library(tidyr)
  library(ggplot2)
  
  
  # List of variable columns to plot (excluding 'year' and 'shock')
  vars_to_plot <- c("mean_htl", "mean_htl_esp","mean_tca_hhi", "mean_gmp", "mean_gmp_srv_pct",
                    "mean_tax_adm", "mean_bgt", "mean_tourbgt", "mean_tnf_pct",
                    "mean_loans", "mean_n_bsns", "mean_flypax", "mean_w_inet",
                    "mean_educ" )
  
  # Reshape data to long format
  long_df <- agg_series_mean %>%
    pivot_longer(cols = all_of(vars_to_plot),
                 names_to = "variable",
                 values_to = "value") 
  long_df$variable <- factor(long_df$variable, levels = vars_to_plot)
  
  #plot
  ggplot(long_df, aes(x = year, y = value, color = variable)) +
    geom_line() +
    facet_wrap(~ variable, scales = "free_y") +
    theme_bw() +  # White background
    theme(
      strip.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",  
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)  # Vertical labels
    ) +
    labs(title = "Time Series of Multiple Variables",
         x = "Year",
         y = "Value") +
    scale_x_continuous(breaks = unique(long_df$year))
  
  
 # ggsave("TSPLOT.png", plot = last_plot(), width=10, height=8)
 
}
}

##2. Data processing and manipulation

{

  # Run the unit root augmented Dickey Fuller test with trend on the mean of yearly aggregated data
   {
  library(urca)
      
       htl <- ur.df(agg_series_mean$mean_htl, type='trend', selectlags='AIC')
       htl_esp <- ur.df(agg_series_mean$mean_htl_esp, type='trend', selectlags='AIC')
       tca_hhi <- ur.df(agg_series_mean$mean_tca_hhi, type='trend', selectlags='AIC')
       tourbgt <- ur.df(agg_series_mean$mean_tourbgt, type='trend', selectlags='AIC')
       bgt <- ur.df(agg_series_mean$mean_bgt, type='trend', selectlags='AIC')
       tnf_pct <- ur.df(agg_series_mean$mean_tnf_pct, type='trend', selectlags='AIC')
       gmp <- ur.df(agg_series_mean$mean_gmp, type='trend', selectlags='AIC')
       gmp_srv_pct <- ur.df(agg_series_mean$mean_gmp_srv_pct, type='trend', selectlags='AIC')
       tax_adm <- ur.df(agg_series_mean$mean_tax_adm, type='trend', selectlags='AIC')
       loans <- ur.df(agg_series_mean$mean_loans, type='trend', selectlags='AIC')
       n_bsns <- ur.df(agg_series_mean$mean_n_bsns, type='trend', selectlags='AIC')
       flypax <- ur.df(agg_series_mean$mean_flypax, type='trend', selectlags='AIC')
       w_inet <- ur.df(agg_series_mean$mean_w_inet, type='trend', selectlags='AIC')
       educ <- ur.df(agg_series_mean$mean_educ, type='trend', selectlags='AIC')
       
       print(summary(htl))
       print(summary(htl_esp))
       print(summary(tca_hhi))
       print(summary(tourbgt))
       print(summary(bgt))
       print(summary(tnf_pct))
       print(summary(gmp))
       print(summary(gmp_srv_pct))
       print(summary(tax_adm))
       print(summary(loans))
       print(summary(n_bsns))
       print(summary(flypax))
       print(summary(w_inet))
       print(summary(educ))
      }
    
 # Data is non-stationary, hence first difference transformation
  {
    delta_bra_tour_log <- bra_tour_log |> 
      arrange(id, year) |> 
      group_by(id) |> 
      mutate(htl_1d = as.numeric(htl) - dplyr::lag(as.numeric(htl), order_by = year), 
             htl_esp_1d = as.numeric(htl_esp) - dplyr::lag(as.numeric(htl_esp), order_by = year),
             tca_hhi_1d = as.numeric(tca_hhi) - dplyr::lag(as.numeric(tca_hhi), order_by = year),
             tourbgt_1d = as.numeric(tourbgt) - dplyr::lag(as.numeric(tourbgt), order_by = year),
             bgt_1d = as.numeric(bgt) - dplyr::lag(as.numeric(bgt), order_by = year),
             tnf_pct_1d = as.numeric(tnf_pct) - dplyr::lag(as.numeric(tnf_pct), order_by = year),
             gmp_1d = as.numeric(gmp) - dplyr::lag(as.numeric(gmp), order_by = year),
             gmp_srv_pct_1d = as.numeric(gmp_srv_pct) - dplyr::lag(as.numeric(gmp_srv_pct), order_by = year),
             tax_adm_1d = as.numeric(tax_adm) - dplyr::lag(as.numeric(tax_adm), order_by = year),
             loans_1d = as.numeric(loans) - dplyr::lag(as.numeric(loans), order_by = year),
             n_bsns_1d = as.numeric(n_bsns) - dplyr::lag(as.numeric(n_bsns), order_by = year),
             flypax_1d = as.numeric(flypax) - dplyr::lag(as.numeric(flypax), order_by = year),
             w_inet_1d = as.numeric(w_inet) - dplyr::lag(as.numeric(w_inet), order_by = year),
             educ_1d = as.numeric(educ) - dplyr::lag(as.numeric(educ), order_by = year),
      )|>
      filter(!is.na(htl_1d) & !is.na(gmp_1d))
 glimpse(delta_bra_tour_log) 
   }
 #creating 'shock', time dummy
  {
    delta_bra_tour_log$year <- as.numeric(as.character(delta_bra_tour_log$year))
  delta_bra_tour_log <- delta_bra_tour_log |> mutate(shock = ifelse (year < 2015,0, 1))
  glimpse(delta_bra_tour_log) 
  }
  
 #defining dataframe as panel data
  {
  
  #selecting only first differenced data
  
  dbtl <- delta_bra_tour_log |> select(id, year,htl_1d,
                         htl_esp_1d, 
                         tca_hhi_1d, 
                         gmp_1d, 
                         gmp_srv_pct_1d, 
                         bgt_1d,
                         tourbgt_1d,
                         tnf_pct_1d,
                         loans_1d,
                         n_bsns_1d,
                         flypax_1d,
                         w_inet_1d,
                         educ_1d,
                         shock)
  #assigning data a panel structure
  library(plm)
  dbtl <- pdata.frame(dbtl, index = c("id", "year"))
  #checking if its balanced
  is.pbalanced(dbtl)
  #checking for NAs
  anyNA(dbtld)
  #reinspecting data frame
  glimpse(dbtl)
  
  }

  #Cross sectional dependency test
  { 
  library(plm)
  
  #Pesaran cross-sectional dependecy test
  model_w <- plm(htl_1d ~  htl_esp_1d+
                      tca_hhi_1d+ 
                      gmp_1d + 
                      gmp_srv_pct_1d + 
                      bgt_1d+
                      tourbgt_1d+
                      tnf_pct_1d+
                      loans_1d+
                      n_bsns_1d+
                      flypax_1d+
                      w_inet_1d+
                      educ_1d,
                    data= dbtl,
                    model= "within",
                    index= c("id","year"))
  summary(model_w)
  pcdtest(model_w)
  }
  #Checking for multicollinearity
  { 
  #ploting a correlation matrix
  library(ggcorrplot)
  # Select only numeric columns for correlation
  numeric_columns <- sapply(dbtl, is.numeric)
  dblt_numeric <- dbtl[, numeric_columns]
  
  #computing correlation matrix
  cor_matrix <- cor(dblt_numeric)
  # assigning correlation matrix to a dataframe
  cor_df <- as.data.frame(as.table(cor_matrix))
  #viewing correlation matrix
  View(cor_df)
  #ploting correlation matrix
  ggcorrplot(cor_matrix, lab = TRUE, lab_size =2, ggtheme = theme_minimal())
  
  #calculating Variation Inflation Index (VIF)
  
  library(car)
  model_pool <- plm(htl_1d ~  htl_esp_1d+
                   tca_hhi_1d+ 
                   gmp_1d + 
                   gmp_srv_pct_1d + 
                   bgt_1d+
                   tourbgt_1d+
                   tnf_pct_1d+
                   loans_1d+
                   n_bsns_1d+
                   flypax_1d+
                   w_inet_1d+
                   educ_1d,
                 data= dbtl,
                 model= "pooling",
                 index= c("id","year"))
  summary(model_pool)
  vif_values <- vif(model_pool)
  print(vif_values)
  }

  #Checking for heteroskedasticity
  { 
  library(lmtest)
  #extracting residuals
  residuals_w <- residuals(model_w)
  fitted_w<- fitted(model_w)
  residuals_wdf <- data.frame(
  residuals = residuals_w,
  fitted = fitted_w)
  #running breusch-pagan test
  bp_test <- bptest(residuals ~ fitted, data = residuals_wdf)
  print(bp_test)
  }

  #Running Hausman test
  {
  #Estimating random model
  model_rdm <- plm(htl_1d ~  htl_esp_1d+
                   tca_hhi_1d+ 
                   gmp_1d + 
                   gmp_srv_pct_1d + 
                   bgt_1d+
                   tourbgt_1d+
                   tnf_pct_1d+
                   loans_1d+
                   n_bsns_1d+
                   flypax_1d+
                   w_inet_1d+
                   educ_1d,
                 data= dbtl,
                 model= "random",
                 index= c("id","year"))
  summary(model_rdm)
  
  #Computing Hausman test, comparing fixed effects and random effects models
  hausman_test <- phtest(model_w, model_rdm)
  print(hausman_test)
  }

  # Transforming data with Empirical Cumulative Distribution Functio (ECDF)
  {
#--Further we will apply Quantile regression and also model
#--data transformed by Empirical Cumulative Distribution Function (ECDF),
#--so we will already calculate this transformations for the models to be in order afterwards


  #Demean data, since quantile does not run on fixed effects
    {
  delta_bra_tour_log_demean <- delta_bra_tour_log |> 
    group_by(id) |> 
    mutate(htl_1d = htl_1d - mean(htl_1d, na.rm=TRUE),
           htl_esp_1d = htl_esp_1d - mean(htl_esp_1d, na.rm=TRUE),
           tca_hhi_1d = tca_hhi_1d - mean(tca_hhi_1d, na.rm=TRUE),
           gmp_1d = gmp_1d - mean(gmp_1d, na.rm = TRUE),
           gmp_srv_pct_1d = gmp_srv_pct_1d - mean(gmp_srv_pct_1d, na.rm = TRUE),
           bgt_1d = bgt_1d - mean(bgt_1d, na.rm = TRUE), 
           tourbgt_1d = tourbgt_1d - mean(tourbgt_1d, na.rm = TRUE),
           tnf_pct_1d  =tnf_pct_1d - mean(tnf_pct_1d, na.rm = TRUE),
           loans_1d = loans_1d - mean(loans_1d, na.rm = TRUE),
           n_bsns_1d= n_bsns_1d - mean(n_bsns_1d, na.rm = TRUE),
           flypax_1d = flypax_1d - mean(flypax_1d, na.rm = TRUE),
           w_inet_1d = w_inet_1d - mean(w_inet_1d, na.rm = TRUE),
           educ_1d = educ_1d- mean(educ_1d, na.rm = TRUE)) |> 
    ungroup()
  
  dbtld <-delta_bra_tour_log_demean |>
    select(id, 
           year,
           htl_1d,
           htl_esp_1d, 
           tca_hhi_1d, 
           gmp_1d,
           gmp_srv_pct_1d,
           bgt_1d,
           tourbgt_1d,
           tnf_pct_1d,
           loans_1d,
           n_bsns_1d,
           flypax_1d,
           w_inet_1d,
           educ_1d,
           shock)
    }
  #ecdf distribution
    {
  # selecting variables
  ecdf_transform <- dbtld|> select( 
    htl_1d,
    htl_esp_1d, 
    tca_hhi_1d, 
    gmp_1d,
    gmp_srv_pct_1d,
    bgt_1d,
    tourbgt_1d,
    tnf_pct_1d,
    loans_1d,
    n_bsns_1d,
    flypax_1d,
    w_inet_1d,
    educ_1d)
  #applying ecdf transformation
    for (var in names(ecdf_transform)) {
    ecdf_fn <- ecdf(ecdf_transform[[var]])
    dbtld[[paste0("ecdf_", var)]] <- ecdf_fn(ecdf_transform[[var]])
      }
  #inspecting results
  glimpse(dbtld)
    }
  }
}

##3. Modeling
{
  #Model A
  {
    #since model_w bp_test show it is heteroscedastic, let's apply Driscoll Kraay robust estimator
    model_A <- plm(htl_1d ~ htl_esp_1d+ 
                            tca_hhi_1d+ 
                            gmp_1d + 
                            gmp_srv_pct_1d + 
                            bgt_1d+
                            tourbgt_1d+
                            tnf_pct_1d+
                            loans_1d+
                            n_bsns_1d+
                            flypax_1d+
                            w_inet_1d+
                            educ_1d+
                            shock,
                          data= dbtld,
                          model= "within",
                          index= c("id","year"))
    
    summary_original_A <- summary(model_A)  #preliminary before correction
    print("Original Model Summary (before robust correction):")
    print(summary_original_A)
    
    # Calculate Driscoll-Kray Robust Standard Errors
    library(sandwich)
    library(lmtest)
    robust_vcov_A <- vcovDC(model_A, method = "CH")
    
    # Perform Hypothesis Tests with Robust Standard Errors
    robust_results_A <- coeftest(model_A, vcov = robust_vcov_A)  # Apply robust VCV matrix
    print("Results with Driscoll-Kray Robust Standard Errors:")
    print(robust_results_A)
    
        # Perform the Jarque-Bera test on the residuals
    library(tseries)
    #Extract residuals
    residuals_A <- residuals(model_A)
    residuals_A <- as.numeric(residuals_A)
    
    #Jarque_bera 
    jb_test_A <- jarque.bera.test(residuals_A)
    print(jb_test_A)
  }
  
  #Model B

  {
    #FGLS with pggls funcion from plm
    model_B <- pggls(htl_1d ~ htl_esp_1d+ 
                     tca_hhi_1d+ 
                     gmp_1d + 
                     gmp_srv_pct_1d + 
                     bgt_1d+
                     tourbgt_1d+
                     tnf_pct_1d+
                     loans_1d+
                     n_bsns_1d+
                     flypax_1d+
                     w_inet_1d+
                     educ_1d+
                     shock,
                   data= dbtld,
                   model= "within",
                   index= c("id","year"))
    summary(model_B)
    }
  
  #Model C
  {
    glimpse(dbtld)
    #ecdf
    model_C <- plm (ecdf_htl_1d ~ ecdf_htl_esp_1d+ 
                    ecdf_tca_hhi_1d+ 
                    ecdf_gmp_1d + 
                    ecdf_gmp_srv_pct_1d + 
                    ecdf_bgt_1d+
                    ecdf_tourbgt_1d+
                    ecdf_tnf_pct_1d+
                    ecdf_loans_1d+
                    ecdf_n_bsns_1d+
                    ecdf_flypax_1d+
                    ecdf_w_inet_1d+
                    ecdf_educ_1d+
                     shock,
                   data= dbtld,
                   model= "pooling", #data is already demeaned
                   index= c("id","year"))
   summary(model_C)
   
   # Perform the Jarque-Bera test on the residuals
      #Extract residuals
   residuals_C <- residuals(model_C)
   residuals_C <- as.numeric(residuals_C)
   
   #Jarque_bera 
   jb_test_C <- jarque.bera.test(residuals_C)
   print(jb_test_C)
   
   #Calculating Driscoll Kraay robust estimator

   # computing robust variance-covariance matrix using vcovDC 
   robust_vcov_C <- vcovDC(model_C, method = "CH")
   
   # Perform hypothesis tests using coeftest 
   coeftest(model_C, vcov = robust_vcov_C)
   
    }
  
  # Models A,B,C residual plot
  { library(ggplot2)
    library(patchwork)
    
    #models list
    models <- list(
      "Model_A (Driscol-Kraay)" = model_A,
      "Model_B (fgls)" = model_B,
      "Model_C (ecdf)" = model_C)
    plots <- list()
    #extracting residuals
    for (name in names(models)){
      res <- residuals(models[[name]], type = "normalized")
      fit <- fitted(models[[name]])
      
      # 1 plot) Residuals vs fitted
      p1 <- ggplot(data.frame(Fitted=fit, Resid=res), aes(x=Fitted, y=Resid)) +
        geom_point() +
        geom_hline(yintercept=0, col='red', linetype='dashed') +
        labs(title=paste(name, "- Residuals vs Fitted"),
             x="Fitted Values", y="Residuals") +
        theme_classic()
      # 2 plot) Residuals over Observation order
      df_res <- data.frame(Observation=1:length(res), Residual=res)
      p2 <- ggplot(df_res, aes(x=Observation, y=Residual)) +
        geom_point() +
        geom_hline(yintercept=0, col='red', linetype='dashed') +
        labs(title=paste(name, "- Residuals over Order"),
             y="Observation Number", x="Residuals") +
        theme_classic()
      # 3 plot) Quantile-Quantile plot
      qq <- qqnorm(res, plot.it=FALSE)
      qq_df <- data.frame(qq$x, qq$y)
      colnames(qq_df) <- c("qq_x", "qq_y")
      p3 <- ggplot(qq_df, aes(y=qq_x, x=qq_y)) +
        geom_point() +
        geom_abline(slope=1, intercept=0, col='red') +
        labs(title=paste(name, "- QQ Plot")) +
        theme_classic()
      
      #Save to plots list
      plots[[paste0(name, "_residual_vs_fit")]] <- p1
      plots[[paste0(name, "_res_order")]] <- p2
      plots[[paste0(name, "_qq")]] <- p3
    }
    #Export as jpg 3x3 image
    ggsave("diagnostics_models.jpg", 
           width=15, height=10, units="in",
           plot=wrap_plots(
            (plots[["Model_A (Driscol-Kraay)_residual_vs_fit"]]| plots[["Model_A (Driscol-Kraay)_res_order"]] | plots[["Model_A (Driscol-Kraay)_qq"]]) /
               (plots[["Model_B (fgls)_residual_vs_fit"]] | plots[["Model_B (fgls)_res_order"]] | plots[["Model_B (fgls)_qq"]]) /
               (plots[["Model_C (ecdf)_residual_vs_fit"]] | plots[["Model_C (ecdf)_res_order"]] | plots[["Model_C (ecdf)_qq"]])) 
            , dpi=300)
    }
  
  # Model D, quantile
  { 
    library(quantreg)
  
  # Define the sequence for tau
  tau_seq <-c(0.1, 0.5,0.9)
  
  # Perform quantile regression
  model_D <- rq(htl_1d ~ htl_esp_1d + 
                  tca_hhi_1d + 
                  gmp_1d + 
                  gmp_srv_pct_1d + 
                  bgt_1d +
                  tourbgt_1d +
                  tnf_pct_1d +
                  loans_1d +
                  n_bsns_1d +
                  flypax_1d +
                  w_inet_1d +
                  educ_1d, 
                data = dbtld,
                tau = tau_seq)
  summary(model_D)
  
  #ploting model D residuals and intercepts
  # Define the predictor variables
  predictor_vars <- c("htl_esp_1d", "tca_hhi_1d", "gmp_1d", "gmp_srv_pct_1d",
                      "bgt_1d", "tourbgt_1d", "tnf_pct_1d", "loans_1d",
                      "n_bsns_1d", "flypax_1d", "w_inet_1d", "educ_1d")
  
  # Define the tau values
  taus <- c(0.1, 0.5, 0.9)
  
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each predictor variable
  for (predictor in predictor_vars) {
    # Create the plot for the current predictor
    plot <- ggplot(dbtld, aes(x = .data[[predictor]], y = htl_1d)) +  # Use .data[[predictor]]
      geom_point(alpha = 0.3, size =1) +  # Scatter plot of the data
      
      # Add quantile regression lines
      stat_quantile(aes(color = factor(after_stat(quantile))), formula = y ~ x, quantiles = taus) +
      
      labs(title = paste("htl_1d vs.", predictor),
           x = predictor,
           y = "htl_1d",
           color = NULL) +  # Remove legend title
      theme_classic() +
      scale_color_manual(values = c("0.1" = "green", "0.5" = "blue", "0.9" = "red"),
                         labels = c("tau 0.1", "tau 0.5", "tau 0.9")) + # Fix labels
      guides(color = guide_legend(override.aes = list(linetype = 1))) + # fix the lines
      theme(plot.title = element_text(size = 10),  # Smaller title size
            axis.title = element_text(size = 8),   # Smaller axis title size
            axis.text = element_text(size = 6),    # Smaller axis text size
            legend.text = element_text(size = 6),  # Smaller legend text size
            plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), # Reduced margins
            legend.position = c(0.2, 0.9),
            legend.spacing.y = unit(0.05, "cm"),
            legend.key.size = unit(0.1, "cm"))   # Position legend inside the plot
    
    # Store the plot in the list
    plot_list[[predictor]] <- plot
  }
  
  # Combine the plots into a grid
  combined_plot <- wrap_plots(plot_list, ncol = 3) +
    plot_annotation(#title = "Relationships between htl_1d and Predictors",   # Overall title
      theme = theme(plot.title = element_text(size = 12, hjust = 0.5))) # Center the overall title
  
  # Save the combined plot to an A4-sized PDF
  ggsave("quantile_plot.png", combined_plot,
         width = 21, height = 29.7, units = "cm")  # A4 size in cm
  
  }
  
  # Model E, ecdf quantile
  {
  model_E <- rq(ecdf_htl_1d ~ ecdf_htl_esp_1d + 
                  ecdf_tca_hhi_1d+ 
                  ecdf_gmp_1d + 
                  ecdf_gmp_srv_pct_1d + 
                  ecdf_bgt_1d+
                  ecdf_tourbgt_1d+
                  ecdf_tnf_pct_1d+
                  ecdf_loans_1d+
                  ecdf_n_bsns_1d+
                  ecdf_flypax_1d+
                  ecdf_w_inet_1d+
                  ecdf_educ_1d,
                  data = dbtld,
                  tau = tau_seq)
  summary(model_E)
  
  #plot model E
 
   # Define the predictor variables
  predictor_vars_ecdf <- c("ecdf_htl_esp_1d", "ecdf_tca_hhi_1d", "ecdf_gmp_1d", "ecdf_gmp_srv_pct_1d",
                         "ecdf_bgt_1d", "ecdf_tourbgt_1d", "ecdf_tnf_pct_1d", "ecdf_loans_1d",
                         "ecdf_n_bsns_1d", "ecdf_flypax_1d", "ecdf_w_inet_1d", "ecdf_educ_1d")
  # Create an empty list to store the plots
  plot_list_ecdf <- list()
  # Loop through each predictor variable
  for (predictor in predictor_vars_ecdf) {
    # Create the plot for the current predictor
    plot <- ggplot(dbtld, aes(x = .data[[predictor]], y = ecdf_htl_1d)) +  # Use .data[[predictor]]
      geom_point(alpha = 0.3, size =1) +  # Scatter plot of the data
      
      # Add quantile regression lines
      stat_quantile(aes(color = factor(after_stat(quantile))), formula = y ~ x, quantiles = tau_seq) +
      
      labs(title = paste("ecdf_htl_1d vs.", predictor),
           x = predictor,
           y = "ecdf_htl_1d",
           color = NULL) +  # Remove legend title
      theme_classic() +
      scale_color_manual(values = c("0.1" = "green", "0.5" = "blue", "0.9" = "red"),
                         labels = c("tau 0.1", "tau 0.5", "tau 0.9")) + # Fix labels
      guides(color = guide_legend(override.aes = list(linetype = 1))) + # fix the lines
      theme(plot.title = element_text(size = 10),  # Smaller title size
            axis.title = element_text(size = 8),   # Smaller axis title size
            axis.text = element_text(size = 6),    # Smaller axis text size
            legend.text = element_text(size = 6),  # Smaller legend text size
            plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), # Reduced margins
            legend.position = c(0.2, 0.9),
            legend.spacing.y = unit(0.05, "cm"),
            legend.key.size = unit(0.1, "cm"))   # Position legend inside the plot
    
  # Store the plot in the list
  plot_list_ecdf[[predictor]] <- plot
  }
  
  # Combine the plots into a grid
  combined_plot <- wrap_plots(plot_list_ecdf, ncol = 3) +
    plot_annotation(#title = "Relationships between htl_1d and Predictors",   # Overall title
      theme = theme(plot.title = element_text(size = 12, hjust = 0.5))) # Center the overall title
  
  # Save the combined plot to an A4-sized PDF
  ggsave("quantile_plot_ECDF.png", combined_plot,
         width = 21, height = 29.7, units = "cm")  # A4 size in cm
  }
  
  # Model F, Pvar
  
  #Compute lag variables
  {
    library(data.table)
  
   # Convert to data.table
    setDT(dbtld)
  
   # Arrange data by id and year
   setorder(dbtld, id, year)
  
   # Set key for efficient operations (optional, but recommended for large datasets)
   setkey(dbtld, id, year)
  
   # Create the lagged variables using shift()
    dbtld[, `:=`(
    lag1_htl_1d = shift(htl_1d, n = 1, type = "lag"),
    lag2_htl_1d = shift(htl_1d, n = 2, type = "lag"),
    lag1_htl_esp_1d = shift(htl_esp_1d, n = 1, type = "lag"),
    lag2_htl_esp_1d = shift(htl_esp_1d, n = 2, type = "lag"),
    lag1_tca_hhi_1d = shift(tca_hhi_1d, n = 1, type = "lag"),
    lag2_tca_hhi_1d = shift(tca_hhi_1d, n = 2, type = "lag"),
    lag1_gmp_1d = shift(gmp_1d, n = 1, type = "lag"),
    lag2_gmp_1d = shift(gmp_1d, n = 2, type = "lag"),
    lag1_bgt_1d = shift(bgt_1d, n = 1, type = "lag"),
    lag2_bgt_1d = shift(bgt_1d, n = 2, type = "lag"),
    lag1_tourbgt_1d = shift(tourbgt_1d, n = 1, type = "lag"),
    lag2_tourbgt_1d = shift(tourbgt_1d, n = 2, type = "lag"),
    lag1_loans_1d = shift(loans_1d, n = 1, type = "lag"),
    lag2_loans_1d = shift(loans_1d, n = 2, type = "lag"),
    lag1_n_bsns_1d = shift(n_bsns_1d, n = 1, type = "lag"),
    lag2_n_bsns_1d = shift(n_bsns_1d, n = 2, type = "lag"),
    lag1_educ_1d = shift(educ_1d, n = 1, type = "lag"),
    lag2_educ_1d = shift(educ_1d, n = 2, type = "lag")
  ), by = id]
  
  #inspecting data frame
  glimpse(dbtld)
  
  #exclude NAs
  dbtld<- na.omit(dbtld)
  
  #define as panel data frame
  dbtld <- pdata.frame(dbtld, index =c('id', 'year'))
  
  #ensure all variables are numeric
  dbtld$id <- as.numeric(as.character(dbtld$id))
  
  # Convert 'year' to numeric (or integer)
  dbtld$year <- as.numeric(as.character(dbtld$year))
  
  #reinspecting data frame
  glimpse(dbtld)
 }
  #Model F
  {
  library(panelvar)
    pvar <- pvarfeols(
      dependent_vars =c('htl_1d', 'htl_esp_1d', 'tca_hhi_1d','gmp_1d', 'bgt_1d','tourbgt_1d', 'loans_1d', 'n_bsns_1d', 'educ_1d') ,
      lags = 2,
      data = dbtld,
      #transformation = FALSE, 
      panel_identifier = c('id', 'year'),
      exog_vars = 'shock')
    summary(pvar)
  }
}
      