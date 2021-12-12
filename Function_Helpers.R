library(tidyverse)
library(ggpmisc)
library(stringr)
library(car)
library(utilities)
library(HH)
library(patchwork)
library(EnvStats)
library(olsrr)
library(sjmisc)
library(dgof)

#################################### PLOT LABELING #######################################


get_plot_labels <- function(plot_kind, plot_type_info, extra_info=NULL){
  
  if(plot_kind == "residual" && plot_type_info == "externally_studentized"){
    plot_title <- "Externally Studentized Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Externally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  } 
  else if(plot_kind == "residual" && plot_type_info == "internally_studentized"){
    plot_title <- "Internally Studentized Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Internally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual" && plot_type_info == "regular") {
    plot_title <- "Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel) 
  }
  else if(plot_kind == "residual" && plot_type_info == "deleted"){
    plot_title <- "Deleted (Prediction) Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Deleted (aka Prediction) Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "externally_studentized"){
    plot_title <- "Distribution of Externally Studentized Residuals"
    plot_xlabel <- "Externally Studentized Residual Values"
    plot_ylabel <- "Count of Externally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "internally_studentized"){
    plot_title <- "Distribution of Internally Studentized Residuals"
    plot_xlabel <- "Internally Studentized Residual Values"
    plot_ylabel <- "Count of Internally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "regular"){
    plot_title <- "Distribution of Residuals"
    plot_xlabel <- "Residual Values"
    plot_ylabel <- "Count of Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "deleted"){
    plot_title <- "Distribution of Deleted Residuals"
    plot_xlabel <- "Deleted Residual Values"
    plot_ylabel <- "Count of Deleted Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "externally_studentized"){
    plot_title <- "QQ Plot of Externally Studentized Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Externally Studenzied Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "internally_studentized"){
    plot_title <- "QQ Plot of Internally Studentized Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Internally Studenzied Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "deleted"){
    plot_title <- "QQ Plot of Deleted (Prediction) Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Deleted (Prediction) Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "regular"){
    plot_title <- "QQ Plot of Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "externally_studentized"){
    plot_title <- "Externally Studentized Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Externally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "internally_studentized"){
    plot_title <- "Internally Studentized Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Internally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "deleted"){
    plot_title <- "Deleted Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Deleted Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "regular"){
    plot_title <- "Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "case_stat_vs_obs"){
    full_name <- case_stat_name_map(case_stat=plot_type_info, get_full_name=TRUE)
    plot_title <- paste0(full_name_to_title(full_name), " vs Observation Number")
    plot_xlabel <- "Observation Number"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=full_name)
  }else if(plot_kind =="partial_residual"){
    variable_name <- full_name_to_title(plot_type_info)
    plot_title <- paste0("Partial Residual Plot for Explanatory Variable ", variable_name)
    plot_xlabel <- variable_name
    plot_ylabel <- paste0("Partial Residual of ", variable_name)
    
    if(extra_info){
      plot_title <- paste0("Augmented ", plot_title)
      plot_ylabel <- paste0("Augmented ", plot_ylabel)
    }
    
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  
  return(return_list)
  
}


##################################################################################################

# Calculate all the case statistics to assess model fit.
create_case_df <- function(fit, dataframe=NULL){
  
  case_stats <- case(fit)
  case_df <- as.data.frame(case_stats)
  
  # Add deleted residuals to the dataframe
  case_df['deleted_resids'] <- case_df[,"e"] / (1 - case_df[, "h"])
  
  # Add fitted values to the dataframe
  case_df[,"fitted_values"] <- fit$fitted.values
  
  # Add a column to track observation number
  
  if(!is.null(dataframe) & ("Id" %in% names(dataframe))){
    
    case_df[,"obs_number"] <- dataframe[,"Id"]
    
  }else{
    
    case_df[,"obs_number"] <- seq(1, as.numeric(nrow(case_df)))  
  }
  
  
  
  return(case_df)
  
}

##################################################################################################
#################################### RESIDUAL PLOT SECTION #######################################
##################################################################################################

# 1. Calculates all case statistics and stores in a dataframe
# 2. Copies the data for the residual type being plotted to a column named "Resid_Plot_Column"
# 3. Filters the dataframe based on the "remove_less"than" and "remove_greater_than" filters.
get_residual_plot_data <- function(fit, residual_type, remove_less_than, remove_greater_than, dataframe=NULL){
  
  
  # Create the case statistics dataframe
  case_df <- create_case_df(fit, dataframe=dataframe)
  
  # Add the appropriate Resid_Plot_Column for this residual_type
  case_df <- set_resid_plot_column(case_df, residual_type=residual_type)
  
  # Filter according to the remove_less_than and remove_greater_than parameters, which filter out values
  # "less than" or "greater than" the specified value.
  case_df <- filter_by_absolute_value(df=case_df, 
                                      filter_column="Resid_Plot_Column", 
                                      remove_less_than=remove_less_than, 
                                      remove_greater_than=remove_greater_than)
  
  return(case_df)
  
}


# Sets the column to be plotted (residual type) based on user input.
set_resid_plot_column <- function(case_df, residual_type){
  
  if(residual_type == "externally_studentized"){
    case_df[,"Resid_Plot_Column"] <- case_df[,"stu.res"]
    
  }
  else if(residual_type == "internally_studentized"){
    case_df[,"Resid_Plot_Column"] <- case_df[,"sta.res"]  
  }
  else if(residual_type == "regular"){
    case_df[,"Resid_Plot_Column"] <- case_df[,"e"]
    
  }
  else if(residual_type == "deleted") {
    case_df[,"Resid_Plot_Column"] <- case_df[,"deleted_resids"]
  }
  else{
    case_df[,"Resid_Plot_Column"] <- case_df[, residual_type]
  }
  
  return(case_df)
}


# Function to filter a dataset based on a given column
# Removes values less than remove_less_than and greater than remove_greater_than
filter_by_absolute_value <- function(df, filter_column, remove_less_than=NULL, remove_greater_than=NULL){
  
  if(is.null(remove_less_than) == FALSE){
    df <- df[abs(df[,filter_column]) >= remove_less_than,]
  }
  
  if(is.null(remove_greater_than) == FALSE){
    df <- df[abs(df[,filter_column]) <= remove_greater_than,]
  }
  
  return(df)
  
}

##################################################################################################
################################## END RESIDUAL PLOT SECTION #####################################
##################################################################################################




##################################################################################################
####################### PARTIAL (COMPONENT PLUS) RESIDUAL PLOT SECTION ###########################
##################################################################################################

check_datatypes <- function(df, analysis_var){
  
  if(!is.numeric(df[,analysis_var])){
    print(paste0(analysis_var, " is  of non-numeric data type."))
    print("Multiple Regression Coefficients Exist For this categorical variable")
    print("Please specify which level you wish to plot the component plus residuals for")
    check_failed = TRUE
  } else{
    check_failed = FALSE
  }
  return(check_failed)
}


add_obs_number_column <- function(df){
  
  # If the dataframe already has an "Id" column for observation identifiers,
  # then use that, otherwise create 1 by number the rows of the dataframe.
  if("Id" %in% names(df)){
    df[,"obs_number"] <- df[,"Id"]
  }else{
    df[,"obs_number"] <- seq(1, as.numeric(nrow(df))) 
  }
  return(df)
}

get_augmented_data <- function(df, analysis_var, explanatory_vars) {
  
  # Create the new squared variable name
  squared_var <- paste0(analysis_var, "_squared")
  
  # Add the term to the dataframe
  df[, squared_var] <- df[,analysis_var]**2
  
  # Update the explanatory variables
  explanatory_vars <- append(explanatory_vars, squared_var)
  
  # Update analysis_var, because now there is two.
  analysis_var <- append(analysis_var, squared_var)
  
  augmented_data <- list(data_frame=df, 
                         explanatory_variables=explanatory_vars, 
                         analysis_variables=analysis_var)
  
  return(augmented_data)
  
}

# Fits a linear model based on string values specifying the response and explanatory variables
build_lm_from_strings <- function(df, response_var, explanatory_vars, x=FALSE){
  
  # Set up the formula for the linear model fit
  lm_formula <- as.formula(paste0(response_var, "~",str_c(explanatory_vars, collapse=" + ")))
  
  # Fit the model
  fit <- lm(lm_formula, data=as.data.frame(df), x=x)
  
  return(fit)
  
}

get_analysis_coefs <- function(fit, analysis_var){
  
  # Holds the coefficients for the analysis variables
  analysis_var_coefs <- c()
  
  # For each analysis variable (one variable for standard partial resids, two if augmented)
  for(analysis_index in 1:length(analysis_var)){
    
    # Grab the name of this analysis variable
    analysis_var_name <- analysis_var[analysis_index]
    
    # Loop through the coefficients
    for(coef_index in 1:length(fit$coefficients)){
      
      # Grab the name of the current coef
      coef_name <- names(fit$coefficients[coef_index])
      
      # Check if the name for this coef matches the analysis variable we are currently finding a coef for.
      if(analysis_var_name == coef_name){
        
        # Add the coef to the list
        analysis_var_coefs <- append(analysis_var_coefs, fit$coefficients[[coef_index]])
      }
    }
  }
  
  return(analysis_var_coefs)
}

set_analysis_var_column <- function(df, analysis_var){
  df[,"analysis_variable"] <- df[, analysis_var[1]]
  return(df)
}

compute_partial_residuals <- function(df, fit, analysis_var){
  
  df[,"residuals"] <- fit$residuals
  
  # Setting initial value of the component residuals for the analysis variables to zero.
  df[,"component_resid"] <- 0
  
  # Get the coefficient values for these analysis variable(s)
  analysis_var_coefs <- get_analysis_coefs(fit=fit, analysis_var=analysis_var)
  
  for(index in 1:length(analysis_var_coefs)){
    
    # For each analysis variable, multiple its X values by its coefficient and add it to the component residual column
    df[,"component_resid"] <- df[,"component_resid"] + (df[,analysis_var[index]] * analysis_var_coefs[index])
    
  }
  
  df[,"partial_resid"] <- df[,"residuals"] + df[,"component_resid"]
  
  df <- set_analysis_var_column(df=df, analysis_var=analysis_var)
  
  return(df)
  
}

assign_x_y <- function(df, x, y){
  
  df[, "x_variable"] <- df[,x]
  df[, "y_variable"] <- df[,y]
  return(df)
}

add_least_squares_line <- function(p, df, x_var, y_var, linecolor, linetype, 
                                   show_legend, add_least_squares){
  
  if(!add_least_squares){
    return(p)
  }
  
  df <- assign_x_y(df=df, x=x_var, y=y_var)
  
  fit <- lm(y_variable~x_variable, data=df)
  
  # Get the least squares intercept and slope
  ls_coefs <- fit$coeff
  intercept <- ls_coefs[[1]]
  slope <- ls_coefs[[2]]
  
  p <- p + 
    geom_abline(slope=slope, 
                intercept=intercept, 
                color=linecolor,
                linetype=linetype, 
                show.legend=show_legend)
  
  return(p)
  
}

add_point_removal_comparison_line <- function(p, df, x_var, y_var, linecolor, linetype, 
                                              show_legend, add_pt_removal_line, id_removal_compare_pts,
                                              obs_txt_size, obs_txt_hjust, obs_txt_vjust, obs_txt_color) {
  
  if(typeof(add_pt_removal_line) == "logical" && !add_pt_removal_line){
    return(p)
  }
  
  # Data frame that only contains the points we are removing, saving this so we can
  # color these points differently.
  removed_pts_df <- df[df[,"obs_number"] %in% add_pt_removal_line,]
  
  # Color the points we are removing differently
  p <- p + 
    geom_point(data=removed_pts_df, 
               mapping=aes(x=analysis_variable, y=partial_resid), 
               color=linecolor)
  
  # Remove the list of desired observation numbers before fitting 
  # the new least squares line
  df <- df[!(df[,"obs_number"] %in% add_pt_removal_line),]
  
  p <- add_least_squares_line(p=p, 
                              df=df, 
                              x_var=x_var, 
                              y_var=y_var, 
                              linecolor=linecolor, 
                              linetype=linetype,
                              show_legend=show_legend, 
                              add_least_squares=TRUE)
  
  # Identify the removed points
  if(id_removal_compare_pts){
    p <- p + geom_text(data=removed_pts_df, mapping=aes(x=analysis_variable, y=partial_resid, label=obs_number), 
                       hjust=obs_txt_hjust, vjust=obs_txt_vjust, color=obs_txt_color, size=obs_txt_size)    
  }

  
  return(p)
  
}

add_obs_numbers <- function(p, df, obs_txt_size, obs_txt_color,
                            obs_txt_vjust, obs_txt_hjust, identify_obs, 
                            x_var=NULL, y_var=NULL, called_from=NULL) {
  
  if(typeof(identify_obs) == "logical" && !identify_obs){
    return(p)
  }
  
  # If we didn't call this function from the plot_partial_residuals function 
  if(!is.null(called_from)){
    
    # This section of code only runs when called from plot_scatter, so these names aren't correct,
    # it is a little messy, but its the result of reusing this function for a secondary purpose (it 
    # was originally designed to add obs_numbers to the partial residual plots, but can be repurposed to
    # add obs_numbers to other plots too, just need the name to match).
    df[,"partial_resid"] <- df[,y_var]
    df[,"analysis_variable"] <- df[,x_var]
  }
  
  # If we don't want to identify all observatins, filter based on the vector passed
  # to identify_obs
  if(typeof(identify_obs) != "logical"){
    df <- df[df[,"obs_number"] %in% identify_obs,]
    
  }
  
  p <- p + geom_text(data=df, mapping=aes(x=analysis_variable, y=partial_resid, label=obs_number), 
                     hjust=obs_txt_hjust, vjust=obs_txt_vjust, color=obs_txt_color, size=obs_txt_size)
  
  return(p)
}

add_shading_variable <- function(fit, df, shade_by_case){
  
  if(is.null(shade_by_case)){
    return(df)
  }
  
  # Get the short name for the case statistic we want to shade by
  shading_variable <- case_stat_name_map(case_stat=shade_by_case, get_short_name=TRUE)
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    residual_type=shading_variable, 
                                    remove_less_than=NULL, 
                                    remove_greater_than=NULL)
  
  df[,"shade_by_case"] <- case_df[,shading_variable]
  
  return(df)
  
}

##################################################################################################
##################### END PARTIAL (COMPONENT PLUS) RESIDUAL PLOT SECTION #########################
##################################################################################################





##################################################################################################
############################## CASE STAT VS OBSERVATION SECTION ##################################
##################################################################################################



# Convert a full name to a title by removing underscores and adding capitolization
full_name_to_title <- function(case_stat){
  full_name_split <- str_replace_all(case_stat, pattern="_", replacement=" ")
  title <- str_to_title(full_name_split)
  return(title)
}

case_stat_name_map <- function(case_stat, get_full_name=NULL, get_short_name=NULL){
  
  full_to_short <- list(cooks_d="cook", leverage="h", DFFITS="dffit", 
                        externally_studentized_residuals="stu.res", deleted_standard_deviation="si")
  
  short_to_full <- list(cook="cooks_d", h="leverage", dffit="DFFITS", stu.res="externally_studentized_residuals", 
                        si="deleted_standard_deviation")
  
  if(!is.null(get_full_name)){
    
    # If its already the full name
    if(case_stat %in% short_to_full){
      return(case_stat)
    }
    else{
      return(short_to_full[[case_stat]])
    }
    
  }
  else if(!is.null(get_short_name)){
    
    if(case_stat %in% full_to_short){
      return(case_stat)
    }
    else{
      return(full_to_short[[case_stat]])
    }
  }
}

plot_rule_of_thumb <- function(fit, case_df, case_stat, cook_rot, dffit_rotm, 
                              resid_rot, std_rotm, leverage_rotm, p, ref_linecolor, 
                              ref_linetype, annot_reflines, plot_rot_reflines, 
                              flag_extreme, max_flagged, extreme_value_shape, extreme_value_color, 
                              obs_txt_hjust, obs_txt_size, obs_txt_vjust) {
  
  if(!plot_rot_reflines){
    return(p)
  }
  
  
  if(case_stat == "h"){
    p <- plot_leverage_rot(fit=fit, 
                           p=p, 
                           case_df=case_df, 
                           leverage_rotm=leverage_rotm, 
                           ref_linecolor=ref_linecolor, 
                           ref_linetype=ref_linetype, 
                           annot_reflines=annot_reflines, 
                           flag_extreme=flag_extreme,
                           max_flagged=max_flagged, 
                           extreme_value_shape=extreme_value_shape, 
                           extreme_value_color=extreme_value_color, 
                           obs_txt_hjust=obs_txt_hjust, 
                           obs_txt_size=obs_txt_size, 
                           obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "cook"){
    p <- plot_cook_rot(p=p, 
                       ref_linecolor=ref_linecolor, 
                       ref_linetype=ref_linetype, 
                       cook_rot=cook_rot, 
                       case_df=case_df, 
                       flag_extreme=flag_extreme,
                       max_flagged=max_flagged,                            
                       extreme_value_shape=extreme_value_shape, 
                       extreme_value_color=extreme_value_color, 
                       obs_txt_hjust=obs_txt_hjust, 
                       obs_txt_size=obs_txt_size, 
                       obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "dffit"){
    p <- plot_dffit_rot(fit=fit, 
                        p=p, 
                        dffit_rotm=dffit_rotm, 
                        ref_linetype=ref_linetype, 
                        ref_linecolor=ref_linecolor, 
                        annot_reflines=annot_reflines, 
                        case_df=case_df, 
                        flag_extreme=flag_extreme,
                        max_flagged=max_flagged,                            
                        extreme_value_shape=extreme_value_shape, 
                        extreme_value_color=extreme_value_color, 
                        obs_txt_hjust=obs_txt_hjust, 
                        obs_txt_size=obs_txt_size, 
                        obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "stu.res"){ # Externally Studentized Residuals
    p <- plot_ext_std_resid_rot(p=p, 
                                ref_linetype=ref_linetype, 
                                ref_linecolor=ref_linecolor, 
                                resid_rot=resid_rot, 
                                case_df=case_df,
                                flag_extreme=flag_extreme,
                                max_flagged=max_flagged,                            
                                extreme_value_shape=extreme_value_shape, 
                                extreme_value_color=extreme_value_color, 
                                obs_txt_hjust=obs_txt_hjust, 
                                obs_txt_size=obs_txt_size, 
                                obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "si"){  # deleted standard deviation
    p <- plot_deleted_std_rot(fit=fit, 
                              p=p, 
                              ref_linetype=ref_linetype, 
                              ref_linecolor=ref_linecolor, 
                              std_rotm=std_rotm,
                              annot_reflines=annot_reflines, 
                              case_df=case_df,
                              flag_extreme=flag_extreme,
                              max_flagged=max_flagged,                            
                              extreme_value_shape=extreme_value_shape, 
                              extreme_value_color=extreme_value_color, 
                              obs_txt_hjust=obs_txt_hjust, 
                              obs_txt_size=obs_txt_size, 
                              obs_txt_vjust=obs_txt_vjust)
  }
  
  return(p)
  
}

plot_deleted_std_rot <- function(fit, p, ref_linetype, case_df,
                                 ref_linecolor, std_rotm, annot_reflines,
                                 flag_extreme, max_flagged, extreme_value_shape, 
                                 extreme_value_color, obs_txt_size, obs_txt_vjust, obs_txt_hjust) {
  
  rmse <- summary(fit)$sigma
  
  upper_multiplier <- (1 + std_rotm)
  lower_multiplier <- (1 - std_rotm)
  
  upper_threshold <- rmse * upper_multiplier
  lower_threshold <- rmse * lower_multiplier
  
  p <- p + 
    geom_hline(yintercept=rmse, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=upper_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=lower_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype)
  
  if(flag_extreme){
    
    # Filter to find values above the upper threshold or below
    # the lower threshold
    above_filter <- (case_df[,"si"] >= upper_threshold)
    below_filter <- (case_df[,"si"] <= lower_threshold)
    
    # Combine the above filters, and subset to only include values that exceeded the threshold.
    exceeded_threshold <- above_filter | below_filter
    case_df <- case_df[exceeded_threshold,]
    
    case_df[,"si_difference"] <- rmse - case_df[,"si"]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"si_difference"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  if(!annot_reflines){
    return(p)
  }
  
  return(p)
  
}

plot_ext_std_resid_rot <- function(p, ref_linetype, ref_linecolor,resid_rot, case_df, 
                                   flag_extreme,max_flagged, extreme_value_shape, 
                                   extreme_value_color, obs_txt_size, obs_txt_vjust, 
                                   obs_txt_hjust){
  
  upper_threshold <- resid_rot
  lower_threshold <- resid_rot * -1
  
  p <- p + 
    geom_hline(yintercept=upper_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=lower_threshold, 
               color=ref_linecolor,
               linetype=ref_linetype)
  
  
  if(flag_extreme){
    
    above_filter <- (case_df[,"stu.res"] >= upper_threshold)
    below_filter <- (case_df[,"stu.res"] <= lower_threshold)
    
    # Combine the above filters, and subset to only include values that exceeded the threshold.
    exceeded_threshold <- above_filter | below_filter
    case_df <- case_df[exceeded_threshold,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"stu.res"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  return(p)
  
}

plot_dffit_rot <- function(fit, p, dffit_rotm, case_df, ref_linetype, 
                           ref_linecolor, annot_reflines,flag_extreme, 
                           max_flagged, extreme_value_shape, extreme_value_color,
                           obs_txt_size, obs_txt_vjust, obs_txt_hjust) {
  
  num_parameters <- length(fit$coefficients)
  num_observations <- as.numeric(nrow(case_df))
  
  upper_threshold <- dffit_rotm * sqrt((num_parameters - 1)/num_observations)
  lower_threshold <- upper_threshold * -1
  
  p <- p + 
    geom_hline(yintercept=upper_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=lower_threshold, 
               color=ref_linecolor,
               linetype=ref_linetype)
  
  
  if(flag_extreme){
    
    above_filter <- (case_df[,"dffit"] >= upper_threshold)
    below_filter <- (case_df[,"dffit"] <= lower_threshold)
    
    # Combine the above filters, and subset to only include values that exceeded the threshold.
    exceeded_threshold <- above_filter | below_filter
    case_df <- case_df[exceeded_threshold,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"dffit"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  
  if(!annot_reflines){
    return(p)
  }
  
  return(p)
  
}

get_average_leverage <- function(fit, case_df){
  
  num_parameters <- length(fit$coefficients)
  num_observations <- as.numeric(nrow(case_df))
  
  # Calculate the average leverage
  avg_leverage = num_parameters / num_observations
  
  return(avg_leverage)
  
}

plot_leverage_rot <- function(fit, p, case_df, leverage_rotm, ref_linecolor, 
                              ref_linetype, annot_reflines, flag_extreme, 
                              max_flagged, extreme_value_shape,extreme_value_color,
                              obs_txt_size, obs_txt_vjust, obs_txt_hjust){
  
  threshold <- get_leverage_threshold(fit=fit, 
                                      leverage_line_multiplier=leverage_rotm, 
                                      case_df=case_df)
  
  average_leverage <- get_average_leverage(fit=fit, case_df=case_df)
  
  p <- p + 
    geom_hline(yintercept=threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) +
    geom_hline(yintercept=average_leverage, 
               color=ref_linecolor, 
               linetype=ref_linetype)
  
  if(flag_extreme){
    
    case_df <- case_df[case_df[,'h'] >= threshold,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"h"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  if(!annot_reflines){
    return(p)
  }
  
  return(p)
}

plot_cook_rot <- function(p, cook_rot, ref_linecolor, ref_linetype, case_df,
                          flag_extreme, max_flagged, extreme_value_shape, 
                          extreme_value_color,obs_txt_size, obs_txt_vjust, obs_txt_hjust){
  
  p <- p + 
    geom_hline(yintercept=cook_rot, 
               color=ref_linecolor, 
               linetype=ref_linetype)
  
  if(flag_extreme){
    
    threshold_filter <- (case_df[,"cook"] >= cook_rot)
    case_df <- case_df[threshold_filter,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"dffit"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  return(p)
  
}

##################################################################################################
############################ END CASE STAT VS OBSERVATION SECTION ################################
##################################################################################################




##################################################################################################
################################ RESIDUAL VS LEVERAGE SECTION ####################################
##################################################################################################

get_leverage_threshold <- function(fit, leverage_line_multiplier, case_df){
  
  # Calculate average leverage
  avg_leverage <- get_average_leverage(fit=fit, case_df=case_df)
  
  # Calculate the leverage level at which we want to inspect observations closer
  leverage_threshold <- leverage_line_multiplier * avg_leverage
  
  return(leverage_threshold)
  
}

flag_extreme_observations <- function(fit, residual_type, case_df, flag_extreme_obs, 
                                      max_points_flagged, show_extreme_obs_numbers, resid_line_threshold, 
                                      extreme_value_color, p, leverage_line_multiplier, obs_txt_size, obs_txt_vjust,
                                      obs_txt_hjust){
  
  # Current implementation, flagging only makes sense for externally studentized residuals
  if(!flag_extreme_obs || residual_type != "externally_studentized"){
    return(p)
  }
  
  leverage_threshold <- get_leverage_threshold(fit, leverage_line_multiplier, case_df)
  
  # Filter dataframe to only contain observation that exceed the acceptable thresholds
  # for both leverage and (externally studentized) residual size
  case_df <- case_df[case_df[,"h"] > leverage_threshold,]
  case_df <- case_df[abs(case_df[,"Resid_Plot_Column"]) > resid_line_threshold,]
  
  num_values_to_flag <- min(as.numeric(nrow(case_df)), max_points_flagged)
  
  # Dataframe containing up to max_point_flagged observations with the largest Cooks D
  case_df <- case_df[order(abs(case_df[,"cook"]), decreasing=TRUE),][1:num_values_to_flag,]
  
  # Plot extreme values in red
  p <- p + geom_point(data=case_df, mapping=aes(x=h, y=Resid_Plot_Column), shape=8, color=extreme_value_color) +
    geom_point(data=case_df, mapping=aes(x=h, y=Resid_Plot_Column), color=extreme_value_color)
  
  # If we don't need to show the extreme observation numbers, we can stop the function here.
  if(!show_extreme_obs_numbers){
    return(p)
  }
  
  # Annotate the observation numbers for the extreme points
  p <- p +
    geom_text(data=case_df, mapping=aes(x=h, y=Resid_Plot_Column, label=obs_number),
              hjust=obs_txt_hjust, vjust=obs_txt_vjust, size=obs_txt_size, 
              color=extreme_value_color)
  
  return(p)
}


add_reference_lines <- function(fit, residual_type, case_df, add_reference_lines,
                                leverage_line_multiplier, p, resid_line_threshold, 
                                reference_linetype, reference_linecolor, annotate_thresholds) {
  
  if(!add_reference_lines){
    return(p)
  }
  
  leverage_threshold <- get_leverage_threshold(fit, leverage_line_multiplier, case_df)
  
  p <- p + geom_vline(xintercept=leverage_threshold, 
                      color=reference_linecolor, 
                      linetype=reference_linetype)
  
  # It only makes sense to plot the residual reference lines to externally studentized
  # residuals, so if its any other type, end the function here.
  if(residual_type != "externally_studentized"){
    return(p)
  }
  
  positive_resid_thresh=resid_line_threshold
  negative_resid_thresh=resid_line_threshold*-1
  
  p <- p + 
    geom_hline(yintercept=positive_resid_thresh, color=reference_linecolor, 
               linetype=reference_linetype) + 
    geom_hline(yintercept=negative_resid_thresh, color=reference_linecolor,
               linetype=reference_linetype)
  
  # If we don't want to annotate the threshold values, function can end here
  if(!annotate_thresholds){
    return(p)  
  }
  
  # Annotate the leverage threshold
  leverage_annot <- paste0(leverage_line_multiplier, "x avg leverage")
  leverage_df <- data.frame(txt=leverage_annot, x_txt=leverage_threshold, y_txt=-Inf)
  
  p <- p +
    geom_text(data=leverage_df, mapping=aes(x=x_txt, y=y_txt, label=txt, hjust=-0.05, vjust=-0.5), 
              color=reference_linecolor)
  
  return(p)
  
}

##################################################################################################
############################### END RESIDUAL VS LEVERAGE SECTION #################################
##################################################################################################






##################################################################################################
################################## RESIDUAL HISTOGRAM SECTION ####################################
##################################################################################################

# The deleted and regular residual plots can not have their binwidth set directly in a very intuitive manner
# Therefore it is easier to set the number of bins instead. To overlay the normal curve however, we need to know
# binwidth. This function calculates the correct bin width based on the x-axis values and the number of bins.
get_binwith <- function(binwidth, num_bins, current_plot){
  
  if(is.null(binwidth) == TRUE){
    
    build <- ggplot_build(current_plot)
    x_min <- build$layout$panel_params[[1]]$x.range[1]
    x_max <- build$layout$panel_params[[1]]$x.range[2]
    x_length <- x_max - x_min
    binwidth <- x_length / num_bins
    
  } 
  
  return(binwidth)
  
}

##################################################################################################
################################ END RESIDUAL HISTOGRAM SECTION ##################################
##################################################################################################


##################################################################################################
######################################## QQ PLOT SECTION #########################################
##################################################################################################

# Function to calculate the intercept and slope of the line that goes through the first and third
# quartiles of the (theoretical_quartile, data_quartile) x,y pairs. 
get_quartile_line_params <- function(qq){
  
  # Get the first and third quartiles for the theoretical distribution
  x_quartiles <- quantile(qq$x)
  first_quartile_x <- x_quartiles[[2]]
  third_quartile_x <- x_quartiles[[4]]
  
  # Get the first and third quartiles for the observed distribution (the data).
  y_quartiles <- quantile(qq$y)
  first_quartile_y <- y_quartiles[[2]]
  third_quartile_y <- y_quartiles[[4]]
  
  # Fit a line between the first and third quartiles, to get the intercept and slope for plotting
  line_df <- data.frame(x_quartiles= c(first_quartile_x, third_quartile_x), 
                        y_quartiles=c(first_quartile_y, third_quartile_y))
  
  quartile_line_fit <- lm(y_quartiles~x_quartiles, data=line_df)
  
  
  # Extract line parameters from the fit
  quartile_line_coefs <- quartile_line_fit$coeff
  quartile_line_intercept <- quartile_line_coefs[[1]]
  quartile_line_slope <- quartile_line_coefs[[2]]
  
  line_params <- list(slope=quartile_line_slope, intercept=quartile_line_intercept)
  
  return(line_params)
}

flag_n_largest <- function(qq_df, case_df, p, flag_largest_resid, flag_nlargest, flag_color_resid, 
                           flag_marker_shape, flag_txt_hjust,flag_txt_vjust, flag_txt_size){
  
  
  # Combined the case statistics (residual values) with the qqplot data
  # combined_df <- cbind(qq_df, case_df)
  combined_df <- merge(x=qq_df, y=case_df, by.x="obs_number", by.y="obs_number")
  
  #return(list(c_df=combined_df, q_df=qq_df, case_df=case_df))
    
  # Sort by absolute value of residual, so the largest residuals are at the top of the
  # dataframe, then grab the nlargest of those.
  nlargest_resids <- combined_df[order(abs(combined_df[,"Resid_Plot_Column"]), 
                                       decreasing=TRUE),][1:flag_nlargest,]
    
  p <- p + 
    geom_point(data=nlargest_resids, mapping=aes(x=x_values, y=y_values), color=flag_color_resid) + 
    geom_point(data=nlargest_resids, mapping=aes(x=x_values, y=y_values), color=flag_color_resid, 
               shape=flag_marker_shape) + 
    geom_text(data=nlargest_resids, mapping=aes(x=x_values,y=y_values, label=obs_number), 
              hjust=flag_txt_hjust, vjust=flag_txt_vjust, color=flag_color_resid, size=flag_txt_size)    

  return(p)
  
}


##################################################################################################
###################################### END QQ PLOT SECTION #######################################
##################################################################################################


##################################################################################################
##################################### SCATTER PLOT SECTION #######################################
##################################################################################################

filter_scatter_data <- function(df, filter_column, keep_values, remove_less_than, 
                                remove_greater_than) {
  
  # If there is nothing we want to filter
  if(is.null(filter_column)){
    return(df)
  }
  
  # Categorical filtering based on a vector of categories to keep
  if(!is.null(keep_values)){
    df <- df[df[,filter_column] %in% keep_values,]
    return(df)
  }
  
  # Numeric filtering, remove everything less than
  if(!is.null(remove_less_than)){
    df <- df[df[,filter_column] >= remove_less_than,]
  }
  
  # Numeric filtering, remove everything greater than
  if(!is.null(remove_greater_than)){
    df <- df[df[,filter_column] <= remove_greater_than,]
  }
 
  return(df)
  
}

filter_by_observation_numbers <- function(df, observation_numbers){
  
  if(is.null(observation_numbers)){
    return(df)
  }
  
  # Remove the desired observation numbers
  df <- df[!(df[,"obs_number"] %in% observation_numbers),]
  
  return(df)
}

get_plotting_data <- function(df, x_var, y_var, shade_var, shape_var, size_var, keep_values, 
                              remove_less_than, remove_greater_than, filter_column){
  
  df[,"Response"] <- df[,y_var]
  df[,"Explanatory"] <- df[,x_var]
  
  df <- filter_scatter_data(df=df, 
                            filter_column=filter_column, 
                            keep_values=keep_values, 
                            remove_less_than=remove_less_than, 
                            remove_greater_than=remove_greater_than)
  
  # If we want to shade the points based on a variable
  if(!is.null(shade_var)){
    df[,"shading_variable"] <- as.factor(df[,shade_var])
    shading_variable <- df[,"shading_variable"]
  }
  else{
    shading_variable <- NULL
  }
  
  # If we want to change the points shapes based on a variable
  if(!is.null(shape_var)){
    df[,"shape_variable"] <- as.factor(df[,shape_var])
    shape_variable <- df[,"shape_variable"]
  }
  else{
    shape_variable <- NULL
  }
  
  # If we want to change the points sizes based on a variable
  if(!is.null(size_var)){
    df[,"size_variable"] <- df[,size_var]
    size_variable <- df[,"size_variable"]
  }
  else{
    size_variable <- NULL
  }
  
  
  return_list <- list(data_frame=df, 
                      shading_var=shading_variable, 
                      shape_var=shape_variable, 
                      size_var=size_variable)
  
  return(return_list)
  
}

add_legend_data <- function(p, shade_var, shape_var){
  
  if(is.null(shade_var) == FALSE){
    p <- p + labs(color=shade_var)
  }
  else{
    p <- p + labs(color=NULL)
  }
  
  if(is.null(shade_var) == FALSE){
    p <- p + labs(shape=shape_var)
  }
  else{
    p <- p + labs(shape=NULL)
  }
  
  return(p)
  
}

add_scatterplot_title <- function(p, show_regression, x_var, y_var, fit=NULL, round_digits=4){
  
  if(!show_regression){
    
    plot_title <- paste0("Scatter Plot of ", y_var, " vs ", x_var)
    p <- p + ggtitle(plot_title) + 
      xlab(x_var) + 
      ylab(y_var)
    
    return(p)
  }
  
  # Save summary statistics to add to title
  rmse <- round(summary(fit)$sigma, round_digits)
  r_square <- round(summary(fit)$r.squared, round_digits)
  adj_r_square <- round(summary(fit)$adj.r.squared, round_digits)
  
  # Create plot title.
  plot_title <- paste0("Regression of ", y_var, " on ", x_var, "\n", 
                  "RMSE=", rmse, "  R Square=", r_square, "  Adjusted R Square=", adj_r_square)
  
  p <- p + ggtitle(plot_title) + 
    xlab(x_var) + 
    ylab(y_var)
  
  return(p)
  
}

get_reg_table_location <- function(table_loc){
  
  if(table_loc == "upper_right"){
    x_coord <- Inf
    y_coord <- Inf
  }
  else if(table_loc == "lower_right"){
    x_coord <- Inf
    y_coord <- -Inf
    
  }
  else if(table_loc == "upper_left"){
    x_coord <- -Inf
    y_coord <- Inf
    
  }
  else if(table_loc == "lower_left"){
    x_coord <- -Inf
    y_coord <- -Inf
  }
  
  coords <- list(x_coordinate=x_coord, 
                 y_coordinate=y_coord)
  
  return(coords)
  
}


add_regression_table <- function(reg_table, table_loc, p, x_var, fit=NULL, round_digits=4){
  
  if(!reg_table){
    return(p)
  }
  
  table_coordinates <- get_reg_table_location(table_loc=table_loc)
  
  # Information for creating the table
  annot_data <- data.frame(summary(fit)$coefficients)
  
  # Round values so table isn't way too big
  annot_data <- round(annot_data, digits=round_digits)
  
  # Adjust the tables row names based on the dataset.
  row.names(annot_data)[row.names(annot_data) == "Explanatory"] <- paste0(x_var, "(Slope)")
  
  # Adjust the tables column names
  names(annot_data) <- c("Estimate", "Std_Error", "T_value", "P_value")
  
  p <- p + 
    annotate(geom="table", 
             x=table_coordinates[["x_coordinate"]], 
             y=table_coordinates[["y_coordinate"]], 
             label=list(annot_data), 
             table.rownames=TRUE)
  
  return(p)
  
}

add_regression_and_title <- function(df, show_regression, p, conf_level, pred_band, 
                                     conf_band, reg_linecolor, conf_linecolor, pred_linecolor,
                                     conf_linetype, pred_linetype, x_var, y_var, round_digits, 
                                     reg_table, table_loc){
  
  if(!show_regression){
    p <- add_scatterplot_title(p=p, show_regression=show_regression, x_var=x_var, y_var=y_var)
    return(p)
  }
  
  fit <- lm(Response~Explanatory, data=df)
  
  # Data for prediction band
  pred_data <- as.data.frame(predict(fit, interval="prediction", level=conf_level))
  pred_data <- rename(pred_data, pred_lower=lwr, pred_upper=upr)
  
  # Data for confidence band
  conf_data <- as.data.frame(predict(fit, interval="confidence", level=conf_level))
  conf_data <- rename(conf_data, conf_lower=lwr, conf_upper=upr)
  
  # combine original data, confidence band data, and prediction band data.
  df <- cbind(df, conf_data, pred_data[,c("pred_lower", "pred_upper")])

  
  # Add the regression line
  p <- p + geom_smooth(method="lm", color=reg_linecolor, level=conf_level)
  
  # Add the confidence band
  if(conf_band){
    p <- p + geom_line(data=df, mapping=aes(x=Explanatory, y=conf_lower), 
                       color=conf_linecolor, linetype=conf_linetype) +
      geom_line(data=df, mapping=aes(x=Explanatory, y=conf_upper), 
                color=conf_linecolor, linetype=conf_linetype)
  }
  
  # Add the prediction band
  if(pred_band){
    p <- p + geom_line(data=df, mapping=aes(x=Explanatory, y=pred_lower), color=pred_linecolor, 
                       linetype=pred_linetype) + 
      geom_line(data=df, mapping=aes(x=Explanatory, y=pred_upper), color=pred_linecolor, 
                linetype=pred_linetype)

  }
  
  p <- add_scatterplot_title(p=p, show_regression=show_regression, x_var=x_var, y_var=y_var, 
                             fit=fit, round_digits=round_digits)
  
  p <- add_regression_table(reg_table=reg_table, table_loc=table_loc, p=p, x_var=x_var, 
                            fit=fit, round_digits=round_digits)
  
  return(p)
  
}

##################################################################################################
################################### END SCATTER PLOT SECTION #####################################
##################################################################################################


############################## SEPARATE/PARALLEL LINES MODEL SECTION  ############################### 

get_ggplot_colors <- function(n) {
  hues = seq(15, 375, length = n + 1)
  return(hcl(h = hues, l = 65, c = 100)[1:n])
}


get_adjustment_terms <- function(coefficients, explanatory_continuous, explantory_grouping){
  
  cfs_names <- names(coefficients)
  
  intercept_adjustments <- c()
  slope_adjustments <- c()
  for(name_index in 1:length(cfs_names)){
    
    current_name <- cfs_names[name_index]
    
    contains_grouping <- stringr::str_detect(string=current_name, pattern=explantory_grouping)
    contains_continuous <- stringr::str_detect(string=current_name, pattern=explanatory_continuous)
    is_interaction <- contains_grouping & contains_continuous
    
    if(is_interaction){
      
      slope_adjustments <- append(x=slope_adjustments, 
                                  values=current_name)
    }
    else if(contains_grouping){
      intercept_adjustments <- append(x=intercept_adjustments, 
                                      values=current_name)
      
    }else if(current_name == explanatory_continuous){
      base_slope <- current_name
    }
    else if(current_name == "(Intercept)"){
      base_intercept <- current_name
    }
    
  }
  
  terms <- list(slope_adj=slope_adjustments,
                int_adjs=intercept_adjustments,
                base_slope=base_slope,
                base_intercept=base_intercept)
  
  return(terms)
}

add_group_means <- function(p, df, explanatory_continuous, explantory_grouping, response, 
                            plot_group_means, mean_shape, mean_size){
  
  if(!plot_group_means){
    return(p)
  }
  
  df[,"group_var"] <- df[,explantory_grouping]
  df[,"x_variable"] <- df[,explanatory_continuous]
  df[,"y_variable"] <- df[,response]
  
  group_mean_df <- df %>% 
    group_by(group_var=group_var) %>%
    summarize(mean_x=mean(x_variable),
              mean_y=mean(y_variable))
  
  group_mean_df <- as.data.frame(group_mean_df)
  
  p <- p + geom_point(data=group_mean_df, shape=mean_shape, size=mean_size, 
                      mapping=aes(x=mean_x, y=mean_y, color=group_var))
  
  return(p)
}

plot_all_mlr_lines <- function(p, fit, model_type, explanatory_continuous, explantory_grouping, response,
                               plot_group_means, df, mean_shape, mean_size){
  
  # Get the coefficients for the MLR model
  coefficients <- fit$coefficients
  
  # Sort the coefs into base intercept, base slope, intercept adjustments and slope adjustments
  model_term_names <- get_adjustment_terms(coefficients=coefficients,
                                           explanatory_continuous=explanatory_continuous, 
                                           explantory_grouping=explantory_grouping)
  
  num_lines <- length(model_term_names$int_adjs) + 1
  
  base_intercept_value <- coefficients[[model_term_names$base_intercept]]
  base_slope_value <- coefficients[[model_term_names$base_slope]]
  
  line_colors <- get_ggplot_colors(n=num_lines)
  
  for(line_num in 1:num_lines){
    
    if(line_num == 1){
      intercept_term <- base_intercept_value
      slope_term <- base_slope_value
    } else{
      intercept_term <- base_intercept_value + coefficients[[model_term_names$int_adjs[line_num - 1]]]
      
      # If the lines have separate slopes, else they are parallel lines with the same slope
      if(model_type == "separate_lines") {
        slope_term <- base_slope_value + coefficients[[model_term_names$slope_adj[line_num -1]]]
      }
      else{
        slope_term <- base_slope_value
      }
    }
    
    line_color <- line_colors[[line_num]]
    
    p <- p + 
      geom_abline(slope=slope_term,
                  intercept=intercept_term,
                  linetype="solid",
                  color=line_color, 
                  show.legend=TRUE)
    
  }
  
  # If we want to add a special marker to show that the regression line for each group always
  # goes through the mean (for the separate lines model at least).
  p <- add_group_means(p=p, df=df, explanatory_continuous=explanatory_continuous,
                       explantory_grouping=explantory_grouping, response=response, 
                       plot_group_means=plot_group_means, mean_shape=mean_shape, 
                       mean_size=mean_size)

  
  return(p)
}

############################## END SEPARATE/PARALLEL LINES MODEL SECTION  #############################



#################################### Data Cleaning #################################### 

ord_var_analyzer <- function(variable, ordering=NULL){
  
  train_df <- read.csv("./train.csv")
  test_df <- read.csv("./test.csv")
  
  train_df <- handle_categoricals_with_NA_level(train_df)
  test_df <- handle_categoricals_with_NA_level(test_df)
  
  # CREATE UNORDERED FACTORS SO WE CAN CHECK WHAT LEVELS EXIST
  train_df[,variable] <- factor(train_df[,variable])
  test_df[,variable] <- factor(test_df[,variable])
  
  # TRAIN SET CHECKS
  train_set_unique <- unique(train_df[,variable])
  train_set_counts <- table(train_df[,variable])
  num_missing_train <- sum(is.na(train_df[,variable]))
  
  # TEST SET CHECKS
  test_set_unique <- unique(test_df[,variable])
  test_set_counts <- table(test_df[,variable])
  num_missing_test <- sum(is.na(test_df[,variable]))
  
  print(paste0("====================== TRAIN REPORT FOR ", variable, " ======================"))
  print("UNIQUE LEVELS: ")
  cat("\n")
  print(train_set_unique)
  cat("\n****************************************\n\n")
  print("LEVEL COUNTS: ")
  print(train_set_counts)
  cat("\n****************************************\n\n")
  print(paste0("NUMBER OF MISSINGS -->  ", num_missing_train))
  print("======================================================================")
  cat("\n")
  cat("\n")
  
  print(paste0("====================== TEST REPORT FOR ", variable, " ======================"))
  print("UNIQUE LEVELS: ")
  cat("\n")
  print(test_set_unique)
  cat("\n****************************************\n\n")
  print("LEVEL COUNTS: ")
  print(test_set_counts)
  cat("\n****************************************\n\n")
  print(paste0("NUMBER OF MISSINGS -->  ", num_missing_test))
  print("======================================================================")
  cat("\n")
  cat("\n")
  
  if(!is.null(ordering)){
    # CREATE UNORDERED FACTORS SO WE CAN CHECK WHAT LEVELS EXIST
    train_df[,variable] <- factor(train_df[,variable], 
                                  levels=ordering, 
                                  order=TRUE)
    test_df[,variable] <- factor(test_df[,variable],
                                 levels=ordering,
                                 order=TRUE)
    
    train_ordering <- train_df[,variable][1]
    train_ordered_counts <- table(train_df[,variable])
    
    test_ordering <- test_df[,variable][1]
    test_ordered_counts <- table(test_df[,variable])
    
    print(paste0("====================== *AFTER ORDERING* REPORT FOR ", variable, " ======================"))
    print("TRAIN LEVEL ORDERING: ")
    cat("\n")
    print(train_ordering)
    cat("\n****************************************\n\n")
    print("TRAIN COUNTS: ")
    cat("\n")
    print(train_ordered_counts)
    cat("\n****************************************\n\n")
    print("TEST LEVEL ORDERING: ")
    cat("\n")
    print(test_ordering)
    cat("\n****************************************\n\n")
    print("TEST COUNTS: ")
    cat("\n")
    print(test_ordered_counts)
    cat("\n****************************************\n\n")
    print("======================================================================")
    cat("\n")
    cat("\n")
    
  }
}

handle_categoricals_with_NA_level <- function(df){
  
  cols_with_legit_na <- c("MiscFeature", "Fence", "PoolQC", "GarageCond", "GarageQual", "GarageFinish", "GarageType", 
                          "FireplaceQu", "BsmtFinType2", "BsmtFinType1", "BsmtExposure", "BsmtCond", "BsmtQual", "Alley")
  
  for(index in 1:length(cols_with_legit_na)){
    
    column_name <- cols_with_legit_na[[index]]
    
    # Fill in the NA with "no_feature" because this value isn't missing, it is an actual level of 
    # the categorical variable that indicates the feature is not present in that house.
    df[is.na(df[,column_name]), column_name] <- "no_feature"
    
  }
  
  return(df)
  
}

print_missing_values_ames <- function(df){
  
  df <- handle_categoricals_with_NA_level(df)
  
  return(sapply(df, function(x) sum(is.na(x))))
  
}

print_missings_post_cleaning_ames <- function(df){
  
  return(sapply(df, function(x) sum(is.na(x))))
}

remove_heavily_imbalanced_factors <- function(dataframe, training_data, save_path, 
                                              imbalance_threshold, num_rows=1460){
  
  column_names <- names(dataframe)
  
  max_allowed_imbalance <- imbalance_threshold * num_rows
  
  imbalanced_columns = c()
  
  if(training_data){
    
    for(column_index in 1:length(column_names)){
      
      col_name <- column_names[[column_index]]
      
      # If this is a factor column, check if it is imbalanced
      if(class(dataframe[,col_name]) == "factor"){
        
        # If this factor is imbalanced beyond the allowed threshold
        if(max(table(dataframe[,col_name])) > max_allowed_imbalance){
          
          # Update the list of imbalanced columns, so they can be removed from
          # the test set as well.
          imbalanced_columns <- append(imbalanced_columns, col_name)
          
        }
      }
    }
    
    # Remove the imbalanced columns from the training set
    dataframe <- dataframe[, !(names(dataframe) %in% imbalanced_columns)]
    
    # Save the columns removed, so they can be removed from the test set too
    imbalanced_factor_df <- data.frame(imbalanced_factors=imbalanced_columns)
    write.csv(imbalanced_factor_df, save_path)
    
    #return_list <- list(dataframe=dataframe,
    #                    columns_removed=imbalanced_columns)
    
  }else{ # ELSE, WE ARE PROCESSING THE TEST SET
    
    # Read in the dataframe containing the imbalanced columns to remove
    imb_categorical_df <- read.csv(save_path)
    imb_col_names <- imb_categorical_df[,"imbalanced_factors"]
    
    # Remove the imbalanced columns from the test set
    dataframe <- dataframe[,!(names(dataframe) %in% imb_col_names)]
    
    #return_list <- list(dataframe=dataframe,
    #                  columns_removed=imb_col_names)
    
  }
  
  return(dataframe)
  
}


add_custom_columns <- function(df){
  
  # Create a total indoor square footage feature
  df[,"TotalIndoorSF"] <- df[,"X1stFlrSF"] + df[,"X2ndFlrSF"] + df[,"TotalBsmtSF"]
  
  # Remove the three columns used to create the new feature above
  # GrLivArea is simply "X1stFlrSF" + "X2ndFlrSF", making it also now redundant.
  df <- df[,!(names(df) %in% c("X1stFlrSF", "X2ndFlrSF", "TotalBsmtSF", "GrLivArea"))]
  
  return(df)
  
}


impute_missing_values_ames <- function(df, train_data, save_path){
  
  cols_with_missings_train_or_test <- c("MSZoning", "LotFrontage", "Utilities", "Exterior1st", "Exterior2nd", "MasVnrType",
                                        "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", 
                                        "BsmtHalfBath", "KitchenQual", "Functional", "GarageCars", "GarageArea",
                                        "SaleType", "Electrical")
  
  
  impute_with_most_frequent <- c("MSZoning", "Utilities", "MasVnrType", "KitchenQual", "Functional", "SaleType", "Electrical",
                                 "Exterior1st", "Exterior2nd")
  
  impute_with_mean <- c("LotFrontage", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", 
                        "BsmtFullBath", "BsmtHalfBath", "GarageCars", "GarageArea")
  
  
  # IMPUTE MISSING VALUES IN GARAGEYRBUILT WITH THE YEAR THE HOME WAS BUILT
  df[is.na(df[,"GarageYrBlt"]),"GarageYrBlt"] <- df[is.na(df[,"GarageYrBlt"]),"YearBuilt"]
  
  if(train_data){
    
    # STRANGE SPELLING MISTAKES IN THE EXTERIOR COLUMNS
    df[df[,"Exterior2nd"] == "Wd Shng", "Exterior2nd"] <- "WdShing"
    df[df[,"Exterior2nd"] == "Brk Cmn", "Exterior2nd"] <- "BrkComm"
    df[df[,"Exterior2nd"] == "CmentBd", "Exterior2nd"] <- "CemntBd"
    df[df[,"Exterior2nd"] == "CBlock", "Exterior2nd"] <- "Other"
    df[df[,"Exterior1st"] == "CBlock", "Exterior1st"] <- "Other"
    df[df[,"Exterior1st"] == "AsphShn", "Exterior1st"] <- "Other"
    df[df[,"Exterior2nd"] == "AsphShn", "Exterior2nd"] <- "Other"
    
    
    # MODE IMPUTATIONS
    for(mode_index in 1:length(impute_with_most_frequent)){
      
      feat_name <- impute_with_most_frequent[[mode_index]]
      
      most_frequent <- tail(names(sort(table(df[!is.na(df[,feat_name]),feat_name]))),1)
      
      
      if(mode_index == 1){
        test_imputation_df <- data.frame(MSZoning=most_frequent)
      }
      else{
        test_imputation_df[,feat_name] <- most_frequent
      }
      
      
      # IMPUTE MISSINGS WITH THE MOST FREQUENT VALUE
      df[is.na(df[,feat_name]),feat_name] <- most_frequent
      
    }
    
    # MEAN IMPUTATIONS
    for(mean_index in 1:length(impute_with_mean)){
      
      feat_name <- impute_with_mean[[mean_index]]
      
      # TAKE THE MEAN VALUE OF THAT COLUMN, USING ALL ROWS WHERE THE VALUE IS NOT MISSING.
      feature_mean <- mean(df[!is.na(df[,feat_name]),feat_name])
      
      test_imputation_df[,feat_name] <- feature_mean
      
      # IMPUTE MISSINGS WITH THE AVERAGE VALUE IN THAT COLUMN
      df[is.na(df[,feat_name]),feat_name] <- feature_mean
      
    }
    
    write.csv(test_imputation_df, save_path) 
    
  }else{
    
    impute_df <- read.csv(save_path)
    
    for(impute_index in 1:length(cols_with_missings_train_or_test)){
      
      feat_name <- cols_with_missings_train_or_test[[impute_index]]
      
      # IMPUTE WITH THE SAME VALUE WE IMPUTED WITH DURING TRAINING
      df[is.na(df[,feat_name]),feat_name] <- impute_df[,feat_name]
    }
    
    
    # STRANGE SPELLING MISTAKES IN THE EXTERIOR COLUMNS, and some categories
    # with just way to few entries (causing big problems fitting the coefficients).
    df[df[,"Exterior2nd"] == "Wd Shng", "Exterior2nd"] <- "WdShing"
    df[df[,"Exterior2nd"] == "Brk Cmn", "Exterior2nd"] <- "BrkComm"
    df[df[,"Exterior2nd"] == "CmentBd", "Exterior2nd"] <- "CemntBd"
    df[df[,"Exterior2nd"] == "CBlock", "Exterior2nd"] <- "Other"
    df[df[,"Exterior1st"] == "CBlock", "Exterior1st"] <- "Other"
    df[df[,"Exterior1st"] == "AsphShn", "Exterior1st"] <- "Other"
    df[df[,"Exterior2nd"] == "AsphShn", "Exterior2nd"] <- "Other"
     
  }
  
  return(df)
  
}


get_nominal_feature_map <- function(){
  
  nominal_references <- list("MSSubClass"=c("20", "30", "40", "45", "50", "60", "70", "75", "80", "85", "90",
                                            "120", "150", "160", "180", "190"),
                             "MSZoning"=c("RM","RL", "RH", "FV", "C (all)"),
                             "Street"=c("Grvl", "Pave"),
                             "Alley"=c("no_feature", "Grvl", "Pave"),
                             "LandContour"= c("Lvl", "Low", "Bnk", "HLS"),
                             "LotConfig"=c("CulDSac", "Corner", "Inside", "FR2", "FR3"),
                             "Neighborhood"=c("NAmes", "Blmngtn", "Blueste", "BrDale", "BrkSide", "ClearCr",
                                              "CollgCr", "Crawfor", "Edwards", "Gilbert", "IDOTRR", "MeadowV",
                                              "Mitchel", "NoRidge", "NPkVill", "NridgHt", "NWAmes", "OldTown",
                                              "Sawyer", "SawyerW", "Somerst", "StoneBr", "SWISU", "Timber", "Veenker"),
                             "Condition1"=c("Norm", "Artery", "Feedr", "PosA", "PosN", "RRAe", "RRAn", "RRNe",
                                            "RRNn"),
                             "Condition2"=c("Norm", "Artery", "Feedr", "PosA", "PosN", 
                                            "RRAe", "RRAn","RRNn"),
                             "BldgType"=c("1Fam", "2fmCon", "Duplex", "Twnhs", "TwnhsE"),
                             "HouseStyle"=c("1Story", "1.5Fin", "1.5Unf", "2.5Fin", "2.5Unf",
                                            "2Story", "SFoyer", "SLvl"),
                             "RoofStyle"=c("Flat", "Gable", "Gambrel", "Hip", "Mansard", "Shed"),
                             "RoofMatl"=c("ClyTile", "CompShg", "Membran", "Metal", 
                                          "Roll", "Tar&Grv", "WdShake", "WdShngl"),
                             "Exterior1st"=c("WdShing", "Wd Sdng", "VinylSd", "Stucco", "Stone", "Plywood", 
                                             "Other", "MetalSd", "ImStucc", "HdBoard", "CemntBd", "BrkFace", 
                                             "BrkComm", "AsbShng"),
                             "Exterior2nd"=c("WdShing", "Wd Sdng", "VinylSd", "Stucco", "Stone", "Plywood", 
                                             "Other","MetalSd", "ImStucc", "HdBoard", "CemntBd", "BrkFace", 
                                             "BrkComm", "AsbShng"),
                             "MasVnrType"=c("None", "BrkFace", "Stone", "BrkCmn"),
                             "Foundation"=c("Slab", "Stone", "Wood", "PConc", "CBlock", "BrkTil"),
                             "Heating"=c("Floor", "GasA", "GasW", "Grav", "OthW", "Wall"),
                             "CentralAir"=c("Y", "N"),
                             "GarageType"=c("no_feature", "2Types", "Attchd", "Basment", 
                                            "BuiltIn", "CarPort", "Detchd"),
                             "MiscFeature"=c("no_feature", "Gar2", "Othr", "Shed", "TenC"),
                             "SaleType"=c("New", "COD", "Con", "ConLD", "ConLI", 
                                          "ConLw", "CWD", "Oth", "WD"),
                             "SaleCondition"=c("Normal", "Partial", "Abnorml", 
                                               "AdjLand", "Alloca", "Family"))
  
  return(nominal_references)
  
}


get_ordinal_conversion_list <- function(){
  
  conversion_list <- list("LotShape"=c("IR3", "IR2", "IR1", "Reg"),
                          "Utilities"=c("ELO", "NoSeWa", "NoSewr", "AllPub"),
                          "LandSlope"=c("Gtl", "Mod", "Sev"),
                          "OverallCond"=c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                          "ExterQual"=c("Fa", "TA", "Gd", "Ex"),
                          "ExterCond"=c("Po", "Fa", "TA", "Gd", "Ex"),
                          "BsmtQual"=c("no_feature", "Fa", "TA", "Gd", "Ex"),
                          "BsmtCond"=c("no_feature", "Po", "Fa", "TA", "Gd", "Ex"),
                          "BsmtExposure"=c("no_feature", "No", "Mn", "Av", "Gd"),
                          "BsmtFinType1"=c("no_feature", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
                          "BsmtFinType2"=c("no_feature", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
                          "HeatingQC"=c("Po", "Fa", "TA", "Gd", "Ex"),
                          "Electrical"=c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"),
                          "KitchenQual"=c("Po", "Fa", "TA", "Gd", "Ex"),
                          "Functional"=c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"),
                          "FireplaceQu"=c("no_feature", "Po", "Fa", "TA", "Gd", "Ex"),
                          "GarageFinish"=c("no_feature", "Unf", "RFn", "Fin"),
                          "GarageQual"=c("no_feature", "Po", "Fa", "TA", "Gd", "Ex"),
                          "GarageCond"=c("no_feature", "Po", "Fa", "TA", "Gd", "Ex"),
                          "PavedDrive"=c("N", "P", "Y"),
                          "PoolQC"=c("no_feature", "Fa", "TA", "Gd", "Ex"),
                          "Fence"=c("no_feature", "MnWw", "GdWo", "MnPrv", "GdPrv"))
  
  return(conversion_list)
  
}


convert_ordinals_to_integers <- function(df, conv_list, feature_names, name_suffx){
  
  for(index in 1:length(feature_names)){
    
    # Grab the name of the feature at this index
    feature_name <- feature_names[[index]]
    
    # Use the feature name to grab the ordinal values
    ordinal_values <- conv_list[[feature_name]]
    
    # Unique values actually present in the dataset for this ordinal feature
    unique_values_present <- unique(df[,feature_name])
    
    # Mask of TRUE and FALSES depending on if an ordinal level is actually present in the dataset
    values_contained_mask <- (ordinal_values %in% unique_values_present)
    
    
    # Create a numeric value for all possible levels, even the ones that aren't present.
    numeric_values <- seq(from=1, 
                          to=length(ordinal_values),
                          by=1)
    
    # Remove any numeric values associated with potential values of the ordinal variable that 
    # do not actually show up in this dataset
    numeric_values_present <- numeric_values[values_contained_mask]
    
    # Remove the ordinal levels associated with values not actually present in the dataset
    ordinal_levels_present <- ordinal_values[values_contained_mask]
    
    # Create a new feature name by appending a suffix if desired (default is to 
    # leave the name the same).
    new_feature_name <- paste0(feature_name, name_suffx)
    
    df[,new_feature_name] <- plyr::mapvalues(df[,feature_name],
                                             from=ordinal_levels_present,
                                             to=numeric_values_present)
    
    df[,new_feature_name] <- as.integer(df[,new_feature_name])
    
  }
  
  return(df)
  
}

convert_ordinals_to_factors <- function(df, conv_list, feature_names, order_factors,
                                        name_suffx){
  
  # Iterate over the list of feature names
  for(index in 1:length(feature_names)){
    
    # Grab the name of the feature at this index
    feature_name <- feature_names[[index]]
    
    # Use the feature name to grab the ordinal values
    ordinal_values <- conv_list[[feature_name]]
    
    # Create a new feature name by appending a suffix if desired (default is to 
    # leave the name the same).
    new_feature_name <- paste0(feature_name, name_suffx)
    
    
    # CONVERT THE FEATURE EITHER TO AN ORDINAL OR NON-ORDERED FACTOR.
    df[,new_feature_name] <-  factor(x=df[,feature_name], 
                                     levels = ordinal_values, 
                                     order = order_factors)
    
  }
  
  return(df)
  
}


clean_ordinal_values <- function(df, ordinal_as_factor, order_ordinal_factors, 
                                 ordinal_as_integer, feature_name_suffix){
  
  
  # A list mapping column names for ordinal features to their categories (in the proper order, which is 
  # necessary if ordered factors are ever to be used).
  conv_list <- get_ordinal_conversion_list()
  
  # Get a list of the names of the ordinal features
  feature_names <- names(conv_list)
  
  if(ordinal_as_factor){
    
    df <- convert_ordinals_to_factors(df=df, 
                                      conv_list=conv_list,
                                      feature_names=feature_names,
                                      order_factors=order_ordinal_factors,
                                      name_suffx=feature_name_suffix)
    
  }else if(ordinal_as_integer){
    
    df <- convert_ordinals_to_integers(df=df,
                                       conv_list=conv_list,
                                       feature_names=feature_names,
                                       name_suffx=feature_name_suffix)
    
  }
  
  return(df)
  
}

clean_nominal_features <- function(df){
  
  nominal_features <- get_nominal_feature_map()
  
  nominal_feature_names <- names(nominal_features)
  
  
  df <- convert_ordinals_to_factors(df=df, 
                                    conv_list=nominal_features,
                                    feature_names=nominal_feature_names,
                                    order_factors=FALSE,
                                    name_suffx="")
  
  return(df)
  
}

#################################### END Data Cleaning #################################### 



#################################### FEATURE SELECTION SECTION ####################################

prepare_model_dataset <- function(df, target, extra_exclusions){
  
  non_predictor_columns <- c("SalePrice", "Log_SalePrice", "Id", "target")
  
  non_predictor_columns <- append(non_predictor_columns, extra_exclusions)
  
  # Create a list of potential predictors, by removing the target and any other features we
  # wish to manually exclude
  potential_predictors <- names(df)[!(names(df) %in% non_predictor_columns)]
  
  write.csv(df, "./troubleshoot.csv")
  
  # Create a dataframe containing all potential predictors, and the desired target
  model_df <- df[, (names(df) %in% potential_predictors)]
  
  if("target" %in% names(df)){
    model_df[,"target"] <- df[,"target"]
  }else{
    model_df[,"target"] <- df[,target]  
  }
  
  
  return(model_df)
  
}


filter_inf_vif_columns <- function(df, target, training_data, save_path,
                                   vif_threshold, verbose){
  
  
  if(training_data){
    
    # Dataframe to create the lm model with
    vif_model_df <- prepare_model_dataset(df=df, target=target, extra_exclusions=c())
    
    # Fit the model and compute VIFs
    fit <- lm(formula=target~., data=vif_model_df, x=TRUE)
    model_vifs <- vif(fit)
    max_vif = max(model_vifs)
    
    # Get a vector of all predictor names that resulted in a lm
    # coefficient that had infinite VIF
    high_vif_columns <- c()
    
    loop_count <- 0
    
    while(max_vif >= vif_threshold){
      
      loop_count <- loop_count + 1
      
      # Get the names of all features that ended up with infinite VIF
      high_vif_names <- names(model_vifs[model_vifs >= vif_threshold])
      
      # Get a vector of all predictor names
      column_names <- names(vif_model_df)
      predictor_names <- column_names[column_names != target]
      
      for(col_index in 1:length(predictor_names)){
        
        column_name <- predictor_names[[col_index]]
        
        if(sjmisc::str_contains(x=high_vif_names, pattern=column_name)){
      
          high_vif_columns <- append(high_vif_columns, column_name)
        }
      }    
      
      df <- df[,!(names(df) %in% high_vif_columns)]  
      vif_model_df <- prepare_model_dataset(df=df, target=target, extra_exclusions=c())
      
      # Fit the model and compute VIFs
      fit <- lm(formula=target~., data=vif_model_df, x=TRUE)
      model_vifs <- vif(fit)
      max_vif <- max(model_vifs)
      
      if(verbose){
        cat("\n\n")
        print("******************************************************")
        print(paste0("Finished Loop: ", loop_count))
        cat("\n")
        print("Features Removed: ")
        cat("\n")
        print(high_vif_columns)
        cat("\n")
        print("Max Remaining VIF: ")
        print(max_vif)
        cat("\n")
        print("Model VIFS:")
        print(model_vifs)
        cat("\n\n")
        print("******************************************************")
        cat("\n\n")        
      }
    }
    
  
    # Save the columns removed, so we can mimic this on test data.
    vif_removal_df <- data.frame(high_vif_features=high_vif_columns)
    write.csv(vif_removal_df, save_path)
    return(vif_model_df)
    
  }
  else{ #else, this is the test data

      vif_df <- read.csv(save_path)
      columns_to_remove <- vif_df[,"high_vif_features"]
      df <- df[,!(names(df) %in% columns_to_remove)]
      return(df)

  }
}

build_lm_from_preds <- function(m_df, preds){
  
  l_df <- m_df[,(names(m_df) %in% preds)]
  l_df[,"target"] <- m_df[,"target"]
  
  lm_fitted <- lm(target~., data=l_df)
  
  return(lm_fitted)
}


build_ols_regress_from_preds <- function(m_df, preds){
  
  l_df <- m_df[,(names(m_df) %in% preds)]
  l_df[,"target"] <- m_df[,"target"]
  
  ols_regression <- olsrr::ols_regress(target~., data=l_df)
  
  return(ols_regression)
}


perform_best_subset_regression <- function(regression_df){
  
  gaurenteed_names <- c("TotalIndoorSF")
  
  non_sampleable <- c(gaurenteed_names, "target")
  
  potential_names <- names(regression_df)[!(names(regression_df) %in% non_sampleable)]
  
  num_potential_features <- length(potential_names)
  
  if(num_potential_features > 10){
    num_samples <- 10
  }else{
    num_samples <- num_potential_features
  }
  
  
  feature_samples <- sample(x=seq(from=1, 
                                  to=num_potential_features, 
                                  by=1),
                            size=num_samples,
                            replace=FALSE)
  
  selected_names <- potential_names[feature_samples]
  
  for(name_index in 1:length(gaurenteed_names)){
    
    current_name <- gaurenteed_names[name_index]
    
    if(current_name %in% potential_names){
      selected_names <- append(selected_names, current_name)    
    }
  }
  
  reg_df <<- regression_df[,selected_names]
  reg_df[,"target"] <<- regression_df[,"target"]
  
  best_fit <<- lm(target~., data=reg_df)
  
  best_subset <- olsrr::ols_step_best_subset(best_fit)
  
  best_subset_df <- build_best_subset_df(subset_result=best_subset,
                                         model_dataframe=reg_df)
  
  return(best_subset_df)
  
  
}


build_best_subset_df <- function(subset_result, model_dataframe, subset_metric="sbic"){
  
  
  
  bs_df <- data.frame(subset_result)
  
  best <- bs_df[order(bs_df[,subset_metric], decreasing=FALSE),][1,]
  
  best_preds <- best[,"predictors"]
  
  best_pred_vector <- strsplit(x=best_preds, split=" ")[[1]]
  
  num_preds <- length(best_pred_vector)
  
  best_regress <- build_ols_regress_from_preds(m_df=model_dataframe,
                                               preds=best_pred_vector)
  
  
  
  coef_determ <- best_regress$r
  rmse <- best_regress$sigma
  coef_variation <- best_regress$cv
  mse <- best_regress$mse
  mae <- best_regress$mae
  sbc <- best_regress$sbc
  sbic <- best_regress$sbic
  pred_rsq <- best_regress$prsq
  tss <- best_regress$tss
  rss <- best_regress$rss
  ess <- best_regress$ess
  rms <- best_regress$rms
  ems <- best_regress$ems
  aic <- best_regress$aic
  adj_rsquare <- best_regress$adjr
  rsquare <- best_regress$rsq
  
  search_fit <- build_lm_from_preds(m_df=model_dataframe,
                                    preds=best_pred_vector)
  
  
  msep <- ols_msep(model=search_fit)
  PRESS <- ols_press(model=search_fit)
  
  result_data <- data.frame(selection_algorithm=c("Best Subset (SBIC)"),
                            predictors_chosen=c(best_preds),
                            num_predictors=c(num_preds),
                            PRESS=c(PRESS),
                            MSEP=c(msep),
                            AIC=c(aic), 
                            SBC=c(sbc),
                            SBIC=c(sbic),
                            RMSE=c(rmse),
                            MSE=c(mse),
                            MAE=c(mae),
                            Adj_Rsquare=c(adj_rsquare),
                            Pred_Rquare=c(pred_rsq),
                            RSquare=c(rsquare),
                            Coef_Determ=c(coef_determ),
                            Coef_Var=c(coef_variation),
                            Total_SS=c(tss),
                            Reg_SS=c(rss),
                            Err_SS=c(ess),
                            RMS=c(rms),
                            EMS=c(ems),
                            Function_Used=c("ols_step_best_subset"))
  
  return(result_data)
  
}


build_selection_df_from_search <- function(search, model_dataframe, algorithm_name, func_used){
  

  if(algorithm_name != "Stepwise (aic)"){
    model_predictors <- attr(search$model$terms , "term.labels")  
  }else{
    model_predictors <- search$predictors  # ISSUE WITH STEPWISE AIC
  }
  
  
  preds <- stringr::str_c(model_predictors, 
                          collapse=" ")
  
  num_preds <- length(model_predictors)
  
  
  best_regress <- build_ols_regress_from_preds(m_df=model_dataframe,
                                               preds=model_predictors)
  
  
  coef_determ <- best_regress$r
  rmse <- best_regress$sigma
  coef_variation <- best_regress$cv
  mse <- best_regress$mse
  mae <- best_regress$mae
  sbc <- best_regress$sbc
  sbic <- best_regress$sbic
  pred_rsq <- best_regress$prsq
  tss <- best_regress$tss
  rss <- best_regress$rss
  ess <- best_regress$ess
  rms <- best_regress$rms
  ems <- best_regress$ems
  aic <- best_regress$aic
  adj_rsquare <- best_regress$adjr
  rsquare <- best_regress$rsq
  
  search_fit <- build_lm_from_preds(m_df=model_dataframe,
                                    preds=model_predictors)
  
  msep <- ols_msep(model=search_fit)
  PRESS <- ols_press(model=search_fit)
  
  
  result_data <- data.frame(selection_algorithm=c(algorithm_name),
                            predictors_chosen=c(preds),
                            num_predictors=c(num_preds),
                            PRESS=c(PRESS),
                            MSEP=c(msep),
                            AIC=c(aic), 
                            SBC=c(sbc),
                            SBIC=c(sbic),
                            RMSE=c(rmse),
                            MSE=c(mse),
                            MAE=c(mae),
                            Adj_Rsquare=c(adj_rsquare),
                            Pred_Rquare=c(pred_rsq),
                            RSquare=c(rsquare),
                            Coef_Determ=c(coef_determ),
                            Coef_Var=c(coef_variation),
                            Total_SS=c(tss),
                            Reg_SS=c(rss),
                            Err_SS=c(ess),
                            RMS=c(rms),
                            EMS=c(ems),
                            Function_Used=c(func_used))
  
  return(result_data)
  
}

build_selection_df_p <- function(p_selections){
  
  fwd <- p_selections$forward
  bwd <- p_selections$backward
  stp <- p_selections$stepwise
  
  model_dataframe <- p_selections$model_dataframe
  
  fwd_result_df <- build_selection_df_from_search(search=fwd,
                                                  model_dataframe=model_dataframe,
                                                  algorithm_name="Forward (p-values)",
                                                  func_used="ols_step_forward_p")
  
  
  
  bwd_result_df <- build_selection_df_from_search(search=bwd,
                                                  model_dataframe=model_dataframe,
                                                  algorithm_name="Backward (p-values)",
                                                  func_used="ols_step_backward_p")
  
  stp_result_df <- build_selection_df_from_search(search=stp,
                                                  model_dataframe=model_dataframe,
                                                  algorithm_name="Stepwise (p-values)", 
                                                  func_used="ols_step_both_p") 
  
  
  combined_p_df <- rbind(fwd_result_df, bwd_result_df, stp_result_df)
  
  
  return(combined_p_df)
  
}

build_selection_df_aic <- function(aic_selections){
  
  fwd <- aic_selections$forward
  bwd <- aic_selections$backward
  stp <- aic_selections$stepwise
  
  model_dataframe <- aic_selections$model_dataframe
  
  fwd_result_df <- build_selection_df_from_search(search=fwd,
                                                  model_dataframe=model_dataframe,
                                                  algorithm_name="Forward (aic)",
                                                  func_used="ols_step_forward_aic")
  
  
  
  bwd_result_df <- build_selection_df_from_search(search=bwd,
                                                  model_dataframe=model_dataframe,
                                                  algorithm_name="Backward (aic)",
                                                  func_used="ols_step_backward_aic")
  
  stp_result_df <- build_selection_df_from_search(search=stp,
                                                  model_dataframe=model_dataframe,
                                                  algorithm_name="Stepwise (aic)", 
                                                  func_used="ols_step_both_aic") 
  
  
  combined_df <- rbind(fwd_result_df, bwd_result_df, stp_result_df)
  
  return(combined_df)
  
}

create_parameter_text_file <- function(ordinal_as_factor, ordinal_as_integer, imbalance_threshold,
                                       vif_threshold, filter_vifs, extra_feature_exclusions, p_remove,
                                       p_enter, target, param_filepath){
  
  
  cat("=========================================\n", file=param_filepath)
  cat("              PARAMETER REPORT           ", file=param_filepath, append=TRUE)
  cat("\n=========================================\n\n", file=param_filepath, append=TRUE)
  
  
  cat("Target Column: ", target, "\n\n", file=param_filepath, append=TRUE)
  
  if(filter_vifs){
    cat("Filter VIFs: Yes\n", file=param_filepath, append=TRUE)
    cat("VIF Filter Threshold: ", vif_threshold, "\n", file=param_filepath, append=TRUE)
  }else{
    cat("Filter VIFs: No\n", file=param_filepath, append=TRUE)
    cat("VIF Filter Threshold: N/A \n", file=param_filepath, append=TRUE)
  }
  
  if(ordinal_as_integer){
    cat("Ordinal Feature Format: Integer\n", file=param_filepath, append=TRUE)
    cat("Categorical Imbalance Threshold: ", imbalance_threshold, "\n", file=param_filepath, append=TRUE)
  }else if(ordinal_as_factor){
    cat("Ordinal Feature Format: Factor\n", file=param_filepath, append=TRUE)
    cat("Categorical Imbalance Threshold: ", imbalance_threshold, "\n", file=param_filepath, append=TRUE)
  }

  
  cat("P-value to Enter: ", p_enter, "\n", file=param_filepath, append=TRUE)
  cat("P-value to Remove", p_remove, "\n\n", file=param_filepath, append=TRUE)
  
  cat("Extra feature exclusions: ", extra_feature_exclusions, "\n", file=param_filepath, append=TRUE)
  
  return("Parameter File Created")
  
}

perform_test_set_evaluations <- function(clean_test_data, clean_train_data, selected_models_df,
                                         project_directory="./"){
  
  #clean_train_data[,"target"] <- clean_train_data[,"SalePrice"]
  
  num_models <- nrow(selected_models_df)
  
  ks_test_stats <- c()
  ks_p_values <- c()
  
  for(model_index in 1:num_models){
    
    features <- selected_models_df[model_index, "predictors_chosen"]
    feature_vector <- strsplit(features, split=" ")[[1]]
    
    model_name <- selected_models_df[model_index, "selection_algorithm"]
    submission_filepath <- paste0(project_directory, "/SUBISSION_", model_name, ".csv")
    
    train_df <<- clean_train_data[,feature_vector]
    train_df[,"target"] <<- clean_train_data[,"target"]
    
    test_df <- clean_test_data[,feature_vector]
    
    fit <- lm(target~., data=train_df)
    
    # PREDICT ON THE TEST SET
    predictions <- predict(fit, newdata=test_df)
    
    # PREDICT ON THE TRAINING SET (ONLY FOR KS.TEST USAGE)
    train_predictions <- predict(fit, newdata=train_df[,(names(train_df) != "target")])
    ks_result <- dgof::ks.test(x=train_predictions, y=predictions)
    ks_test_stats <- append(ks_test_stats, ks_result$statistic[[1]])
    ks_p_values <- append(ks_p_values, ks_result$p.value)
    
    submission_df <- data.frame(Id=clean_test_data[,"Id"], SalePrice=predictions)
    write.csv(submission_df, submission_filepath, row.names=FALSE)
    
  }
  
  return(list(kstest_stats=ks_test_stats,
              kstest_pvalues=ks_p_values))
  
}

generate_kaggle_submission <- function(fit, submission_filepath, test_data, log_target){
  
  predictions <- predict(fit, newdata=test_data)
  
  if(log_target){
    predictions <- exp(predictions)
  }
  
  submission_df <- data.frame(Id=test_data[,"Id"], SalePrice=predictions)
  
  write.csv(submission_df, submission_filepath, row.names=FALSE)
  
  return(submission_df)
  
}


filter_dataframe <- function(df, metric, order_decreasing=FALSE, algo_type=NULL, min_ks_pvalue=NULL){
  
  if(!is.null(algo_type)){
    
    if(algo_type == "backward"){
      df <- df[((df[,"selection_algorithm"] == "Backward (aic)") | (df[,"selection_algorithm"] == "Backward (p-values)")),]
    }else if(algo_type == "forward"){
      df <- df[((df[,"selection_algorithm"] == "Forward (aic)") | (df[,"selection_algorithm"] == "Forward (p-values)")),]
    }else if(algo_type == "stepwise"){
      df <- df[((df[,"selection_algorithm"] == "Stepwise (aic)") | (df[,"selection_algorithm"] == "Stepwise (p-values)")),]
    }else if(algo_type == "best_subset"){
      df <- df[(df[,"selection_algorithm"] == "Best Subset (SBIC)"),]
    }
  }
  
  if(!is.null(min_ks_pvalue)){
    df <- df[df[,"KS_Pvalues"] >= min_ks_pvalue,]
    
  }
  
  df <- df[order(df[,metric], decreasing=order_decreasing),]
  
  return(df)
}
