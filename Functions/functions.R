



############# ANOVA FUNCTION #########################################
# creating the function anova_nunu because it's mine. 
# the arguments of the fonction will be our data set, the independant variable and the dependant variable of interest. 

anova_nunu <- function(data, indep_var, dep_var ) {
  
  # we want to make sure that the independent variable is a categorical variable. I set it up such that if the independent variable is not a factor, an error appears and stops the computation. 
  if (is.factor(data %>%  pull({{indep_var}})) ) {
    print("indep var is a factor")
  } else {
    stop("indep var is not a factor")
  }
  
  # same idea here for the dependent varible which has to be a numeric. Otherwise, an error shows. 
  if (is.numeric(data %>% pull({{dep_var}}))) {
    print("dep var is numeric")
  } else {
    stop("dep var is not numeric")
  }
  
  #Ok... So I couldn't figure out how to feed the data to aov() without having an error. I did resort to used chat GPT for that bit of code. 
  
  # I am not exactly sure what all the functions do, but the idea is the following. We take the column names from the data set, paste it so that it looks like "dep_var ~ indep var" (required input to the aov() function) and tell R to process that as a formula. 
  formula <- as.formula(paste(deparse(substitute(dep_var)), "~", deparse(substitute(indep_var))))
  
  # calculating the anova based on the formula above and with our data set.
  anova <- aov(formula, data = data)
  anova_result <<- anova # saves anova in the RStudio  environment
  
  # applying the Tukey's test
  tukey <- TukeyHSD(anova) 
  tukey <<- tukey # saves tukey in the RStudio  environment
  
  #generating the compact letter display (cld).
  cld <- multcompLetters4(anova,tukey)
  
  #generating a data set with the cld that can be plotted with the quantiles
  Tk<- data %>% 
    group_by({{indep_var}}) %>% 
    summarize(avg = mean({{dep_var}}, na.rm = T),
              quant = quantile({{dep_var}}, probs = 0.75, na.rm = T)) %>% 
    arrange(desc(avg))
  
  # pull the cld and adding them to the data set with the quantiles.
  cld_df <- as.data.frame.list(cld[[1]])
  Tk$cld <- cld_df$Letters
  
  # saves Tk in the RStudio  environment
  Tk <<- Tk  
  
  # tell the user that Tk has been saved and can be used for plotting
  print("Tk saved in environment" )
  return(Tk)
  
}

