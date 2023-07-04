library(brms)
library(dplyr)
library(brms)
library(ggplot2)
library(tidyr)
library(glue)

data$Gender <- factor(data$Gender, levels = c("Male", "Female", "Other"))
data$Gender_numeric <- ifelse(data$Gender == "Male", 1, ifelse(data$Gender == "Female", 0, 2))


data$VRHeadset <- factor(data$VRHeadset, levels = c("HTC Vive","PlayStation VR","Oculus Rift"))
data$VRHeadset_numeric <- ifelse(data$VRHeadset == "HTC Vive", 1, ifelse(data$VRHeadset == "PlayStation VR", 0, 2))



def_priors <-
  #c(mean(challenger_0$nfails.field), 2.5* sd(challenger_0$nfails.field))
  c(prior(normal(2.976, 3.524389), class = Intercept),
    #2.5 * sd(challenger_0$nfails.field)/sd(challenger_0$c_temp)
    prior(normal(0, 0.1168445), class = b, coef = Age),
    #2.5 * sd(challenger_0$nfails.field)/sd(challenger_0$c_pres)
    prior(normal(0, 1.718569), class = b, coef = Gender_numeric),
    #2.5 * sd(challenger_0$nfails.field)/sd(challenger_0$c_pres * challenger_0$c_temp)
    prior(normal(0, 1.724959), class = b, coef = VRHeadset_numeric),
    
    prior(normal(0, 0.0893137), class = b, coef = Duration),
    prior(normal(0, 0.4911699), class = b, coef = MotionSickness),      
    #1/sd(challenger_0$nfails.field)
    prior(exponential(0.2837371), class = sigma))




model <- brm(ImmersionLevel ~ Age + Gender_numeric + VRHeadset_numeric + Duration + MotionSickness , data = data, family = gaussian(),prior = def_priors)

pp_check(model, type = "stat_2d")
pp_check(model)
summary(model)