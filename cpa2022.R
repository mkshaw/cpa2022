# 0 Load data and dependencies --------------------------------------------

library(r2mlm) # this also loads lme4
library(lmerTest) # significance for coefficients
library(performance)
library(dplyr)

# 1 Null Model ------------------------------------------------------------

# Teachers clustered within classes

null_model <- lmer(satisfaction ~ 1 + (1|schoolID), data = teachsat, REML = TRUE)
summary(null_model)

r2mlm(null_model)

# Three sets of output: Decompositions, R2s, and Graph
# Decompositions give you the unique R-squareds
# R2s gives you the unique R-squareds and the combinations
# Graph visualizes Decompositions

# My order of reading is usually: R2s, then look at the graph

# The only variance explained in job satisfaction is explained by group membership (random intercept)
# That's the intraclass correlation!
# So 31.6% of variance in job satisfaction is attributed to cluster

# For comparison, we can use the performance package to calculate the ICC
performance::icc(null_model)

# They match, as expected!

# 2 Full Model ------------------------------------------------------------

# Level-1: salary (centered within school)
# Level-2: student-teacher ratio (same for all teachers within a school, differs across schools)

model <- lmer(satisfaction ~ 1 + salary_c + s_t_ratio + (salary_c|schoolID), 
              data = teachsat, 
              REML = TRUE,
              control = lmerControl(optimizer = "bobyqa")) # optimizer change to help convergence (to make the example cleaner)
summary(model) # Instructive to look at our unstandardized results, too: coefficient values

r2mlm(model)

# 3 Manual Entry ----------------------------------------------------------

r2mlm_manual(
  data = teachsat,
  within_covs = c("salary_c"),
  between_covs = c("s_t_ratio"),
  random_covs = c("salary_c"),
  gamma_w = c(0.074648),
  gamma_b = c(7.189783, -0.037282),
  Tau = as.matrix(bdiag(VarCorr(model))),
  sigma2 = getME(model, "sigma")^2,
  has_intercept = TRUE,
  clustermeancentered = TRUE
)

# Compare to the automatic entry: they're the same!

# 4 Model Comparison ------------------------------------------------------

r2mlm_comp(null_model, model)

# 5 graph outputs: model A and B each overall, then comparisons of within, between, and total
# There is also manual entry for model comparison

# 5 Non-CWC Model Options -------------------------------------------------

# You can see the centering, each school has a mean of zero
teachsat %>% 
  group_by(schoolID) %>% 
  summarize(
    mean(salary_c)
  )

# Remove centering on salary by adding a constant to each value
teachsat <- teachsat %>%
  mutate(salary = salary_c + 1)

# You can see mean is no longer zero
teachsat %>% 
  group_by(schoolID) %>% 
  summarize(
    mean(salary)
  )

# Model with this new salary
model_uncwc <- lmer(satisfaction ~ 1 + salary + s_t_ratio + (salary|schoolID), 
              data = teachsat, 
              REML = TRUE,
              control = lmerControl(optimizer = "bobyqa")) 
summary(model_uncwc)

# If you use the regular r2mlm function, you get just an overall breakdown

r2mlm(model_uncwc)

# You can use r2mlm_long to get a full breakdown (currently only available as manual function)

r2mlm_long_manual(
  data = teachsat,
  covs = c("salary", "s_t_ratio"),
  random_covs = c("salary"),
  clusterID = "schoolID",
  gammas = c(0.074648, -0.037282),
  Tau = as.matrix(Matrix::bdiag(VarCorr(model_uncwc))),
  sigma = getME(model_uncwc, "sigma")^2,
  bargraph = TRUE
)

# Three outputs: Decompositions, R2s, Graph
# Similar to r2mlm(), but now there are two v terms
# Recall that v is variance attributed to random slopes
# v1 is slope variation within a cluster
# v2 is slope variation between clusters
# Why two? When a level-1 variable is centered within cluster, every cluster has a mean of 0 and there is no variance between clusters. All the variance is within clusters
# When a level-1 variable is NOT centered within cluster, it varies both within and between clusters

# What does that look like with our centered model?

summary(model)

r2mlm_long_manual(
  data = teachsat,
  covs = c("salary_c", "s_t_ratio"),
  random_covs = c("salary_c"),
  clusterID = "schoolID",
  gammas = c(0.074648, -0.037282),
  Tau = as.matrix(Matrix::bdiag(VarCorr(model))),
  sigma = getME(model, "sigma")^2,
  bargraph = TRUE
)

# The same, as expected! 
# The change: v2 is essentially zero, because there is no variance in salary_c across groups
