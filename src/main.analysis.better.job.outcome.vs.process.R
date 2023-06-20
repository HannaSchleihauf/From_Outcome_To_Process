# Project: PRR
# Analysis: Better-Job Variable only Outcome vs. Process Condition
# Author: Hanna Schleihauf
# Date: September 23 2021

## Analysis with the only the data from the outcome vs process condition: 
## Participants of all age groups --------

## Load necessary packages and data -------------------
library("lme4")
library("tidyverse")
library("tidyselect")
library("parallel")
library("dfoptim")
library("optimx")
source("./functions/diagnostic_fcns.r")
source("./functions/glmm_stability.r")
source("./functions/drop1_para.r")
source("./functions/boot_glmm.r")
library("emmeans")
library("car")

xdata <- read.csv("./data/PRR_data.csv", header = TRUE)
ovpdata <- subset(xdata, xdata$condition == "outcome.vs.process")

## Inspect data ------------------------------------
xx <- ftable(better.job ~ culture + age.group.sum, ovpdata)
xx
# we do not have a complete separation problem

# how many trials are excluded because children did not make a choice
xx <- ovpdata[is.na(ovpdata$better.job), ] # --> none

## Prepare data for model fitting--------------------
xx.fe.re <- fe.re.tab(
  fe.model = "better.job ~ age.group.sum*culture",
  re = "(1|sub.id)", other.vars = c("age", "age.group", "trial.nr"), 
  data = xdata
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

# center dummy variables (necessary for the random effects in the 
# model and potentially plotting)
t.data$culture.code <- t.data$culture.usa - mean(t.data$culture.usa)

t.data$age.group.sum.6.7.code <- 
 t.data$age.group.sum.6.7 - mean(t.data$age.group.sum.6.7)
t.data$age.group.sum.8.9.code <- 
 t.data$age.group.sum.8.9 - mean(t.data$age.group.sum.8.9)
t.data$age.group.sum.adult.code <- 
 t.data$age.group.sum.adult - mean(t.data$age.group.sum.adult)

t.data$z.age <- scale(t.data$age)

# fitting the model as pre-registered
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
full <- glmer(better.job ~ age.group.sum * culture +
  (1 | sub.id),
data = t.data, control = contr, family = binomial(link = "logit")
)

round(summary(full)$coefficients, 3)

# assumptions
overdisp.test(full)
# check for colliniarity
xx <- glmer(better.job ~ age.group.sum + culture +
  (1 | sub.id),
data = t.data, control = contr, family = binomial(link = "logit")
)
library(car)
vif(xx)

# checking model stability
m.stab.b <- glmm.model.stab(model.res = 
                             full, contr = contr, use = c("sub.id"))
m.stab.b$detailed$warnings
xx <- as.data.frame(round(m.stab.b$summary[, -1], 3))
dev.off()
m.stab.plot(round(m.stab.b$summary[, -1], 3))
write.table(xx, "analysis.outcome.vs.process.m.stab.txt", 
            quote = FALSE, sep = "\t")
xx

# null model
null <- 
 glmer(better.job ~ 1 +
  (1 | sub.id),
data = t.data, control = contr, family = binomial(link = "logit")
)
round(anova(full, null, test = "Chisq"), 3)

# main effects model
main <- glmer(better.job ~ age.group.sum + culture +
  (1 | sub.id),
data = t.data, control = contr, family = binomial(link = "logit")
)

# reduced model comparison of full model
tests <- drop1p(model.res = full, para = F, data = NULL, 
                contr = contr, n.cores = c("all-1", "all"), 
                to.del = NULL)
round(tests$drop1.res, 3)

# reduced model comparison of main effects model
tests <- drop1p(model.res = main, para = F, data = NULL, 
                contr = contr, n.cores = c("all-1", "all"), 
                to.del = NULL)
round(tests$drop1.res, 3)

# bootstraps of full model
boot.res <- boot.glmm.pred(
  model.res = full, excl.warnings = T,
  nboots = 1000, para = F, level = 0.95, use = c("age.group", "culture")
)
round(boot.res$ci.estimates, 3)
as.data.frame(round(boot.res$ci.estimates, 3))
m.stab.plot(round(boot.res$ci.estimates, 3))
boot.res$ci.predicted

library(emmeans)
emm1 <- emmeans(full, ~ age.group.sum * culture)
summary(emm1, type = "response")

emmeans(full, pairwise ~ age.group.sum * culture)
xx <- summary(pairs(emm1), type = "response")
summary(pairs(regrid(emm1)), type = "response")

summary(contrast(emm1, "pairwise")[c(1, 23, 8, 26, 14, 28, 4, 11, 17, 22)],
  type = "response", adjust = "fdr"
)

emmip(emm1, ~ age.group.sum * culture, type = "link")
emmip(emm1, ~ age.group.sum * culture, type = "response")

save.image("./R_images/analysis.better.job.outcome.vs.process.RData")


