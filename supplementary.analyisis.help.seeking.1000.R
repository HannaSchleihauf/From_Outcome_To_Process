# Project: PRR
# Analysis: Help-Seeking Variable
# Author: Hanna Schleihauf
# Date: September 9 2021

## Analysis with the full data set: Participants of all age groups --------

## Load necessary packages and data -------------------------------------------------
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

xdata <- read.csv("./PRR_data.csv", header = TRUE)

## Inspect data ------------------------------------
xx <- ftable(help.seeking ~ culture + condition + age.group.sum, xdata)
xx
write.xlsx(xx, file = "./help.seeking.frequencies.xlsx")
table(xdata$help.seeking, xdata$culture, xdata$condition, xdata$age.group.sum, useNA = "ifany")
# here we see that we have in a few cells a 0
# --> therefore we might have a complete separation problem

# how many trials are excluded because children did not make a choice
xx <- xdata[is.na(xdata$help.seeking), ]
table(xx$condition, xx$culture)

# interrater reli
reli.data <- subset(xdata, xdata$help.seeking.reli != "NA")
ratings <- reli.data %>% select(help.seeking, help.seeking.reli)
library(irr)
agree(ratings)

# Cohans Kappa:
(99.7 - 50) / (100 - 50)

## Prepare data for model fitting------------------------------------
xx.fe.re <- fe.re.tab(
  fe.model = "help.seeking ~ condition*age.group.sum*culture",
  re = "(1|sub.id)", other.vars = c("age", "age.group"), data = xdata
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data) # in t.data the 11 observations in which children did not make a choice are excluded
levels(t.data$condition) # outcome.only is reference category

# center dummy variables (necessary for the random effects in the model and potentially plotting)
t.data$condition.process.only.c <-
  t.data$condition.process.only - mean(t.data$condition.process.only)
t.data$condition.outcome.process.c <-
  t.data$condition.outcome.vs.process - mean(t.data$condition.outcome.vs.process)

t.data$culture.code <- t.data$culture.usa - mean(t.data$culture.usa)

t.data$age.group.sum.6.7.code <- t.data$age.group.sum.6.7 - mean(t.data$age.group.sum.6.7)
t.data$age.group.sum.8.9.code <- t.data$age.group.sum.8.9 - mean(t.data$age.group.sum.8.9)
t.data$age.group.sum.adult.code <- t.data$age.group.sum.adult - mean(t.data$age.group.sum.adult)

# fitting the model as pre-registered
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
full <- glmer(help.seeking ~ condition * age.group.sum * culture +
  (1 + (condition.process.only.c + condition.outcome.process.c) || sub.id),
data = t.data, control = contr, family = binomial(link = "logit")
)
# the model is not converging

# trying whether other control functions would make the model converge
full <- glmer(help.seeking ~ condition * age.group.sum * culture +
  (1 + (condition.process.only.c + condition.outcome.process.c) || sub.id),
data = t.data, family = binomial(link = "logit")
)
diff_optims <- allFit(full, maxfun = 1e5, parallel = "multicore", ncpus = detectCores())
is.OK <- sapply(diff_optims, is, "merMod")
diff_optims.OK <- diff_optims[is.OK]
convergence_results <- lapply(diff_optims.OK, function(x) x@optinfo$conv$lme4$messages)
working_indices.full <- sapply(convergence_results, is.null)

# Since these models also led convergence issues, we concluded that the might be caused by complete separation is some of the cells
# to tackle this problem, we create 1000 data sets, in each of which we randomly change one data point at a time in the cells in
# which we have complete separation. We then fit the models again, check how many of these 1000 models converged, and average the
# results over all converged models.

## Changing response to solve the complete separation problem --------------
# function to keep warnings
keepWarnings <- function(expr) {
  localWarnings <- list()
  value <- withCallingHandlers(expr,
    warning = function(w) {
      localWarnings[[length(localWarnings) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = localWarnings)
}

# make a new variable for the response
t.data$resp <- as.numeric(t.data$help.seeking)

# check again which cells have a complete separation issue (have no 1s in it)
ftable(resp ~ culture + condition + age.group.sum, t.data)
aggregate(t.data$resp, list(t.data$culture, t.data$condition, t.data$age.group.sum), mean)
length(t.data$resp)

# determining in which cells we will be changing one data point at a time
to.change.1 <- (1:nrow(t.data))[(t.data$condition == "process.only" & t.data$age.group.sum == "8.9" & t.data$culture == "china")]
to.change.2 <- (1:nrow(t.data))[(t.data$condition == "process.only" & t.data$age.group.sum == "adult" & t.data$culture == "usa")]
# to.change.others = (1:nrow(t.data))[t.data$resp == 1] #this way the number of 0 and 1 s stays the same

# creating empty variables to store the results
all.res <- data.frame(
  n.sim = c(1:1000),
  to.change.1 = NA,
  to.change.2 = NA,
  # full warning
  full.warnings = NA,
  # all.full.coeffs
  all.full.coeffs.intercept = NA,
  all.full.coeffs.conditionoutcome.vs.process = NA,
  all.full.coeffs.conditionprocess.only = NA,
  all.full.coeffs.age.group.sum6.7 = NA,
  all.full.coeffs.age.group.sum8.9 = NA,
  all.full.coeffs.age.group.sumadult = NA,
  all.full.coeffs.cultureusa = NA,
  all.full.coeffs.conditionoutcome.vs.process.age.group.sum6.7 = NA,
  all.full.coeffs.conditionprocess.only.age.group.sum6.7 = NA,
  all.full.coeffs.conditionoutcome.vs.process.age.group.sum8.9 = NA,
  all.full.coeffs.conditionprocess.only.age.group.sum8.9 = NA,
  all.full.coeffs.conditionoutcome.vs.process.age.group.sumadult = NA,
  all.full.coeffs.conditionprocess.only.age.group.sumadult = NA,
  all.full.coeffs.conditionoutcome.vs.process.cultureusa = NA,
  all.full.coeffs.conditionprocess.only.cultureusa = NA,
  all.full.coeffs.age.group.sum6.7.cultureusa = NA,
  all.full.coeffs.age.group.sum8.9.cultureusa = NA,
  all.full.coeffs.age.group.sumadult.cultureusa = NA,
  all.full.coeffs.conditionoutcome.vs.process.age.group.sum6.7.cultureusa = NA,
  all.full.coeffs.conditionprocess.only.age.group.sum6.7.cultureusa = NA,
  all.full.coeffs.conditionoutcome.vs.process.age.group.sum8.9.cultureusa = NA,
  all.full.coeffs.conditionprocess.only.age.group.sum8.9.cultureusa = NA,
  all.full.coeffs.conditionoutcome.vs.process.age.group.sumadult.cultureusa = NA,
  all.full.coeffs.conditionprocess.only.age.group.sumadult.cultureusa = NA,
  # null warnings
  null.warnings = NA,
  # test.full.null
  test.full.null.Chisq = NA,
  test.full.null.Df = NA,
  test.full.null.Pr..Chisq = NA,
  # test.3.way.int
  test.3.way.int.Chisq = NA,
  test.3.way.int.Chi.Df = NA,
  test.3.way.int.Pr..Chisq = NA,
  test.3.way.int.n.opt.warnings = NA,
  test.3.way.int.n.fun.warnings = NA,
  # red warnings
  red.warnings = NA,
  # red coefficients
  all.red.coeffs.intercept = NA,
  all.red.coeffs.conditionoutcome.vs.process = NA,
  all.red.coeffs.conditionprocess.only = NA,
  all.red.coeffs.age.group.sum6.7 = NA,
  all.red.coeffs.age.group.sum8.9 = NA,
  all.red.coeffs.age.group.sumadult = NA,
  all.red.coeffs.cultureusa = NA,
  all.red.coeffs.conditionoutcome.vs.process.age.group.sum6.7 = NA,
  all.red.coeffs.conditionprocess.only.age.group.sum6.7 = NA,
  all.red.coeffs.conditionoutcome.vs.process.age.group.sum8.9 = NA,
  all.red.coeffs.conditionprocess.only.age.group.sum8.9 = NA,
  all.red.coeffs.conditionoutcome.vs.process.age.group.sumadult = NA,
  all.red.coeffs.conditionprocess.only.age.group.sumadult = NA,
  all.red.coeffs.conditionoutcome.vs.process.cultureusa = NA,
  all.red.coeffs.conditionprocess.only.cultureusa = NA,
  all.red.coeffs.age.group.sum6.7.cultureusa = NA,
  all.red.coeffs.age.group.sum8.9.cultureusa = NA,
  all.red.coeffs.age.group.sumadult.cultureusa = NA,
  # red reduced model comparisons
  test.2.way.condition.age.group.sum.Chisq = NA,
  test.2.way.condition.age.group.sum.Chi.Df = NA,
  test.2.way.condition.age.group.sum.Pr..Chisq = NA,
  test.2.way.condition.age.group.sum.n.opt.warnings = NA,
  test.2.way.condition.age.group.sum.n.fun.warnings = NA,
  #
  test.2.way.condition.culture.Chisq = NA,
  test.2.way.condition.culture.Chi.Df = NA,
  test.2.way.condition.culture.Pr..Chisq = NA,
  test.2.way.condition.culture.n.opt.warnings = NA,
  test.2.way.condition.culture.n.fun.warnings = NA,
  #
  test.2.way.age.group.sum.culture.Chisq = NA,
  test.2.way.age.group.sum.culture.Chi.Df = NA,
  test.2.way.age.group.sum.culture.Pr..Chisq = NA,
  test.2.way.age.group.sum.culture.n.opt.warnings = NA,
  test.2.way.age.group.sum.culture.n.fun.warnings = NA,
  # main warnings
  main.warnings = NA,
  # main coefficients
  all.main.coeffs.intercept = NA,
  all.main.coeffs.conditionoutcome.vs.process = NA,
  all.main.coeffs.conditionprocess.only = NA,
  all.main.coeffs.age.group.sum6.7 = NA,
  all.main.coeffs.age.group.sum8.9 = NA,
  all.main.coeffs.age.group.sumadult = NA,
  all.main.coeffs.cultureusa = NA,
  # red3 (main effects) reduced model comparisons
  test.main.condition.Chisq = NA,
  test.main.condition.Chi.Df = NA,
  test.main.condition.Pr..Chisq = NA,
  test.main.condition.n.opt.warnings = NA,
  test.main.condition.n.fun.warnings = NA,
  #
  test.main.age.group.sum.Chisq = NA,
  test.main.age.group.sum.Chi.Df = NA,
  test.main.age.group.sum.Pr..Chisq = NA,
  test.main.age.group.sum.n.opt.warnings = NA,
  test.main.age.group.sum.n.fun.warnings = NA,
  #
  test.main.culture.Chisq = NA,
  test.main.culture.Chi.Df = NA,
  test.main.culture.Pr..Chisq = NA,
  test.main.culture.n.opt.warnings = NA,
  test.main.culture.n.fun.warnings = NA,
  # assumptions
  overdispersion.test.full = NA,
  colliniarity.test.condition = NA,
  colliniarity.test.age.group.sum = NA,
  colliniarity.test.culture = NA
)

emm.post.hoc.full <- c()
emm.post.hoc.full.cis <- c()
emm.post.hoc.red <- c()
emm.post.hoc.red.cis <- c()
emm.post.hoc.main <- c()
emm.post.hoc.main.cis <- c()

boot.full.values <- c()
boot.plot.1.values <- c()
boot.plot.2.values <- c()
boot.full.estimates <- c()
boot.plot.1.estimates <- c()
boot.plot.2.estimates <- c()

# starting the loop
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))

for (i in 1:1000) { # i=1
  set.seed(i)
  t.data$new.resp <- t.data$resp

  xx <- sample(to.change.1, 1, replace = TRUE)
  t.data$new.resp[xx] <- ifelse(t.data$new.resp[xx] == 0, 1, 0)
  all.res$to.change.1[i] <- xx

  xx <- sample(to.change.2, 1, replace = TRUE)
  t.data$new.resp[xx] <- ifelse(t.data$new.resp[xx] == 0, 1, 0)
  all.res$to.change.2[i] <- xx

  # Full model
  full1 <- keepWarnings(glmer(new.resp ~ condition * age.group.sum * culture +
    (1 + (condition.process.only.c + condition.outcome.process.c) || sub.id),
  data = t.data, family = binomial(link = "logit"), control = contr
  ))
  if (length(full1$warnings) == 0) {
    full <- full1$value
    all.res$full.warnings[i] <- "no"
    all.res$all.full.coeffs.intercept[i] <- summary(full)$coefficients["(Intercept)", 1]
    all.res$all.full.coeffs.conditionoutcome.vs.process[i] <- summary(full)$coefficients["conditionoutcome.vs.process", 1]
    all.res$all.full.coeffs.conditionprocess.only[i] <- summary(full)$coefficients["conditionprocess.only", 1]
    all.res$all.full.coeffs.age.group.sum6.7[i] <- summary(full)$coefficients["age.group.sum6.7", 1]
    all.res$all.full.coeffs.age.group.sum8.9[i] <- summary(full)$coefficients["age.group.sum8.9", 1]
    all.res$all.full.coeffs.age.group.sumadult[i] <- summary(full)$coefficients["age.group.sumadult", 1]
    all.res$all.full.coeffs.cultureusa[i] <- summary(full)$coefficients["cultureusa", 1]
    all.res$all.full.coeffs.conditionoutcome.vs.process.age.group.sum6.7[i] <- summary(full)$coefficients["conditionoutcome.vs.process:age.group.sum6.7", 1]
    all.res$all.full.coeffs.conditionprocess.only.age.group.sum6.7[i] <- summary(full)$coefficients["conditionprocess.only:age.group.sum6.7", 1]
    all.res$all.full.coeffs.conditionoutcome.vs.process.age.group.sum8.9[i] <- summary(full)$coefficients["conditionoutcome.vs.process:age.group.sum8.9", 1]
    all.res$all.full.coeffs.conditionprocess.only.age.group.sum8.9[i] <- summary(full)$coefficients["conditionprocess.only:age.group.sum8.9", 1]
    all.res$all.full.coeffs.conditionoutcome.vs.process.age.group.sumadult[i] <- summary(full)$coefficients["conditionoutcome.vs.process:age.group.sumadult", 1]
    all.res$all.full.coeffs.conditionprocess.only.age.group.sumadult[i] <- summary(full)$coefficients["conditionprocess.only:age.group.sumadult", 1]
    all.res$all.full.coeffs.conditionoutcome.vs.process.cultureusa[i] <- summary(full)$coefficients["conditionoutcome.vs.process:cultureusa", 1]
    all.res$all.full.coeffs.conditionprocess.only.cultureusa[i] <- summary(full)$coefficients["conditionprocess.only:cultureusa", 1]
    all.res$all.full.coeffs.age.group.sum6.7.cultureusa[i] <- summary(full)$coefficients["age.group.sum6.7:cultureusa", 1]
    all.res$all.full.coeffs.age.group.sum8.9.cultureusa[i] <- summary(full)$coefficients["age.group.sum8.9:cultureusa", 1]
    all.res$all.full.coeffs.age.group.sumadult.cultureusa[i] <- summary(full)$coefficients["age.group.sumadult:cultureusa", 1]
    all.res$all.full.coeffs.conditionoutcome.vs.process.age.group.sum6.7.cultureusa[i] <- summary(full)$coefficients["conditionoutcome.vs.process:age.group.sum6.7:cultureusa", 1]
    all.res$all.full.coeffs.conditionprocess.only.age.group.sum6.7.cultureusa[i] <- summary(full)$coefficients["conditionprocess.only:age.group.sum6.7:cultureusa", 1]
    all.res$all.full.coeffs.conditionoutcome.vs.process.age.group.sum8.9.cultureusa[i] <- summary(full)$coefficients["conditionoutcome.vs.process:age.group.sum8.9:cultureusa", 1]
    all.res$all.full.coeffs.conditionprocess.only.age.group.sum8.9.cultureusa[i] <- summary(full)$coefficients["conditionprocess.only:age.group.sum8.9:cultureusa", 1]
    all.res$all.full.coeffs.conditionoutcome.vs.process.age.group.sumadult.cultureusa[i] <- summary(full)$coefficients["conditionoutcome.vs.process:age.group.sumadult:cultureusa", 1]
    all.res$all.full.coeffs.conditionprocess.only.age.group.sumadult.cultureusa[i] <- summary(full)$coefficients["conditionprocess.only:age.group.sumadult:cultureusa", 1]

    # full-red model comparisons
    tests.full.red <- drop1p(model.res = full, para = F, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
    test.3.way.int <- as.data.frame(tests.full.red$drop1.res[2, c("Chisq", "Chi.Df", "Pr..Chisq.", "n.opt.warnings", "n.fun.warnings")])
    all.res$test.3.way.int.Chisq[i] <- test.3.way.int$Chisq
    all.res$test.3.way.int.Chi.Df[i] <- test.3.way.int$Chi.Df
    all.res$test.3.way.int.Pr..Chisq[i] <- test.3.way.int$Pr..Chisq.
    all.res$test.3.way.int.n.opt.warnings[i] <- test.3.way.int$n.opt.warnings
    all.res$test.3.way.int.n.fun.warnings[i] <- test.3.way.int$n.fun.warnings

    # assumptions
    all.res$overdispersion.test.full[i] <- overdisp.test(full)[1, c("dispersion.parameter")]

    # post hoc pairwise comparisons model with three-way interaction
    emm <- emmeans(full, ~ condition * age.group.sum * culture)
    emm.post.hoc.full.cis <- rbind(emm.post.hoc.full.cis, as.data.frame(emm))

    emm.pairs.full <- summary(contrast(emm, "pairwise")[c(
      1, 2, 3, 6, 9, 12, 13, 14, 15, 18,
      26, 29, 32, 34, 35, 36, 38, 41, 44,
      48, 51, 54, 55, 56, 57, 60, 63, 66,
      67, 68, 75, 78, 79, 80, 81, 84, 87,
      89, 92, 95, 97, 98, 99, 101, 104, 108,
      111, 114, 115, 116, 117, 120, 123, 126,
      124, 125, 129, 132, 135, 136, 137, 138,
      141, 143, 146, 149, 151, 152, 153, 155,
      159, 162, 165, 166, 167, 168, 171, 172,
      173, 174, 177, 180, 183, 184, 185, 186,
      188, 191, 194, 196, 197, 198, 201, 204,
      207, 208, 209, 210, 211, 212, 213, 216,
      219, 222, 224, 227, 230, 234, 237, 240,
      241, 242, 243, 246, 249, 251, 254, 256,
      258, 261, 262, 263, 264, 267, 269, 274, 273, 275, 276
    )],
    type = "response", adjust = "tukey"
    )
    emm.post.hoc.full <- rbind(emm.post.hoc.full, data.frame(
      term = emm.pairs.full$contrast,
      odds.ratio = emm.pairs.full$odds.ratio,
      se = emm.pairs.full$SE,
      p.value = emm.pairs.full$p.value
    ))

    # boot full model
    boot.full <- boot.glmm.pred(
      model.res = full, excl.warnings = T, nboots = 10,
      para = F, resol = 100, level = 0.95, use = c("condition", "age.group.sum", "culture")
    )

    boot.full$ci.predicted$name <- paste(boot.full$ci.predicted$condition,
      boot.full$ci.predicted$age.group.sum,
      boot.full$ci.predicted$culture,
      sep = "."
    )
    boot.full.values <-
      rbind(boot.full.values, data.frame(
        term = boot.full$ci.predicted$name,
        fitted = boot.full$ci.predicted$fitted,
        lower.cl = boot.full$ci.predicted$lower.cl,
        upper.cl = boot.full$ci.predicted$upper.cl
      ))
    boot.full.estimates <-
      rbind(boot.full.estimates, data.frame(
        term = rownames(boot.full$ci.estimates),
        orig = boot.full$ci.estimates$orig,
        X2.5. = boot.full$ci.estimates$X2.5.,
        X97.5. = boot.full$ci.estimates$X97.5.
      ))
  } else {
    all.res$full.warnings[i] <- "yes"
  }

  # Null model
  null <- keepWarnings(glmer(new.resp ~ 1 +
    (1 + (condition.process.only.c + condition.outcome.process.c) || sub.id),
  data = t.data, family = binomial(link = "logit"), control = contr
  ))

  if (length(full1$warnings) == 0 & length(null$warnings) == 0) {
    null <- null$value
    all.res$null.warnings[i] <- "no"

    # full null model comparisons
    test.full.null <- as.data.frame(anova(null, full, test = "Chisq"))["full", c("Chisq", "Df", "Pr(>Chisq)")]
    all.res$test.full.null.Chisq[i] <- test.full.null$Chisq
    all.res$test.full.null.Df[i] <- test.full.null$Df
    all.res$test.full.null.Pr..Chisq[i] <- test.full.null$`Pr(>Chisq)`
  } else {
    all.res$null.warnings[i] <- "yes"
  }

  # Red model
  red <- keepWarnings(glmer(new.resp ~ (condition + age.group.sum + culture)^2 +
    (1 + (condition.process.only.c + condition.outcome.process.c) || sub.id),
  data = t.data, family = binomial(link = "logit"), control = contr
  ))
  if (length(red$warnings) == 0) {
    red <- red$value
    all.res$red.warnings[i] <- "no"
    all.res$all.red.coeffs.intercept[i] <- summary(red)$coefficients["(Intercept)", 1]
    all.res$all.red.coeffs.conditionoutcome.vs.process[i] <- summary(red)$coefficients["conditionoutcome.vs.process", 1]
    all.res$all.red.coeffs.conditionprocess.only[i] <- summary(red)$coefficients["conditionprocess.only", 1]
    all.res$all.red.coeffs.age.group.sum6.7[i] <- summary(red)$coefficients["age.group.sum6.7", 1]
    all.res$all.red.coeffs.age.group.sum8.9[i] <- summary(red)$coefficients["age.group.sum8.9", 1]
    all.res$all.red.coeffs.age.group.sumadult[i] <- summary(red)$coefficients["age.group.sumadult", 1]
    all.res$all.red.coeffs.cultureusa[i] <- summary(red)$coefficients["cultureusa", 1]
    all.res$all.red.coeffs.conditionoutcome.vs.process.age.group.sum6.7[i] <- summary(red)$coefficients["conditionoutcome.vs.process:age.group.sum6.7", 1]
    all.res$all.red.coeffs.conditionprocess.only.age.group.sum6.7[i] <- summary(red)$coefficients["conditionprocess.only:age.group.sum6.7", 1]
    all.res$all.red.coeffs.conditionoutcome.vs.process.age.group.sum8.9[i] <- summary(red)$coefficients["conditionoutcome.vs.process:age.group.sum8.9", 1]
    all.res$all.red.coeffs.conditionprocess.only.age.group.sum8.9[i] <- summary(red)$coefficients["conditionprocess.only:age.group.sum8.9", 1]
    all.res$all.red.coeffs.conditionoutcome.vs.process.age.group.sumadult[i] <- summary(red)$coefficients["conditionoutcome.vs.process:age.group.sumadult", 1]
    all.res$all.red.coeffs.conditionprocess.only.age.group.sumadult[i] <- summary(red)$coefficients["conditionprocess.only:age.group.sumadult", 1]
    all.res$all.red.coeffs.conditionoutcome.vs.process.cultureusa[i] <- summary(red)$coefficients["conditionoutcome.vs.process:cultureusa", 1]
    all.res$all.red.coeffs.conditionprocess.only.cultureusa[i] <- summary(red)$coefficients["conditionprocess.only:cultureusa", 1]
    all.res$all.red.coeffs.age.group.sum6.7.cultureusa[i] <- summary(red)$coefficients["age.group.sum6.7:cultureusa", 1]
    all.res$all.red.coeffs.age.group.sum8.9.cultureusa[i] <- summary(red)$coefficients["age.group.sum8.9:cultureusa", 1]
    all.res$all.red.coeffs.age.group.sumadult.cultureusa[i] <- summary(red)$coefficients["age.group.sumadult:cultureusa", 1]

    # red-main model comparisons
    tests.red.main <- drop1p(model.res = red, para = F, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
    test.2.way.condition.age.group.sum <-
      as.data.frame(tests.red.main$drop1.res[2, c("Chisq", "Chi.Df", "Pr..Chisq.", "n.opt.warnings", "n.fun.warnings")])
    all.res$test.2.way.condition.age.group.sum.Chisq[i] <- test.2.way.condition.age.group.sum$Chisq
    all.res$test.2.way.condition.age.group.sum.Chi.Df[i] <- test.2.way.condition.age.group.sum$Chi.Df
    all.res$test.2.way.condition.age.group.sum.Pr..Chisq[i] <- test.2.way.condition.age.group.sum$Pr..Chisq.
    all.res$test.2.way.condition.age.group.sum.n.opt.warnings[i] <- test.2.way.condition.age.group.sum$n.opt.warnings
    all.res$test.2.way.condition.age.group.sum.n.fun.warnings[i] <- test.2.way.condition.age.group.sum$n.fun.warnings

    test.2.way.condition.culture <-
      as.data.frame(tests.red.main$drop1.res[3, c("Chisq", "Chi.Df", "Pr..Chisq.", "n.opt.warnings", "n.fun.warnings")])
    all.res$test.2.way.condition.culture.Chisq[i] <- test.2.way.condition.culture$Chisq
    all.res$test.2.way.condition.culture.Chi.Df[i] <- test.2.way.condition.culture$Chi.Df
    all.res$test.2.way.condition.culture.Pr..Chisq[i] <- test.2.way.condition.culture$Pr..Chisq.
    all.res$test.2.way.condition.culture.n.opt.warnings[i] <- test.2.way.condition.culture$n.opt.warnings
    all.res$test.2.way.condition.culture.n.fun.warnings[i] <- test.2.way.condition.culture$n.fun.warnings

    test.2.way.age.group.sum.culture <-
      as.data.frame(tests.red.main$drop1.res[4, c("Chisq", "Chi.Df", "Pr..Chisq.", "n.opt.warnings", "n.fun.warnings")])
    all.res$test.2.way.age.group.sum.culture.Chisq[i] <- test.2.way.age.group.sum.culture$Chisq
    all.res$test.2.way.age.group.sum.culture.Chi.Df[i] <- test.2.way.age.group.sum.culture$Chi.Df
    all.res$test.2.way.age.group.sum.culture.Pr..Chisq[i] <- test.2.way.age.group.sum.culture$Pr..Chisq.
    all.res$test.2.way.age.group.sum.culture.n.opt.warnings[i] <- test.2.way.age.group.sum.culture$n.opt.warnings
    all.res$test.2.way.age.group.sum.culture.n.fun.warnings[i] <- test.2.way.age.group.sum.culture$n.fun.warnings

    # post hoc pairwise comparisons model with only one two-way interactions
    emm <- emmeans(red, ~ condition * age.group.sum)
    emm.pairs.red <- summary(contrast(emm, "pairwise")[c(
      1, 2, 3, 6, 9, 12, 14, 17, 20, 24, 27,
      30, 31, 32, 33, 36, 39, 41, 44, 48, 51,
      52, 53, 54, 57, 59, 63, 64, 65, 66
    )],
    type = "response", adjust = "tukey"
    )
    emm.post.hoc.red <- rbind(emm.post.hoc.red, data.frame(
      term = emm.pairs.red$contrast,
      odds.ratio = emm.pairs.red$odds.ratio,
      se = emm.pairs.red$SE,
      p.value = emm.pairs.red$p.value
    ))

    # boot condition*age group
    plot.model.1 <- keepWarnings(glmer(new.resp ~ condition * age.group.sum + culture.code +
      (1 + (condition.process.only.c + condition.outcome.process.c) || sub.id),
    data = t.data, family = binomial(link = "logit"), control = contr
    ))
    if (length(plot.model.1$warnings) == 0) {
      boot.plot.1 <- boot.glmm.pred(
        model.res = plot.model.1$value, excl.warnings = T, nboots = 10,
        para = F, resol = 100, level = 0.95, use = c("condition", "age.group.sum")
      )

      boot.plot.1$ci.predicted$name <- paste(boot.plot.1$ci.predicted$condition,
        boot.plot.1$ci.predicted$age.group.sum,
        sep = "."
      )
      boot.plot.1.values <-
        rbind(boot.plot.1.values, data.frame(
          term = boot.plot.1$ci.predicted$name,
          fitted = boot.plot.1$ci.predicted$fitted,
          lower.cl = boot.plot.1$ci.predicted$lower.cl,
          upper.cl = boot.plot.1$ci.predicted$upper.cl
        ))
      boot.plot.1.estimates <-
        rbind(boot.plot.1.estimates, data.frame(
          term = rownames(boot.plot.1$ci.estimates),
          orig = boot.plot.1$ci.estimates$orig,
          X2.5. = boot.plot.1$ci.estimates$X2.5.,
          X97.5. = boot.plot.1$ci.estimates$X97.5.
        ))
    }

    # boot condition*culture
    plot.model.2 <- keepWarnings(glmer(new.resp ~ condition * culture + (age.group.sum.6.7.code + age.group.sum.8.9.code + age.group.sum.adult.code) +
      (1 + (condition.process.only.c + condition.outcome.process.c) || sub.id),
    data = t.data, family = binomial(link = "logit"), control = contr
    ))
    if (length(plot.model.2$warnings) == 0) {
      boot.plot.2 <- boot.glmm.pred(
        model.res = plot.model.2$value, excl.warnings = T, nboots = 10,
        para = F, level = 0.95, use = c("condition", "culture")
      )

      boot.plot.2$ci.predicted$name <- paste(boot.plot.2$ci.predicted$condition,
        boot.plot.2$ci.predicted$age.group.sum,
        sep = "."
      )
      boot.plot.2.values <-
        rbind(boot.plot.2.values, data.frame(
          term = boot.plot.2$ci.predicted$name,
          fitted = boot.plot.2$ci.predicted$fitted,
          lower.cl = boot.plot.2$ci.predicted$lower.cl,
          upper.cl = boot.plot.2$ci.predicted$upper.cl
        ))
      boot.plot.2.estimates <-
        rbind(boot.plot.2.estimates, data.frame(
          term = rownames(boot.plot.2$ci.estimates),
          orig = boot.plot.2$ci.estimates$orig,
          X2.5. = boot.plot.2$ci.estimates$X2.5.,
          X97.5. = boot.plot.2$ci.estimates$X97.5.
        ))
    }
  } else {
    all.res$red.warnings[i] <- "yes"
  }

  # Main Effects model
  main <- keepWarnings(glmer(new.resp ~ (condition + age.group.sum + culture) +
    (1 + (condition.process.only.c + condition.outcome.process.c) || sub.id),
  data = t.data, family = binomial(link = "logit"), control = contr
  ))
  # check colliniarity
  all.res$colliniarity.test.condition[i] <- vif(main$value)[1, 3]
  all.res$colliniarity.test.age.group.sum[i] <- vif(main$value)[2, 3]
  all.res$colliniarity.test.culture[i] <- vif(main$value)[3, 3]

  if (length(main$warnings) == 0) {
    main <- main$value
    all.res$main.warnings[i] <- "no"
    # main coefficients
    all.res$all.main.coeffs.intercept[i] <- summary(main)$coefficients["(Intercept)", 1]
    all.res$all.main.coeffs.conditionoutcome.vs.process[i] <- summary(main)$coefficients["conditionoutcome.vs.process", 1]
    all.res$all.main.coeffs.conditionprocess.only[i] <- summary(main)$coefficients["conditionprocess.only", 1]
    all.res$all.main.coeffs.age.group.sum6.7[i] <- summary(main)$coefficients["age.group.sum6.7", 1]
    all.res$all.main.coeffs.age.group.sum8.9[i] <- summary(main)$coefficients["age.group.sum8.9", 1]
    all.res$all.main.coeffs.age.group.sumadult[i] <- summary(main)$coefficients["age.group.sumadult", 1]
    all.res$all.main.coeffs.cultureusa[i] <- summary(main)$coefficients["cultureusa", 1]

    # main effects
    tests.main <- drop1p(model.res = main, para = F, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
    test.main.condition <- as.data.frame(tests.main$drop1.res[2, c("Chisq", "Chi.Df", "Pr..Chisq.", "n.opt.warnings", "n.fun.warnings")])
    all.res$test.main.condition.Chisq[i] <- test.main.condition$Chisq
    all.res$test.main.condition.Chi.Df[i] <- test.main.condition$Chi.Df
    all.res$test.main.condition.Pr..Chisq[i] <- test.main.condition$Pr..Chisq.
    all.res$test.main.condition.n.opt.warnings[i] <- test.main.condition$n.opt.warnings
    all.res$test.main.condition.n.fun.warnings[i] <- test.main.condition$n.fun.warnings
    #
    test.main.age.group.sum <- as.data.frame(tests.main$drop1.res[3, c("Chisq", "Chi.Df", "Pr..Chisq.", "n.opt.warnings", "n.fun.warnings")])
    all.res$test.main.age.group.sum.Chisq[i] <- test.main.age.group.sum$Chisq
    all.res$test.main.age.group.sum.Chi.Df[i] <- test.main.age.group.sum$Chi.Df
    all.res$test.main.age.group.sum.Pr..Chisq[i] <- test.main.age.group.sum$Pr..Chisq.
    all.res$test.main.age.group.sum.n.opt.warnings[i] <- test.main.age.group.sum$n.opt.warnings
    all.res$test.main.age.group.sum.n.fun.warnings[i] <- test.main.age.group.sum$n.fun.warnings
    #
    test.main.culture <- as.data.frame(tests.main$drop1.res[4, c("Chisq", "Chi.Df", "Pr..Chisq.", "n.opt.warnings", "n.fun.warnings")])
    all.res$test.main.culture.Chisq[i] <- test.main.culture$Chisq
    all.res$test.main.culture.Chi.Df[i] <- test.main.culture$Chi.Df
    all.res$test.main.culture.Pr..Chisq[i] <- test.main.culture$Pr..Chisq.
    all.res$test.main.culture.n.opt.warnings[i] <- test.main.culture$n.opt.warnings
    all.res$test.main.culture.n.fun.warnings[i] <- test.main.culture$n.fun.warnings
  } else {
    all.res$main.warnings[i] <- "yes"
  }

  print(i)
}

# save.image("./supplementary.analysis.help.seeking.1000.RData")
load("./supplementary.analysis.help.seeking.1000.RData")

## Evaluation of the results -----------------------------------------------
str(all.res)
# how many models did converge
# full
sum(all.res$full.warnings == "no") # 948 of the models converged
# null
sum(all.res$null.warnings == "no") # 948 of the models converged
# red
sum(all.res$red.warnings == "no") # 21 of the models converged
# main
sum(all.res$main.warnings == "no") # none of the models converged

# Means of full-null-comparisons
# Chisq
round(mean(all.res$test.full.null.Chisq, na.rm = T), 10)
round(range(all.res$test.full.null.Chisq, na.rm = T), 10)
# DF
range(all.res$test.full.null.Df, na.rm = T)
# p-value
round(mean(all.res$test.full.null.Pr..Chisq, na.rm = T), 10)
round(range(all.res$test.full.null.Pr..Chisq, na.rm = T), 10)
1 - (sum(all.res$test.full.null.Pr..Chisq <= 0.051, na.rm = T) / sum(all.res$null.warnings == "no")) # percent of non-signficant models

# Assumption tests
# overdispersion parameter
round(mean(all.res$overdispersion.test.full, na.rm = T), 10)
round(range(all.res$overdispersion.test.full, na.rm = T), 10)
# colliniarity
round(mean(all.res$colliniarity.test.condition, na.rm = T), 10)
round(range(all.res$colliniarity.test.condition, na.rm = T), 10)
round(mean(all.res$colliniarity.test.age.group.sum, na.rm = T), 10)
round(range(all.res$colliniarity.test.age.group.sum, na.rm = T), 10)
round(mean(all.res$colliniarity.test.culture, na.rm = T), 10)
round(range(all.res$colliniarity.test.culture, na.rm = T), 10)

# means of three-way interaction
sum(all.res$test.3.way.int.n.opt.warnings == 0 &
  all.res$test.3.way.int.n.fun.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res$test.3.way.int.Chisq, na.rm = T), 10)
round(range(all.res$test.3.way.int.Chisq, na.rm = T), 10)
# DF
range(all.res$test.3.way.int.Chi.Df, na.rm = T)
# p-value
round(mean(all.res$test.3.way.int.Pr..Chisq, na.rm = T), 10)
1 - (sum(all.res$test.3.way.int.Pr..Chisq <= 0.051, na.rm = T) / sum(all.res$full.warnings == "no")) # percent of non-signficant models
round(range(all.res$test.3.way.int.Pr..Chisq, na.rm = T), 10)

# Means of two-way interactions
# condition * age group
sum(all.res$test.2.way.condition.age.group.sum.n.opt.warnings == 0 &
  all.res$test.2.way.condition.age.group.sum.n.fun.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res$test.2.way.condition.age.group.sum.Chisq, na.rm = T), 10)
round(range(all.res$test.2.way.condition.age.group.sum.Chisq, na.rm = T), 10)
# DF
range(all.res$test.2.way.condition.age.group.sum.Chi.Df, na.rm = T)
# p-value
round(mean(all.res$test.2.way.condition.age.group.sum.Pr..Chisq, na.rm = T), 10)
1 - (sum(all.res$test.2.way.condition.age.group.sum.Pr..Chisq <= 0.051, na.rm = T) / sum(all.res$red.warnings == "no")) # percent of non-signficant models
round(range(all.res$test.2.way.condition.age.group.sum.Pr..Chisq, na.rm = T), 10)

# condition * culture
sum(all.res$test.2.way.condition.culture.n.opt.warnings == 0 &
  all.res$test.2.way.condition.culture.n.fun.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res$test.2.way.condition.culture.Chisq, na.rm = T), 10)
round(range(all.res$test.2.way.condition.culture.Chisq, na.rm = T), 10)
# DF
range(all.res$test.2.way.condition.culture.Chi.Df, na.rm = T)
# p-value
round(mean(all.res$test.2.way.condition.culture.Pr..Chisq, na.rm = T), 10)
1 - (sum(all.res$test.2.way.condition.culture.Pr..Chisq <= 0.051, na.rm = T) / sum(all.res$red.warnings == "no")) # percent of non-significant models
1 - (sum(all.res$test.2.way.condition.culture.Pr..Chisq <= 0.1, na.rm = T) / sum(all.res$red.warnings == "no")) # percent of non-significant models
round(range(all.res$test.2.way.condition.culture.Pr..Chisq, na.rm = T), 10)

# age group * culture
sum(all.res$test.2.way.age.group.sum.culture.n.opt.warnings == 0 &
  all.res$test.2.way.age.group.sum.culture.n.fun.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res$test.2.way.age.group.sum.culture.Chisq, na.rm = T), 10)
round(range(all.res$test.2.way.age.group.sum.culture.Chisq, na.rm = T), 10)
# DF
range(all.res$test.2.way.age.group.sum.culture.Chi.Df, na.rm = T)
# p-value
round(mean(all.res$test.2.way.age.group.sum.culture.Pr..Chisq, na.rm = T), 10)
1 - (sum(all.res$test.2.way.age.group.sum.culture.Pr..Chisq <= 0.051, na.rm = T) / sum(all.res$red.warnings == "no")) # percent of non-significant models
1 - (sum(all.res$test.2.way.age.group.sum.culture.Pr..Chisq <= 0.01, na.rm = T) / sum(all.res$red.warnings == "no")) # percent of non-significant models
round(range(all.res$test.2.way.age.group.sum.culture.Pr..Chisq, na.rm = T), 10)

# main effect age group
sum(all.res$test.main.age.group.sum.n.opt.warnings == 0 &
  all.res$test.main.age.group.sum.n.fun.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res$test.main.age.group.sum.Chisq, na.rm = T), 10)
round(range(all.res$test.main.age.group.sum.Chisq, na.rm = T), 10)
# DF
range(all.res$test.main.age.group.sum.Chi.Df, na.rm = T)
# p-value
round(mean(all.res$test.main.age.group.sum.Pr..Chisq, na.rm = T), 10)
1 - (sum(all.res$test.main.age.group.sum.Pr..Chisq <= 0.051, na.rm = T) / sum(all.res$main.warnings == "no")) # percent of non-significant models
1 - (sum(all.res$test.main.age.group.sum.Pr..Chisq <= 0.01, na.rm = T) / sum(all.res$main.warnings == "no")) # percent of non-significant models
round(range(all.res$test.main.age.group.sum.Pr..Chisq, na.rm = T), 10)

# main effect condition
sum(all.res$test.main.condition.n.opt.warnings == 0 &
  all.res$test.main.condition.n.fun.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res$test.main.condition.Chisq, na.rm = T), 10)
round(range(all.res$test.main.condition.Chisq, na.rm = T), 10)
# DF
range(all.res$test.main.condition.Chi.Df, na.rm = T)
# p-value
round(mean(all.res$test.main.condition.Pr..Chisq, na.rm = T), 10)
1 - (sum(all.res$test.main.condition.Pr..Chisq <= 0.051, na.rm = T) / sum(all.res$main.warnings == "no")) # percent of non-significant models
1 - (sum(all.res$test.main.condition.Pr..Chisq <= 0.01, na.rm = T) / sum(all.res$main.warnings == "no")) # percent of non-significant models
round(range(all.res$test.main.condition.Pr..Chisq, na.rm = T), 10)

# main effect culture
sum(all.res$test.main.culture.n.opt.warnings == 0 &
  all.res$test.main.culture.n.fun.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res$test.main.culture.Chisq, na.rm = T), 10)
round(range(all.res$test.main.culture.Chisq, na.rm = T), 10)
# DF
range(all.res$test.main.culture.Chi.Df, na.rm = T)
# p-value
round(mean(all.res$test.main.culture.Pr..Chisq, na.rm = T), 10)
1 - (sum(all.res$test.main.culture.Pr..Chisq <= 0.051, na.rm = T) / sum(all.res$main.warnings == "no")) # percent of non-significant models
1 - (sum(all.res$test.main.culture.Pr..Chisq <= 0.01, na.rm = T) / sum(all.res$main.warnings == "no")) # percent of non-significant models
round(range(all.res$test.main.culture.Pr..Chisq, na.rm = T), 10)

# coefs of the full  model
all.coefs <-
  all.res %>%
  select(vars_select(names(all.res), starts_with("all.full", ignore.case = TRUE)))

round(colMeans(all.coefs, na.rm = T), 3)
data.frame(min = sapply(all.coefs, min, na.rm = T), max = sapply(all.coefs, max, na.rm = T))

# post-hoc pairwise comparison of the full model
xx <- mapply(FUN = tapply, X = as.data.frame(emm.post.hoc.full$p.value), MoreArgs = list(INDEX = emm.post.hoc.full$term, FUN = mean))
round(xx, 3)

emm.post.hoc.full.cis$term <- paste(emm.post.hoc.full.cis$condition, emm.post.hoc.full.cis$age.group.sum, emm.post.hoc.full.cis$culture, sep = ".")
xx <- mapply(FUN = tapply, X = as.data.frame(emm.post.hoc.full.cis[, c(4:8)]), MoreArgs = list(INDEX = emm.post.hoc.full.cis$term, FUN = mean))
round(xx, 3)

# boot.full fitted values and confidence intervals
boot.full <- mapply(FUN = tapply, X = boot.full.values[, -1], MoreArgs = list(INDEX = boot.full.values$term, FUN = mean)) # fitted
xx <- round(boot.full, 3)
boot.full <- mapply(FUN = tapply, X = boot.full.values[, -1], MoreArgs = list(INDEX = boot.full.values$term, FUN = min)) # lower ci
yy <- round(boot.full, 3)
boot.full <- mapply(FUN = tapply, X = boot.full.values[, -1], MoreArgs = list(INDEX = boot.full.values$term, FUN = max)) # upper ci
zz <- round(boot.full, 3)

cbind(xx[, 1], yy[, 2], zz[, 3])

# boot.plot.1 values
boot.plot.1 <- mapply(FUN = tapply, X = boot.plot.1.values[, -1], MoreArgs = list(INDEX = boot.plot.1.values$term, FUN = mean)) # median
round(boot.plot.1, 3)

# boot.plot.2 values
boot.plot.2 <- mapply(FUN = tapply, X = boot.plot.2.values[, -1], MoreArgs = list(INDEX = boot.plot.2.values$term, FUN = mean)) # median
round(boot.plot.2, 3)

# boot.full estimates
xx <- mapply(FUN = tapply, X = boot.full.estimates[, -1], MoreArgs = list(INDEX = boot.full.estimates$term, FUN = mean)) # median
xx <- round(xx, 3)
yy <- mapply(FUN = tapply, X = boot.full.estimates[, -1], MoreArgs = list(INDEX = boot.full.estimates$term, FUN = min)) # lower ci
yy <- round(yy, 3)
zz <- mapply(FUN = tapply, X = boot.full.estimates[, -1], MoreArgs = list(INDEX = boot.full.estimates$term, FUN = max)) # upper ci
zz <- round(zz, 3)
help.seeking.boot.full <- cbind(xx[, 1], yy[, 2], zz[, 3])

write.table(help.seeking.boot.full, "analysis.help.seeking.boot.full.txt", sep = "\t", row.names = TRUE, na = "NA", dec = ".")

# boot.plot.1 estimates
xx <- mapply(FUN = tapply, X = boot.plot.1.estimates[, -1], MoreArgs = list(INDEX = boot.plot.1.estimates$term, FUN = mean)) # median
round(xx, 3)

xx <- mapply(FUN = tapply, X = boot.plot.2.estimates[, -1], MoreArgs = list(INDEX = boot.plot.2.estimates$term, FUN = mean)) # median
round(xx, 3)

# plotting p-value distributions
# setting up the plotting window
l.mat <- matrix(1:6,
  ncol = 2, byrow = T
)
l.mat

layout(
  mat = l.mat, widths = c(rep(4, 2)),
  heights = c(rep(4, 3))
)
layout.show(max(l.mat))

par(mar = c(4, 4, 3, 3), mgp = c(2.3, 0.5, 0))
hist(all.res$test.full.null.Pr..Chisq,
  breaks = seq(from = 0, to = 1, by = 0.05), main = "",
  ylab = "frequency",
  xlab = "p values for full-null comparison", border = "black", col = "#FF6666", las = 1
)

plot(
  x = 0, y = 0, xlim = c(0, 0.7), ylim = c(0, 4), type = "n", las = 1, yaxt = "n", xaxt = "n",
  ylab = "", xlab = "", bty = "n"
)

par(mar = c(4, 4, 3, 3), mgp = c(2.3, 0.5, 0))
hist(all.res$test.3.way.int.Pr..Chisq,
  breaks = seq(from = 0, to = 1, by = 0.05), main = "",
  ylab = "frequency",
  xlab = "p values for three-way interaction condition*age group*culture", border = "black", col = "#FF6666", las = 1
)

par(mar = c(4, 4, 3, 3), mgp = c(2.3, 0.5, 0))
hist(all.res$test.2.way.condition.culture.Pr..Chisq,
  breaks = seq(from = 0, to = 1, by = 0.05), main = "",
  ylab = "frequency",
  xlab = "p values for two-way interaction condition*culture", border = "black", col = "#FF6666", las = 1
)

par(mar = c(4, 4, 3, 3), mgp = c(2.3, 0.5, 0))
hist(all.res$test.2.way.condition.age.group.sum.Pr..Chisq,
  breaks = seq(from = 0, to = 1, by = 0.05), main = "",
  ylab = "frequency",
  xlab = "p values for two-way interaction condition*age group", border = "black", col = "#FF6666", las = 1
)

par(mar = c(4, 4, 3, 3), mgp = c(2.3, 0.5, 0))
hist(all.res$test.2.way.age.group.sum.culture.Pr..Chisq,
  breaks = seq(from = 0, to = 1, by = 0.05), main = "",
  ylab = "frequency",
  xlab = "p values for two-way interaction age group*culture", border = "black", col = "#FF6666", las = 1
)
