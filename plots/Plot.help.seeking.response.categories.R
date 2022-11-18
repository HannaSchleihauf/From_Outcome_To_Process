# Project: PRR
# Analysis: Plot for Help-Seeking Categories
# Author: Hanna Schleihauf
# Date: December 1 2021

library("ggplot2")
library("ggpubr")
library("tidyverse")

xdata <- read.csv("./PRR_data.csv", header = TRUE)

# subsetting dataset
xdata.china <- subset(xdata, xdata$culture == "china")
xdata.usa <- subset(xdata, xdata$culture == "usa")

levels(as.factor(xdata$help.seeking.why.cat))
ftable(help.seeking.why.cat ~ condition + age.group.sum, xdata)

####### USA only
# Plotting ----------------------------------------------------------------
dev.off()
xx <- ftable(help.seeking.why.cat ~ condition + age.group.sum, xdata.usa)

# frequency table as I need it, with changed signs
yy <- xx[, 1] + xx[, 2] + xx[, 3] + xx[, 4]
xx <- xx / yy

freq.usa <- as.data.frame(xx)

#dev.off()
p <- freq.usa %>%
  ggplot(aes(x = age.group.sum, y = Freq, fill = factor(help.seeking.why.cat))) +
  geom_bar(stat = "identity", width = .65) +
  labs(title = "USA")
p <- p + facet_wrap(~condition,
  labeller = labeller(condition = c(
    "outcome.only" = "outcome condition",
    "outcome.vs.process" = "process vs.\noutcome condition",
    "process.only" = "process condition"
  ))
)

p <- p + theme(
  plot.title = element_text(size = 8, face = "bold"),
  axis.title.x = element_blank(),
  legend.title = element_blank(),
  axis.title.y = element_blank(),
  legend.key = element_rect(size = 1),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1, "line")
)

p <- p + xlab("") +
  scale_x_discrete(labels = c("4-5", "6-7", "8-9", "adults")) +
  scale_y_continuous(labels = scales::percent)

p <- p + scale_fill_manual(
  labels =
    c("i don't\nknow", "other", "referring to\noutcome", "referring to\nprocess"),
  values = c("#2E2E2E", "#7F7F7F", "#FCC00A", "#00C5CD")
)

p1 <- p

####### china only
# Plotting ----------------------------------------------------------------
#dev.off()
xx <- ftable(help.seeking.why.cat ~ condition + age.group.sum, xdata.china)

## frequency table as I need it, with changed signs
yy <- xx[, 1] + xx[, 2] + xx[, 3] + xx[, 4]
xx <- xx / yy

freq.china <- as.data.frame(xx)

#dev.off()
p <- freq.china %>%
  ggplot(aes(x = age.group.sum, y = Freq, fill = factor(help.seeking.why.cat))) +
  geom_bar(stat = "identity", width = .65) +
  labs(title = "China")
p <- p + facet_wrap(~condition,
  labeller = labeller(condition = c(
    "outcome.only" = "outcome condition",
    "outcome.vs.process" = "process vs.\noutcome condition",
    "process.only" = "process condition"
  ))
)

p <- p + theme(
  plot.title = element_text(size = 8, face = "bold"),
  axis.title.x = element_blank(),
  legend.title = element_blank(),
  axis.title.y = element_blank(),
  legend.key = element_rect(size = 1),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1, "line")
)

p <- p + xlab("") +
  scale_x_discrete(labels = c("4-5", "6-7", "8-9", "adults")) +
  scale_y_continuous(labels = scales::percent)


p <- p + scale_fill_manual(
  labels =
    c("i don't\nknow", "other", "referring to\noutcome", "referring to\nprocess"),
  values = c("#2E2E2E", "#7F7F7F", "#FCC00A", "#00C5CD")
)

p2 <- p

figure <- ggarrange(p2, p1,
  ncol = 1, nrow = 2
)
figure
