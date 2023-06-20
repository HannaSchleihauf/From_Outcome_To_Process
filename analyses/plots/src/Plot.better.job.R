# Project: PRR
# Analysis: Plot for Better-Job Variable
# Author: Hanna Schleihauf
# Date: December 1 2021

library("ggplot2")
library("ggpubr")
library("tidyverse")

xdata <- read.csv("./PRR_data.csv", header = TRUE)

xdata.china <- subset(xdata, xdata$culture == "china")
xdata.usa <- subset(xdata, xdata$culture == "usa")

####### USA only
# Plotting ----------------------------------------------------------------
xx <- ftable(better.job ~ condition + age.group.sum, xdata.usa)
ftable(age.group.sum ~ culture, xdata) / 6
table(xdata.usa$age.group, xdata.usa$gender) / 6

# frequency table as I need it, with changed signs
yy <- xx[, 1] + xx[, 2]
xx <- xx / yy

freq <- as.data.frame(xx)
freq$new <- NA
freq$new[freq$better.job == 0] <- freq$Freq[freq$better.job == 0] * (-1)
freq$new[freq$better.job == 1] <- freq$Freq[freq$better.job == 1]

freq.wide <- freq %>%
  group_by(better.job) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = better.job, values_from = new) %>%
  select(-row)

freq.wide <- freq.wide %>%
  select(condition, age.group.sum, "0", "1")
freq.wide[1:12, 4] <- freq.wide[13:24, 4]
freq.wide <- freq.wide[-(13:24), ]

freq$better.job <- relevel(freq$better.job, ref = "1")

freq$condition <- factor(freq$condition, levels = c("outcome.only", "process.only", "outcome.vs.process"))

p <- freq %>%
  ggplot(aes(x = age.group.sum, y = new, fill = factor(better.job))) +
  geom_bar(stat = "identity", width = .65) +
  labs(title = "USA")
p <- p + facet_wrap(~condition,
  labeller = labeller(condition = c(
    "outcome.only" = "outcome condition",
    "outcome.vs.process" = "process-vs-\noutcome condition",
    "process.only" = "process condition"
  ))
)
p <- p + theme(
  plot.title = element_text(size = 8, face = "bold"),
  axis.title.x = element_blank(),
  legend.title = element_blank(),
  axis.title.y = element_blank(),
  legend.key = element_rect(size = 6),
  legend.key.size = unit(1, "cm")
)
p <- p + xlab("") +
  scale_x_discrete(labels = c("4-5", "6-7", "8-9", "adults")) +
  scale_y_continuous(labels = scales::percent) # , sec.axis = dup_axis())
p <- p + scale_fill_manual(
  labels =
    c("participants\nwho evaluate\nthe process", "participants\nwho evaluate\nthe outcome"),
  values = c("#00C5CD", "#FCC00A")
)
p <- p + guides(fill = guide_legend(
  keywidth = 0.2,
  keyheight = 0.5,
  default.unit = "inch"
))

p1 <- p

####### China only
# Plotting ----------------------------------------------------------------
xx <- ftable(better.job ~ condition + age.group.sum, xdata.china)
ftable(age.group.sum ~ culture, xdata) / 6
table(xdata.china$age.group, xdata.china$gender) / 6

# frequency table as I need it, with changed signs
yy <- xx[, 1] + xx[, 2]
xx <- xx / yy

freq <- as.data.frame(xx)
freq$new <- NA
freq$new[freq$better.job == 0] <- freq$Freq[freq$better.job == 0] * (-1)
freq$new[freq$better.job == 1] <- freq$Freq[freq$better.job == 1]

freq.wide <- freq %>%
  group_by(better.job) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = better.job, values_from = new) %>%
  select(-row)

freq.wide <- freq.wide %>%
  select(condition, age.group.sum, "0", "1")
freq.wide[1:12, 4] <- freq.wide[13:24, 4]
freq.wide <- freq.wide[-(13:24), ]

freq$better.job <- relevel(freq$better.job, ref = "1")

freq$condition <- factor(freq$condition, levels = c("outcome.only", "process.only", "outcome.vs.process"))

p <- freq %>%
  ggplot(aes(x = age.group.sum, y = new, fill = factor(better.job))) +
  geom_bar(stat = "identity", width = .65) +
  labs(title = "China")
p <- p + facet_wrap(~condition,
  labeller = labeller(condition = c(
    "outcome.only" = "outcome condition",
    "outcome.vs.process" = "process-vs-\noutcome condition",
    "process.only" = "process condition"
  ))
)
p <- p + theme(
  plot.title = element_text(size = 8, face = "bold"),
  axis.title.x = element_blank(),
  legend.title = element_blank(),
  axis.title.y = element_blank(),
  legend.key = element_rect(size = 1),
  legend.key.size = unit(1, "cm")
) 
p <- p + xlab("") +
  scale_x_discrete(labels = c("4-5", "6-7", "8-9", "adults")) +
  scale_y_continuous(labels = scales::percent) # , sec.axis = dup_axis())
p <- p + scale_fill_manual(
  labels =
    c("participants\nwho evaluate\nthe process", "participants\nwho evaluate\nthe outcome"),
  values = c("#00C5CD", "#FCC00A")
)
p <- p + guides(fill = guide_legend(
  keywidth = 0.2,
  keyheight = 0.5,
  default.unit = "inch"
))
p2 <- p

figure <- ggarrange(p2, p1,
  ncol = 1, nrow = 2
)
figure

# annotate_figure(figure, fig.lab = "Rationality Judgement", fig.lab.face = "bold")


# Plot with estimates -----------------------------------------------------
load("analysis.better.job.outcome.vs.process.RData")
xx <- as.data.frame(boot.res$ci.predicted)

p <- ggplot(xx, 
               aes(x = age.group.sum, 
               y = fitted,  color = culture)) + 
 geom_pointrange(aes(ymin = lower.cl, ymax = upper.cl), 
               position=position_dodge(width=c(0.6,0.4)), size = 1) #+
 #geom_hline(yintercept=0.5,linetype=2, colour = "grey")
p <- p +  xlab("") + 
          ylab("predicted probability that\nparticipants evaluate process")
p <- p +  scale_x_discrete(labels = c("4-5", "6-7", "8-9", "adults")) +
               theme(axis.text.x = element_text(size = 10, face=NULL, 
                                  margin = margin(t = 2, r = 0, b = 0, l = 0, 
                                                  unit = "mm")),
                     axis.title.x = element_text(size = 5, face=NULL, 
                                   margin = margin(t = 10, r = 0, b = 0, l = 0, 
                                                   unit = "mm")), 
                     axis.title.y = element_text(size = 12, face=NULL,
                                                 margin = margin(t = 0, r = 3, b = 0, l = 0, 
                                                                 unit = "mm")),
                      legend.position="right", legend.title=element_blank())
p <- p +  scale_color_manual(labels = c("China", "USA"), values = c("gray70", "black"))
p




