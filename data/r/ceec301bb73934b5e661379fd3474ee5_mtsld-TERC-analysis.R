#!/usr/bin/Rscript

##=============================================================================
##=============================================================================
## Analysis of the use of TERC Investigation math program
## at the Mt. Lebanon, Pennsylvania, School District.
##
## Primary sources:  2002-12 PSSA data
##
## Analysis by the Mt. Lebanon Accountability Organization
## Tom Moertel <tom@mlao.org>
## http://www.mlao.org/
##
## 2012-11-23
##
## This analysis is an R program:  http://www.r-project.org/
##=============================================================================
##=============================================================================


options(digits = 2)

library(ggplot2)
library(scales)
library(plyr)
library(reshape2)
library(directlabels)


##=============================================================================
## Analysis
##=============================================================================

## Load the merged, cleaned multi-year PSSA data and prepare it to be plotted

achievement_labels <- c("advanced", "proficient", "basic", "below basic")

pssa <- read.csv("data/pssa-all-merged-and-cleaned.csv")
pssa <- transform(pssa, aun = NULL, county = NULL)
pssa <- melt(pssa, 1:4)
pssa <- subset(pssa, !is.na(value))  # trim sds w/o results
pssa <- mutate(pssa,
               ngrade = grade,  # as numeric grade level
               grade = factor(grade), # as factor
               cohort = factor(year - ngrade + 12),  # by graduation year
               achievement = factor(
                 variable,
                 levels = c("a", "p", "b", "bb"),
                 labels = achievement_labels),
               value = value/100,  # 34 => 34% = 0.34
               variable = NULL)
levels(pssa$grade) <- paste("grade", levels(pssa$grade))


## Extract convenient subsets for MTSLD and peers

mtlsd           <- "MT LEBANON SD"
peers           <- c("UPPER SAINT CLAIR SD", "NORTH ALLEGHENY SD")
mtlsd_and_peers <- c(mtlsd, peers)

pssa_peers      <- subset(pssa, district %in% mtlsd_and_peers)
pssa_peers_a    <- subset(pssa_peers, achievement == "advanced")
pssa_peers_11   <- subset(pssa_peers, ngrade == 11)
pssa_peers_11a  <- subset(pssa_peers_11, achievement == "advanced")


## Augment MTLSD data set w/ column to track # years exposure
## to TERC Investigations math program

exposure <- function(year, ngrade) {
  year1 <- year - ngrade + 1      # year tested students were in 1st grade
  year5 <- year - ngrade + 5      #   and in 5th grade
  first_exp <- pmax(year1, 2007)  # zero exposure before 2007
  last_exp  <- pmin(year, year5)  # zero exposure after current year
  pmax(0, last_exp - first_exp + 1)
}

pssa_mtlsd      <- mutate(subset(pssa_peers, district == mtlsd),
                          exposure = exposure(year, ngrade))
pssa_mtlsd_a    <- subset(pssa_mtlsd, achievement == "advanced")
pssa_mtlsd_11   <- subset(pssa_mtlsd, ngrade == 11)
pssa_mtlsd_11a  <- subset(pssa_mtlsd_11, achievement == "advanced")


p <-
qplot(ngrade, value,
      data = subset(pssa_mtlsd_a, subject %in% c("reading", "math")),
      geom = "line",
      facets = . ~ subject,
      group = cohort,
      color = exposure,
      main = "Student progress w.r.t. exposure to TERC Investigations",
      xlab = "Grade level",
      ylab = "Portion of students testing at advanced level of achievement",
      asp = 1
      ) +
  scale_x_continuous(breaks = unique(pssa_mtlsd_l$gd)) +
  scale_y_continuous(breaks = seq(0.25, 1, 0.25), labels = percent)
  # geom_text(aes(label = year), size = 3)
p

p + geom_text(aes(label = cohort),
              data = ddply(pssa_mtlsd_a, .(subject, cohort), function(df) {
                subset(df, ngrade == max(df$ngrade))
              }))


p <-
qplot(exposure, value,
      data = subset(pssa_mtlsd_a),
      facets = subject ~ grade,
      main = paste(sep = "\n",
        "Standardized test achievement vs. exposure to TERC Investigations",
        "Mount Lebanon School District, 2002-2012 PSSA data"),
      xlab = "Exposure to TERC Investigations math regime (years)",
      ylab = "Portion of students testing at advanced level of achievement",
      asp = 1
      ) +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0.25, 1, 0.25), labels = percent)
ggsave(plot = p, file = "out/mtlsd-class-achievement-vs-terc-exposure.pdf",
       useDingbats = F, height = 7.5, width = 11)


summary(lm(value ~ grade * (exposure > 0), data = subset(pssa_mtlsd_l, achievement == "advanced" & subject == "math")))

p <-
qplot(gd, value, data = subset(pssa_mtlsd_l, achievement == "advanced" & subject == "math"),
      geom = c("line"),
      group = cohort,
      facets = subject ~ .,
      main = "Mt. Lebanon School District, students testing at advanced level, 2002-2012",
      xlab = "Grade level",
      ylab = "Portion of students testing at given level of achievement or higher",
      asp = 1
      ) +
  geom_point(aes(color = year - gd >= 2007 - 3)) +
  scale_x_continuous(breaks = unique(pssa_mtlsd_l$gd)) +
  scale_y_continuous(breaks = seq(0.25, 1, 0.25), labels = percent)
p

p <-
qplot(gd, value, data = subset(pssa_mtlsd_l, achievement == "advanced" & subject == "math"),
      geom = c("line"),
      color = exposure,
      group = cohort,
      facets = subject ~ .,
      main = "Mt. Lebanon School District, students testing at advanced level, 2002-2012",
      xlab = "Grade level",
      ylab = "Portion of students testing at given level of achievement or higher",
      asp = 1
      ) +
  scale_x_continuous(breaks = unique(pssa_mtlsd_l$gd)) +
  scale_y_continuous(breaks = seq(0.25, 1, 0.25), labels = percent)
p




(pssa_mtlsd_a_long <- dcast(pssa_mtlsd_a, ngrade + exposure + year ~ subject))

pssa_mtlsd_a_long_4 <- subset(pssa_mtlsd_a_long, grade == "grade 4")


summary(lm(math ~ exposure + reading, data = pssa_mtlsd_a_long))

summary(lm(math ~ exposure + grade * (science + writing + reading), data = pssa_mtlsd_a_long))




require(mgcv)

g1 <- gam(math ~ exposure + te(ngrade,reading), data = pssa_mtlsd_a_long)
summary(g1)

op <- par(mfrow = c(1,1))
plot(g1, residuals=T)
par(op)

gam.check(g1)


g2 <- gam(math ~ te(ngrade,exposure) + te(ngrade,reading), data = pssa_mtlsd_a_long)
summary(g2)

op <- par(mfrow = c(1,1))
plot(g2, residuals=T)
par(op)
