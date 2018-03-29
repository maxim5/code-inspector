###############################################################################
# Title: PSYC 7765 - Lesson 05 - Homework - Answers
# Author: Jeffrey R. Spies (jspies@virginia.edu)
# History: date()
#   Fri Sep  9 18:11:02 2011 - Created the file - JRS
###############################################################################

# We'll start off by removing previously created objects from the environment
rm(list=ls()) 

###############################################################################
# DO NOT CHANGE THIS
###############################################################################

psyc7765.lesson <- 5 # DON'T CHANGE

###############################################################################
# CHANGE THIS
###############################################################################

psyc7765.name <- 'Type your name here'
psyc7765.id <- 'Type your computing id here' # (e.g. 'js6ew')

###############################################################################
# Background
###############################################################################

# Download 2006.sav and gss_codebook_2006.txt from the course website.

setwd('/your/path') # Set your working directory appropriately.

# We will be using data from the 2006 wave of the General Social Survey (GSS).
# You can find infomration about it at http://www3.norc.org/GSS+Website/
# but the following is a description from that site:

# The GSS contains a standard 'core' of demographic, behavioral, and attitudinal
# questions, plus topics of special interest. Many of the core questions have 
# remained unchanged since 1972 to facilitate time-trend studies as well as 
# replication of earlier findings. The GSS takes the pulse of America, and is 
# a unique and valuable resource. It has tracked the opinions of Americans
# over the last four decades.

require(foreign)
dat <- read.spss('2006.sav', to.data.frame=T)
names(dat) <- tolower(names(dat))
dim(dat) # [1] 4510 Participants 1259 Variables

# When looking at multiple factor variables, table and prop.table are handy
# functions.  Here are two factors in the data frame:

str(dat$relig)
levels(dat$relig)

str(dat$sex)
levels(dat$sex)

with(dat, table(relig, sex)) # Frequency of males/females for each religious group in the sample

(tab1 <- with(dat, 
     prop.table(table(relig, sex))
))
sum(tab1) # Whole table adds to 1

(tab2 <- with(dat, 
     prop.table(table(relig, sex), margin=1)
))
sum(tab2[1,]) # Rows add up to 1
sum(tab2[2,]) # ...

(tab3 <- with(dat,
     prop.table(table(relig, sex), margin=2)
))
sum(tab3[,1]) # Columns add up to 1
sum(tab3[,2]) # ...

# What is the qualatative difference between 1 and 2 in the margin argument to
# prop.table?  Make sure you understand this.

# When looking at numeric variables along with factor variables, you can do 
# this:
with(dat, mean(age[sex=="MALE"], na.rm=T))
with(dat, mean(age[sex=="FEMALE"], na.rm=T))
# or more concisely:
with(dat, tapply(age, sex, mean, na.rm=T))
# Age is numeric, sex is a factor, and mean is a function. `tapply` applies the 
# function to the numeric scores for each level of the factor as well as passing
# anything else (e.g. the `na.rm` argument) to the function as arguments.

# Remember, variance/sd can sometimes be just as, if not more, interesting than 
# means!
with(dat, tapply(age, sex, sd, na.rm=T))

# Use logicals to combine levels or thresholds of variables to create
# interesting selections:

str(dat$relig)
levels(dat$relig)
sel_catholic_or_protestant <- dat$relig == "CATHOLIC" | dat$relig == "PROTESTANT"
# or sel_catholic_or_protestant <- with(dat, relig == "CATHOLIC" | relig == "PROTESTANT")

sel_male <- dat$sex == "MALE"

# Let's look at the following item from gss_codebook_2006.txt:
#    prayer    BIBLE PRAYER IN PUBLIC SCHOOLS

# For participants <= 30 years old that are catholic/protestant, what is the 
# proportion of each level of prayer for each level of 'bible'? You will need 
# the following variables (always str them first and, if they're factors, 
# examine their levels:

str(dat$age)

str(dat$prayer)
levels(dat$prayer)

str(dat$bible)
levels(dat$bible)

# Note the margin argument is set to 1:

with(dat[dat$age <= 30 & sel_male & sel_catholic_or_protestant,], 
     prop.table(table(bible, prayer), margin=1)
)

# What do the results say? Does that make sense?

with(dat[dat$age <= 30 & !sel_male & sel_catholic_or_protestant,],
     prop.table(table(bible, prayer), margin=1)
)

# Just as unintuitive. But...

# From http://publicdata.norc.org:41000/gss/documents//BOOK/Main%20Body.pdf
# [VAR: PRAYER]
#    123.  A.  The United States Supreme Court has ruled that no state or local 
#              government may require the reading of the Lord's Prayer or Bible 
#              verses in public schools. What are your views on this--do you 
#              approve or disapprove of the court ruling?

# Ah, that makes more sense....

# In the following questions, you will come up with code that might help you in
# answering the questions.  No statistics are necessary. I will give you the description 
# of the variable to use or the variable name itself.  If given the description, 
# open 'gss_codebook_2006.txt' and use menu Edit/Find and Replace to figure out 
# what the variables you need to use are.  For instance, RS RELIGIOUS PREFERENCE 
# is 'relig' and SPOUSES RELIGIOUS PREFERENCE is 'sprel'. You do not need to 
# place your work in the usual psyc.7765.1.1() format.
# 
# The third problem for this assignment is coming up with your own questions
# to answer from this dataset.  All of the sub problems in Problem 2 are 
# questions from the students last semester.

###############################################################################
# Problem 1 
# Describe what the following funciton does and what its arguments
# do in a comment below the function. You will need to use it in the questions
# that follow.
###############################################################################

per.table <- function(x, margin=NULL, digits=3){
    round(prop.table(x, margin=margin), digits=digits)*100
}

with(dat[dat$age <= 30 & sel_male & sel_catholic_or_protestant,], 
     per.table(table(bible, prayer), margin=1)
)

with(dat[dat$age <= 30 & !sel_male & sel_catholic_or_protestant,],
     per.table(table(bible, prayer), margin=1, digits=5)
)

# ANSWER HERE.  per.table creates...  x is... margin is... digits is...

###############################################################################
# Problem 2.1
# For those who have had extra-marital sex (evstray), is being "reborn" (reborn; 
# having a born again experience) related to their views on whether 
# extra-marital sex is wrong (xmarse)?
###############################################################################

# I'll do this one for you:
     
# The people that had extra-marital sex:
with(dat[dat$evstray=="YES",], per.table(table(xmarsex, reborn), margin=2))
     
# Those that didn't have extra-martial sex:
with(dat[dat$evstray=="NO",], per.table(table(xmarsex, reborn), margin=2))

# I use margin=2 because within each level of "reborn" I want to see how they
# endorse "xmarsex".

# For those who have never had extra-martial sex, it seems like they have a 
# pretty solid view that extra-marital sex is wrong, born again or not, although
# those born again repond "ALWAYS WRONG" more often (93.1%) than those who
# have not been born again (80.5%). However, when comparing this to the table
# for those who have had extra-marital sex, particiants endorse other levels
# more often (ALWAYS WRONG 74% and 46%).  Those who are born again endorse
# ALWAYS WRONG less having had cheated  than those who hadn't (74% vs 93.1%) but
# the differnece is not as great as those who had not been born again (46% 
# vs 80.5%) indicating that the one's ethics/morality about extramarital sex is
# influenced by being born again regardless of what one's behavior might
# indicate.  They cheat, but have strong feelings that it is wrong. NOTE:
# Your descriptions of the results don't have to be this long; a short
# description will suffice.

# I used reborn as the second argument of the table because it has fewer levels.
# It's a bit cleaner that way.
     
###############################################################################
# Problem 2.2
# How does HOURS PER DAY WATCHING TV for men watch relate to how often to their
# FREQUENCY OF SEX DURING LAST YEAR?
###############################################################################

# Again, I'll do this one for you:
     
# We want to select MALES from dat$sex and exmaine tvhours and sexfreq. tvhours
# is continuous, whereas sexfreq is categorical, so I'll use tapply to extract
# the means of tvhours for each group of sexfreq.

with(dat[dat$sex == "MALE",], tapply(tvhours, sexfreq, mean, na.rm=TRUE))

# Those who have had sex "NOT AT ALL" in the last year, do tend to watch more
# TV (3.38 hours/day) than those who have had some amount of 
# sex (2.89 hours/day, 2.68, 2.81, 2.5, etc.), with a minimum tv watching
# for those who have weekly sex (2.5 hours).  The group having the most
# sex (4+ PER WEEK) actually have more sex (2.70 hours) than many of the the
# groups (ONCE A MONTH, WEEKLY, 2-3 PER WEEK).  The question doesn't ask about 
# it, but this trend holds for women, but the differnce in hours of TV per day
# being much smaller at the end points.

###############################################################################
# Problem 2.3
# How does sex (sex) and an understanding what science is about (scistudy)
# predict whether or not you believe in human evolution (evolved)?
###############################################################################

# I'll do one more for you:

sel_IsFemale <- dat$sex=="FEMALE"

with(dat[sel_IsFemale,],  per.table(table(scistudy, evolved), margin=2))
with(dat[!sel_IsFemale,], per.table(table(scistudy, evolved), margin=2))

# Not really all that different when looking at percentages across scistudy
# within level of evolved.
     
with(dat[sel_IsFemale,], per.table(table(scistudy, evolved), margin=1))
with(dat[!sel_IsFemale,], per.table(table(scistudy, evolved), margin=1)) # or dat$sex == "MALE"
     
# Women, regardless of their understanding of science, seem to believe less
# in evolution than men. Even in the middle group of scientific-understanding,
# the majoriy of men belive in evolution, whereas, for women, the majority
# do not. Remember: we don't know if these differences are significant; this is
# simply descriptive exploration.

###############################################################################
# Problem 2.4
# How do males/females (sex) of different political views (polviews) respond to
# the statement "women aren't suited for politics" (fepol)?
###############################################################################

your code

# Your description

###############################################################################
# Problem 2.5
# Is HIV testing (hivtest) in people in their 20s (age) related to number of 
# sexual partners (partners)?
###############################################################################

your code

# Your description

###############################################################################
# Problem 2.6
# Aggregrate the levels in variable THINK OF SELF AS LIBERAL OR CONSERVATIVE 
# into two categories: liberals and conservatives. For each group, what is the 
# relationship between education level (degree) and support for gay marriage 
# (marhomo)?
###############################################################################

your code

# Your description

###############################################################################
# Problem 2.7
# Do full-time working (partfull) moms (childs) tend to be happier (happy) when 
# they're married than when they're single (marital)? 
###############################################################################

your code

# Your description     

###############################################################################
# Problem 2.8
# Create a table using `rbind` of the mean respondent income (realinc) by views 
# on: (a) taxes on the poor (taxpoor); (b) taxes on the middle class (taxmid); 
# and (c) taxes on the rich (taxrich).
###############################################################################

your code
     
###############################################################################
# Problem 2.9
# What is the relationship between respondents having or not having
# the same religious preference (RS RELIGIOUS PREFERENCE) as their spouse 
# (SPOUSES RELIGIOUS PREFERENCE) with GENERAL HAPPINESS, HAPPINESS OF MARRIAGE,
# FREQUENCY OF SEX DURING LAST YEAR, and IS LIFE EXCITING OR DULL? I would 
# recommend creating a variable called hasSameRelig to explore this question.
###############################################################################

your code

# Your description

###############################################################################
# Problem 3: Come up with two (or more) questions that you find interesting or 
# have interesting results that can be examined using selections and tables 
# and/or tapplys and the R code needed to examine them. Try to use more than 
# 2 variables to answer each question. I will choose the most interesting 
# (in terms of analysis and/or question/result) to go over in class next week,
# so try hard and win the honor and respect of your peers. If you work in 
# groups on this problem, each person should have different questions.
###############################################################################

# Question 1
# R code to examine question 1

# Question 2
# R code to examine question 1
