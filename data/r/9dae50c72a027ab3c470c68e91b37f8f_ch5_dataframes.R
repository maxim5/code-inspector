# On an intuitive level, a data frame is like a matrix, with a two dimensional
# rows-and-columns structure
# its different than a matrix in that each column can have a different mode
# On a technical level a dataframe is list. Each column of a dataframe does not
# have to be a vector, but this is rare in practice. So essentially each column
# of the dataframe is going to be a vector. 

# ****************************************************************************
# ********************** 5.1 Creating Data Frames ****************************
# ****************************************************************************

kids <- c('Jack', 'Jill')
ages <- c(12, 10)
d <- data.frame(kids, ages, stringsAsFactors=FALSE)


# ********************** 5.1.1 Accessing Data Frames *************************
# list a list
d[[1]]
d$kids

# like a matrix
d[, 1]
str(d)

# ******* 5.1.2 Extended Ex: Regression Analysis of Exam Gres ****************
examsquiz <- read.csv('exams')
head(examsquiz)

# ****************************************************************************
# ******************** 5.2 Other Matrix-Like Operations **********************
# ****************************************************************************

# ****************** 5.2.1 Extracting Subdata Frames *************************
examsquiz[2:5, ]
examsquiz[2:5, 2]
class(examsquiz[2:5, 2])

# drop: vector or dataframe form
examsquiz[2:5, 2, drop=FALSE]

examsquiz[examsquiz$Exam.1 >= 3.8, ]

# ****************** 5.2.2 More on Treatment of NA Values ********************
x <- c(2, NA, 4)
mean(x)
mean(x, na.rm=TRUE)

subset(examsquiz, Exam.1 >= 3.8)

# use only rows where all data not NA
complete.cases

# ****** 5.2.3 Using the rbind() and cbind() Functions and alternatives *****
rbind(d, list("Laura", 19))
eq <- cbind(examsquiz, examsquiz$Exam.1 - examsquiz$Exam.2)
examsquiz$ExamDiff <- examsquiz$Exam.2 - examsquiz$Exam.1

# *********************** 5.2.4 Applying apply() ****************************
# You can use apply() on data.frames.
# max grade of each student
apply(examsquiz, 1, max)

# *********************** 5.2.5 Extended Ex: A Salary Study *****************
# Note: I don't have the data file for this example
# as.is=TRUE equivalent way to achieve stringsAsFactors=FALSE
all2006 <- read.csv("2006.csv", header=TRUE, as.is=TRUE)
all2006 <- all2006[all2006$Wage_Per=="Year",] # exclude hourly workers
all2006 <- all2006[all2006$Wage_Offered_From > 20000,] # exclude weird cases
all2006 <- all2006[all2006$Prevailing_Wage_amount > 200,] # exclude hourly prvwg

# Column of actual wage to prevailing wage
all2006$rat <- all2006$Wage_Offered_From / all2006$Prevailing_Wage_Amount

# cals median for different subsets
medrat <- function(dataframe) {
    return(median(dataframe$rat, na.rm=TRUE)
}

# Use grep to identify rows containing given job title
se2006 <- all2006[grep("Software Engineer", all2006), ]
prg2006 <- all2006[grep("Programmer", all2006), ]
ee2006 <- all2006[grep("Electronics Engineer", all2006), ]

makecorp <- function(corpname) {
    # notice how we're calling global all2006 from this function
    t <- all2006[all2006$Employer_Name == corpname,]
    return(t)
}

# Create subdata frames for a number of firms
corplist <- c('MICROSOFT CORPORATION', 'ms', 'INTEL CORPORARTION', 'intel',
              'SUN MICROSYSTEMS, INC.', 'sun', 'GOOGLE INC.', 'google')

for(i in 1:(length(corplist)/2)){
    corp <- corplist[2*i - i]
    # create variable names for diff corps
    newdtf <- paste(corplist[2*i], '2006', sep='')
    # create new variable names from character strinrgs
    assign(newdtf, makecorp(corp), pos=.GlobalEnv)
}

# ****************************************************************************
# ******************** 5.3 Merging Data Frames *******************************
# ****************************************************************************

# Similar to a join in database, you can do a merge for dataframes
# merge(x, y)
d1 <- data.frame(kids=c('Jack', 'Jill', 'Jillian', 'John'), 
                 states=c('CA', 'MA', 'MA', 'HI'))

d2 <- data.frame(ages = c(10, 7, 12), kids = c('Jill', 'Lillian', 'Jack'))
merge(d1, d2)

d3 <- data.frame(ages = c(12, 7, 10), pals = c('Jack', 'Jill', 'Lillian'))
merge(d1, d3, by.x='kids', by.y='pals')

d2a <- rbind(d2, list(15, 'Jill'))

# There were two Jills in d2a, and so repeats in the merge
# Choose matching variables carefully
merge(d1, d2a)

# ****************** 5.3.1 Extended Ex: An Employee Database *****************
file_test <- '~/code/ml/kaggle/grockit/data/test.csv'
file_train <- '~/code/ml/kaggle/grockit/data/training.csv'

# This is an amazing command
f.train <- count.fields(file_train, sep=',')
f.test <- count.fields(file_test, sep=',')

table(f.test)
table(f.train)

# check for possible spelling errors in various fields
for(col in 1:6){
    # sort all the elements in a column, unique them, print them
    print(unique(sort(da[, col])))
}

# specify first column from both databases to be the merge column
mrg <- merge(da, db, by.x=1, by.y=1)

# ****************************************************************************
# ************** 5.4 Applying Functions to Data Fraames **********************
# ****************************************************************************

# ************** 5.4.1 Using lapply() and sapply() on Data Frames ************
# Keep in mind that data frames are special cases of lists, with the list
# components consisting of data frame columns. Thus if you call lapply() on a
# dataframe with a specified function f(), then f() will be called on each of
# the frame's columns, with the return values placed in a list.


kids <- c('Jack', 'Jill')
ages <- c(12, 10)
d <- data.frame(kids, ages, stringsAsFactors=FALSE)

# Sort each column of the data frame, and return the sorts in a list
dl <- lapply(d, sort)
# dl is a list, where each list is a vector containing the sorted values of the
# respective dataframe column

# coerce dl into a dataframe
d1 <- as.data.frame(dl)
# But this doesnt make any sense, as each row has jumbled values from the sort

# ************** 5.4.2 Extended Ex: Applying Logistic Regression Models ******
# Let's run log reg model on the abalone data
# Pr(Y=1|X=t) = 1 / (1 + exp(-(β₀ + β₁t)))
# As with linear regression, βi values are estimated from the data, using the
# function glm() with the argument family=binomial
# TODO: revisit logistic regression code from Intro ML and re-create with glm()

# We can use sapply() to fit 8 single-predictor models - one for each of the 8
# variables other than gender in this data set - all just one line of code.

aba <- read.csv('abalone.csv')
abamf <- aba[aba$Gender != "I", ] # exclude infants from analysis
lftn <- function(clmn) {
    glm(abamf$Gender ~ clmn, family = binomial)$coef
}

loall <- sapply(abamf[, -1], lftn)
