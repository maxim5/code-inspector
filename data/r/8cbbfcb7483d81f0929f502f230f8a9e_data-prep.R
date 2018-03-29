#!/usr/bin/Rscript

##=============================================================================
## Prepare the raw PSSA data for analysis
## Tom Moertel <tom@mlao.org>
##
## Here I load the data sets and prepare them for my analysis.  I use
## 2002-2010 PSSA data from the PA Dept. of Education.
##=============================================================================

##=============================================================================
## First, I read in the data sets for math and reading
##=============================================================================

load_pssa_subj <-
  function(subject,
           file,
           year,
           cols,
           skip = 0,
           selector = function(df) df,
           aun = 1,
           county = 2,
           district = 4,
           grade = 5) {
    df <- read.csv(file,
                   skip = skip, na.strings = "#NULL!",
                   as.is=c(grade, cols))
    df <- subset(df, ! grepl("total", df[[grade]], ignore.case=T))
    df <- selector(df)
    df <- data.frame(year = year,
                     aun = df[[aun]],
                     county = df[[county]],
                     district = df[[district]],
                     grade = as.numeric(df[[grade]]),
                     subject = subject,
                     a  = as.numeric(df[[cols[1]]]),
                     p  = as.numeric(df[[cols[2]]]),
                     b  = as.numeric(df[[cols[3]]]),
                     bb = as.numeric(df[[cols[4]]]))
    df
  }


load_mtrd <-
  function(file,
           year,
           math,    # math columns, advanced to below basic
           reading, # reading columns, advanced to below basic
           ...) {
    rbind(load_pssa_subj("math", file, year, cols = math, ...),
          load_pssa_subj("reading", file, year, cols = reading, ...))
}

mtrd_2002 <- load_mtrd("2002MathandReadingperformancelevelsalldistricts.csv",
                       2002, math=8:11, reading=12:15)

mtrd_2003 <- load_mtrd("2003MathandReadingperformancelevelsalldistricts.csv",
                       2003, math=8:11, reading=12:15)

mtrd_2004 <- load_mtrd("2004MathandReadingperformancelevelsalldistricts.csv",
                       2004, math=8:11, reading=12:15)

mtrd_2005 <- load_mtrd("2005MathandReadingperformancelevelsalldistricts.csv",
                       2005, math=9:12, reading=14:17)

mtrd_2006 <- load_mtrd("2006 District Level PSSA Results.csv",
                       2006, math=8:11, reading=14:17, skip=3)

mtrd_2007 <- load_mtrd("2007 District Level PSSA Results.csv",
                       2007, math=7:10, reading=12:15, skip=4)

mtrd_2008 <- load_mtrd("2008 PostAYP District Level PSSA Results.csv",
                       2008, math=7:10, reading=12:15, skip=4)

mtrd_2009 <- load_mtrd("PSSA_Results_Math_and_Reading_District_2009.csv",
                       2009, math=8:11, reading=13:16, skip=3,
                       selector = function(df) {
                         subset(df, Group=="All Students")
                       })

mtrd_2010 <- load_mtrd("PSSA_Results_Math_and_Reading_District_2010.csv",
                       2010, math=8:11, reading=13:16, skip=2,
                       selector = function(df) {
                         subset(df, Group=="All Students")
                       })

mtrd_2011 <- load_mtrd("PSSA_Results_Math_and_Reading_District_2011.csv",
                       2011, math=8:11, reading=13:16, skip=3,
                       selector = function(df) {
                         subset(df, Group=="All Students")
                       })

mtrd_2012 <- load_mtrd("2012 PSSA Math Reading District ALL Students Group by grade level.csv",
                       2012, math=8:11, reading=13:16, skip=3,
                       selector = function(df) {
                         subset(df, Group=="All Students")
                       })


# Then I merge them into a single, composite data set

mtrd_merged <- rbind(mtrd_2002,
                     mtrd_2003,
                     mtrd_2004,
                     mtrd_2005,
                     mtrd_2006,
                     mtrd_2007,
                     mtrd_2008,
                     mtrd_2009,
                     mtrd_2010,
                     mtrd_2011,
                     mtrd_2012)


##=============================================================================
## Next, I read in the data sets for writing
##=============================================================================


wr_2005 <-
  load_pssa_subj("writing",
                 "2005Districtlevelperformancelevelresults11thwriting.csv",
                 2005, cols=7:10)

wr_2006 <-
  load_pssa_subj("writing",
                 "2006 District Level Writing PSSA Results.csv",
                 2006, cols=7:10, skip=3)

wr_2007 <-
  load_pssa_subj("writing",
                 "2007 Writing District Level PSSA Results.csv",
                 2007, cols=7:10, skip=4)

wr_2008 <-
  load_pssa_subj("writing",
                 "2008 District Level Writing PSSA Results.csv",
                 2008, cols=7:10, skip=4)

wr_2009 <-
  load_pssa_subj("writing",
                 "PSSA_Results_Writing_District_2009.csv",
                 2009, cols=8:11, skip=3,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

wr_2010 <-
  load_pssa_subj("writing",
                 "PSSA_Results_Writing_District_2010.csv",
                 2010, cols=8:11, skip=2,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

wr_2011 <-
  load_pssa_subj("writing",
                 "PSSA_Results_Writing_District_2011.csv",
                 2011, cols=7:10, district=3, grade=4, skip=3,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

wr_2012 <-
  load_pssa_subj("writing",
                 "2012 PSSA Writing District All Students Group by grade level.csv",
                 2012, cols=8:11, district=4, grade=5, skip=3,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

# Then I merge them into a single, composite data set

wr_merged <- rbind(wr_2005,
                   wr_2006,
                   wr_2007,
                   wr_2008,
                   wr_2009,
                   wr_2010,
                   wr_2011,
                   wr_2012)


##=============================================================================
## Next, I read in the data sets for science
##=============================================================================


sc_2008 <-
  load_pssa_subj("science",
                 "2008 District Level Science PSSA Results.csv",
                 2008, cols=7:10, skip=4)

sc_2009 <-
  load_pssa_subj("science",
                 "PSSA_Results_Science_District_2009.csv",
                 2009, cols=8:11, skip=3,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

sc_2010 <-
  load_pssa_subj("science",
                 "PSSA_Results_Science_District_2010.csv",
                 2010, cols=8:11, skip=2,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

sc_2011 <-
  load_pssa_subj("science",
                 "PSSA_Results_Science_District_2011.csv",
                 2011, cols=8:11, skip=3,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

sc_2012 <-
  load_pssa_subj("science",
                 "2012 PSSA Science District All Students Group by grade level.csv",
                 2012, cols=8:11, skip=3,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

# Then I merge them into a single, composite data set

sc_merged <- rbind(sc_2008,
                   sc_2009,
                   sc_2010,
                   sc_2011,
                   sc_2012)



##=============================================================================
## Finally, I merge all the data sets into one
##=============================================================================


pssa_all <- rbind(mtrd_merged, wr_merged, sc_merged)


# Next, I write this composite data set into a file that I can
# further clean using Google Refine

write.csv(pssa_all, file="pssa_all_raw.csv")

# After saving this data, I load it into Google Refine and apply the
# data-cleaning transformations specified in the file
# google-refine-prep.json, exporting the result, finally, as the file
# pssa-all-merged-and-cleaned.csv.
