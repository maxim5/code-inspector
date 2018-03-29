# Getting and Cleaning Data - Coursera Data Science Track - 6-15-2014
# Author: Ryan de Vera
#Problem 1

library(httr)

# To obtain the ClientID and ClientSecret you have to register an application on github. To do so go to the following link:
# https://github.com/settings/applications.
# From here click on "Register new application"
# Application name: Coursera Data Science Track
# Homepage URL is the URL for your GitHub homepage - if you are not sure how to find this just click on your user name
# in the top right corner and the URL in your browser is the Hompage URL - mine is https://github.com/rydevera3
# Application description - breif description 
# Authorization callback URL - http://Localhost:1410
# Once all this information is put in - click register application 
# There will be a box that shows "Revoke all user tokens" and "Reset client secret" the ClientID
# and ClientSecret are found right above those buttons.

ClientID = "44eb4dc3634e0fbf31cb"
ClientSecret = "6bf10156e0fd8043e35dac81d574023bc1286bf8"

myapp <- oauth_app("github", ClientID, ClientSecret)
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
req <- GET("https://api.github.com/rate_limit", config(token = github_token))
stop_for_status(req)
content(req)

# curl -u Access Token:x-oauth-basic "https://api.github.com/users/jtleek/repos"
BROWSE("https://api.github.com/users/jtleek/repos",authenticate("Access Token","x-oauth-basic","basic"))

# "name": "datasharing", "created_at": "2013-11-07T13:25:07Z"

# Problem 2

# set working directory to where the getdata-data-ss06pid.csv can be found
# If you do not have it already download the package "sqldf" - install.packages("sqldf")
# I had an issue with "sqldf" and Direct X11 I am using the newest version of mac OSX to remedy this I downloaded 
# XQuartz-2.7.6 and restarted my computer. 
library(sqldf)
acs<-read.csv("getdata-data-ss06pid.csv",header=T,sep=",")
sqldf("select pwgtp1 from acs where AGEP < 50")

# Problem 3
length(unique(acs$AGEP)) #91
nrow(sqldf("select distinct AGEP from acs")) #91

# Problem 4
hurl <- "http://biostat.jhsph.edu/~jleek/contact.html" 
con <- url(hurl)
htmlCode <- readLines(con)
close(con)
sapply(htmlCode[c(10, 20, 30, 100)], nchar)
# Output
# <meta name="Distribution" content="Global" />               <script type="text/javascript"> 
#                                           45                                            31 
# })();                 \t\t\t\t<ul class="sidemenu"> 
#     7                                            25 

# Problem 5
# Data can be found at - https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for 
list.files()
data <- read.csv("./getdata-wksst8110.for", header=T)
head(data)
dim(data)
file_name <- "./getdata-wksst8110.for"
df <- read.fwf(file=file_name,widths=c(-1,9,-5,4,4,-5,4,4,-5,4,4,-5,4,4), skip=4)
head(df)
sum(df[, 4])

# 32426.7







