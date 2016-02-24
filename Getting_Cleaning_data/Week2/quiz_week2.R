## Ques 1
        library(httr)
        
        # 1. Find OAuth settings for github:
        #    http://developer.github.com/v3/oauth/
        oauth_endpoints("github")
        
        # 2. To make your own application, register at at
        #    https://github.com/settings/applications. Use any URL for the homepage URL
        #    (http://github.com is fine) and  http://localhost:1410 as the callback url
        #
        #    Replace your key and secret below.
        myapp <- oauth_app("github",
                           key = "0a2f2414d1b5746a28ad",
                           secret = "f76c6c306fd05d0639dcb4526f3c299cbaca17c9")
        
        # 3. Get OAuth credentials
        github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
        
        # 4. Use API
        gtoken <- config(token = github_token)
        req <- GET("https://api.github.com/rate_limit", gtoken)
        stop_for_status(req)
        content(req)
        
        # OR:
        # 4. Use API
        req <- GET("https://github.com/khillibob/datasharing", config(token = github_token))
        stop_for_status(req)
        output <- content(req)
        list(output[[4]]$name, output[[4]]$created_at)
# Ques 2
library(sqldf);
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
        f <- file.path(getwd(), "ss06pid.csv")
        download.file(url, f)
        acs <- data.table(read.csv(f))
# Ques 3
        query1 <- sqldf("select distinct AGEP from acs")
#Ques 4
        connection <- url("http://biostat.jhsph.edu/~jleek/contact.html")
        htmlCode <- readLines(connection)
        close(connection)
        c(nchar(htmlCode[10]), nchar(htmlCode[20]), nchar(htmlCode[30]), nchar(htmlCode[100]))
        
        
## Ques 5
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
        lines <- readLines(url, n=10)
        w <- c(1, 9, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3)
        colNames <- c("filler", "week", "filler", "sstNino12", "filler", "sstaNino12", "filler", "sstNino3", "filler", "sstaNino3", "filler", "sstNino34", "filler", "sstaNino34", "filler", "sstNino4", "filler", "sstaNino4")
        d <- read.fwf(url, w, header=FALSE, skip=4, col.names=colNames)
        head(d)
        d <- d[, grep("^[^filler]", names(d))]
        sum(d[, 4])
        