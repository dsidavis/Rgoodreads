getSearchResults =
function(title, curl = getCurlHandle())
{
    tt = getForm("http://www.goodreads.com/search", query = title, curl = curl)
    doc = htmlParse(tt, asText = TRUE)
    u = getNodeSet(doc, "//a[@class='bookTitle' and contains(@href, 'search_version')]/@href")
    u = getRelativeURL(u, "http://www.goodreads.com/search")
}


getReviews = 
function(url, max = 2000, start = 1L, verbose = TRUE, curl = getCurlHandle())
{
  pageNum = as.integer(start)
  ans = structure(list(username = character(0), userurl = character(0), 
                       date = character(0), stars = integer(0), text = character(0)), 
                  .Names = c("username", "userurl", "date", "stars", "text"), row.names = integer(0), 
                  class = "data.frame")

  u = url
  while(nrow(ans) < max) {
#      https://www.goodreads.com/book/reviews/1845?authenticity_token=mkl9F9wWWdzcgRNr4OIh5a5sMiiNk593GbWUyUEKOCDC+QV0b1hUr4uOz9zZGOlkWRmu54HFS7QqJfohSod7Zg==&amp;page=3&authenticity_token=1buhsJlARVvARy/RqU3QytpZVi4WfOR5L66fhrOSWmqNC9nTKg5IKJdI82aQtxhLLSzK4RoqMLocPvFuuB8ZLA==
       url = sprintf("%s&page=%d", u, pageNum)      
       if(verbose)
          cat("getting page", pageNum, url, "\n")
       txt = getURLContent(url, curl = curl)
       doc =  htmlParse(txt, asText = TRUE)
       tmp = xpathApply(doc, "//div[@class = 'review']", getReviewInfo)
       ans = rbind(ans, do.call(rbind, tmp))
 
       a = getNodeSet(doc, "//a[starts-with(., 'next')]")
       if(length(a) == 0)
         break
       
       pageNum = pageNum + 1L
  }

  ans
}

getReviewInfo =
function(rv)
{
   tmp = getNodeSet(rv, ".//a[@class='user']")[[1]]
   name = xmlGetAttr(tmp, "title")
   personURL = xmlGetAttr(tmp, "href")

   date = xmlValue(getNodeSet(rv, ".//a[@itemprop = 'publishDate']")[[1]])

   n = getNodeSet(rv, ".//a[contains(@class, 'staticStars')]")
   if(length(n))
      stars = as.integer(substring(xmlValue(n[[1]]), 1, 1))
   else
      stars = NA

   n = getNodeSet(rv, './/div[@class="reviewText stacked"]')[[1]]
   text = xmlValue(n)

   data.frame(username = name, userurl = personURL, date = date, stars = stars, text = text, stringsAsFactors = FALSE)
}



login =
function(email, password = getOption("GoodReadsPassword", stop("need the Web password (not API key)")),
         curl = getCurlHandle(cookiejar = "", followlocation = TRUE, verbose = TRUE, useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:40.0) Gecko/20100101 Firefox/40.0", ...),
         url = "https://www.goodreads.com/user/sign_in", ...)
{
   txt = postForm(url, 'user[email]' = email, 'user[password]' = password, remember_me = "on", 'next' = "Sign in", style = "post", curl = curl)
   curl
}

getUserRatings =
    #
    #  Give the goodreads user id as the first input.
    #
    #  getUserRatings(6773727)
    #
    #  You will have to first login to goodreads.com using Firefox.
    #  This will allow us to get the relevant cookies from Firefox.
    #
    # https://www.goodreads.com/review/list/6773727?sort=rating&view=reviews
function(id, max = 200, page = 1, verbose = TRUE,
         url = "https://www.goodreads.com/review/list/",
         cookie = getLoginCookie(), curl = getLoginHandle(cookie))
{
    u = sprintf("%s%s", url, id)

    ans = NULL
    while(TRUE) {
        if(verbose)
            cat("getting page", page, "\n")

        txt = getForm(u, view = "reviews", sort = "rating", page = page, curl = curl)
        doc = htmlParse(txt, asText = TRUE)
        tbl = readHTMLTable(doc, which = 2, stringsAsFactors = FALSE)

        if(is.data.frame(ans))
            ans = rbind(ans, tbl)
        else
            ans = tbl

        if(!is.na(max) && nrow(ans) >= max)
            break

        if(length(nxt <- getNodeSet(doc, "//a[@class = 'next_page' and contains(., 'next')]")) == 0)
            break

        page = page + 1L
    }

    names(ans) = trim(names(ans))

    ans[] = mapply(cleanVar, ans, names(ans), SIMPLIFY = FALSE)
    
    invisible(ans)
}

cleanVar =
function(vals, field)
{
   gsub(sprintf("^%s[[:space:]]+", field), "", vals)
}

getLoginHandle =
function(cookie = getLoginCookie(), ..., curl = getCurlHandle())
{
   curlSetOpt(..., cookie = cookie, curl = curl)
   curl
}

getLoginCookie = getLoginCookieFirefox =
function(cookiesDB = getFirefoxCookiesFile())
{
   con = dbConnect(SQLite(), cookiesDB)
   ck = dbGetQuery(con, "SELECT * FROM moz_cookies WHERE baseDomain = 'goodreads.com'")
   paste(ck$name, ck$value, sep = "=", collapse = ";")   
}

getLoginCookieChrome =
function(cookiesDB = getChromeCookiesFile())
{
   con = dbConnect(SQLite(), cookiesDB)
   ck = dbGetQuery(con, "SELECT * FROM cookies WHERE host_key = 'www.goodreads.com' OR host_key = '.goodreads.com'")
   paste(ck$name, ck$value, sep = "=", collapse = ";")   
}


getFirefoxCookiesFile =
function()
{
   ff = list.files("~/Library/Application Support/Firefox/profiles", pattern = "^cookies.sqlite$", recursive = TRUE, full.names = TRUE)
   if(length(ff) == 0)
       stop("cannot determine Firefox profile")
   if(length(ff) > 1)
      ff = ff[ which.max(file.info(ff)$mtime) ]
   else
      ff
}

getChromeCookiesFile =
function()
{
   ff = list.files("~/Library/Application Support/Google/Chrome", pattern = "^Cookies$", recursive = TRUE, full.names = TRUE)
   if(length(ff) == 0)
       stop("cannot determine Firefox profile")
   if(length(ff) > 1)
      ff = ff[ which.max(file.info(ff)$mtime) ]
}





if(FALSE) {

library(RHTMLForms)
library(RCurl)
library(XML)

h = getCurlHandle(followlocation = TRUE, cookiejar = "", verbose = TRUE, useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.130 Safari/537.36")
txt = getURLContent("https://www.goodreads.com/", curl = h)
frm = getHTMLFormDescription(htmlParse(txt, asText = TRUE))
fun = createFunction(frm$sign_in)

v = fun('dtemplelang@ucdavis.edu', getOption('GoodReadsPassword', stop("no password")), remember_me = "on", .curl = h)

fun = 
function (user.email, user.password, remember_me = "on", .url = "https://www.goodreads.com/user/sign_in?source=home", 
    ...,  .opts = structure(list(referer = "https://www.goodreads.com/user/sign_in?source=home"), 
        .Names = "referer"), style = "POST", .curl = getCurlHandle(), 
    .cleanArgs = NULL) 
{
    args = list(`user[email]` = user.email, remember_me = remember_me, utf8 = "✓",         
        `user[password]` = user.password, n = "652192", authenticity_token = "0FJySkQBzLkr1mBGhvvUM7yJi6o+5TcbcE124Y4iUC6YfUr6sGhTIP5+t/fpARB4UajuHKOF+htxgAHj69sVLw==",
         `next` = "Sign in")

    txt = postForm(.url, .params = args, style = "POST", curl = .curl, .opts = .opts)
}
}

