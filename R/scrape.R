getSearchResults =
function(title)
{
    tt = getForm("http://www.goodreads.com/search", query = title)
    doc = htmlParse(tt, asText = TRUE)
    u = getNodeSet(doc, "//a[@class='bookTitle' and contains(@href, 'search_version')]/@href")
    u = getRelativeURL(u, "http://www.goodreads.com/search")
}


getReviews = 
function(url, max = 2000, start = 1L, verbose = TRUE)
{
  pageNum = as.integer(start)
  ans = structure(list(username = character(0), userurl = character(0), 
                       date = character(0), stars = integer(0), text = character(0)), 
                  .Names = c("username", "userurl", "date", "stars", "text"), row.names = integer(0), 
                  class = "data.frame")

  u = url
  while(nrow(ans) < max) { 
       if(verbose)
          cat("getting page", pageNum, "\n")
       url = sprintf("%s?page=%d", u, pageNum)
       doc =  htmlParse(url)  
       rvs = getNodeSet(doc, "//div[@class = 'review']")
       tmp = lapply(rvs,  getReviewInfo)
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

