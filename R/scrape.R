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

