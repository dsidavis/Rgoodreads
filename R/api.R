#source("utils.R")

userInfo =
    # userInfo(23187239)
function(id, url = "https://www.goodreads.com/user/show/", curl = getCurlHandle(),
           key = getOption("GoodReadsKey", stop("no API key")))
{
   u = sprintf("%s%s.xml", url, as.character(id))
   txt = getForm(u, key = key, curl = curl)
   doc = xmlParse(txt)
   u = xmlRoot(doc)[["user"]]
   ui = getSimpleFields(u)
   # Omits the updates and user_shelves
   ui
}


userStatus =
function(id, url = "https://www.goodreads.com/user_status/show/", curl = getCurlHandle(),
           key = getOption("GoodReadsKey", stop("no API key")))
{
   u = sprintf("%s%s", url, as.character(id))
   txt = getForm(u, key = key, format = "xml", curl = curl)
   xmlParse(txt)
}


getReview =
function(id, url = "https://www.goodreads.com/review/show.xml", curl = getCurlHandle(),
           key = getOption("GoodReadsKey", stop("no API key")))
{
   txt = getForm(url, id = id, key = key, curl = curl)
   doc = xmlParse(txt)
   rv = xmlRoot(doc)[["review"]]
   ans = getSimpleFields(rv)
   for(i in c("user", "book")) {
      tmp = getSimpleFields(rv[[i]])
      ans[names(tmp)] = tmp
   }
   ans
}

findGroup =
function(query, page = 1, max = NA, url = "https://www.goodreads.com/group/search.xml", curl = getCurlHandle(),
          key = getOption("GoodReadsKey", stop("no API key")), verbose = TRUE)
{

  total = NA
  ans = list()
  
  while(TRUE) {
       if(verbose)
          cat("page", page, "\n")
       txt = getForm(url, q = query, page = page, key = key, curl = curl)
       doc = xmlParse(txt)

       li = getNodeSet(doc, "//groups/list")
       if(length(li) == 0)
           break

       ans = c(ans, xpathApply(doc, "//group", getSimpleFields))
       
       e = xmlGetAttr(li[[1]], "end", converter = as.integer)
       if(!is.na(max) && length(ans) >= max)
           break

       page = page + 1L
   }

   ans = lapply(ans, addElement, "title")
   do.call(rbind, ans)
}


groupShow =
    # g = groupShow(17941)
function(id, url = "https://www.goodreads.com/group/show/", curl = getCurlHandle(followlocation = TRUE),  # note the followlocation
          key = getOption("GoodReadsKey", stop("no API key")))
{
   u = sprintf("%s%s.xml", url, id)
   txt = getForm(u, key = key, curl = curl, .opts = list(followlocation = TRUE))
   doc = xmlParse(txt)
   g = xmlRoot(doc)[["group"]]
   ans = getSimpleFields(g)
   # ignores folders, members, moderators,  and currently_reading
   ans
}


findBooks =
    #
    # b = findBooks("Into the Wild", max = 40)
    #
function(query, page = 1L, max = NA, field = 'all', url = "https://www.goodreads.com/search/index.xml", curl = getCurlHandle(),
          key = getOption("GoodReadsKey", stop("no API key")), verbose = TRUE)
{
   txt = getForm(url, q = query, page = page, key = key, field = field, curl = curl)
   doc = xmlParse(txt)

   ans = list()
   total = xpathSApply(doc, "//total-results", function(x) as.integer(xmlValue(x)))
   while(TRUE) {
       ans = c(ans, xpathApply(doc, "//work", makeWork))
       if(!is.na(max) && length(ans) >= max)
           break
       page = page + 1L
       if(verbose)
          cat("page", page, "\n")
       txt = getForm(url, q = query, page = page, key = key, field = field, curl = curl)
       doc = xmlParse(txt)
   }

   ans = lapply(ans, addElement, c("original_publication_day", "original_publication_month"))
   do.call(rbind, ans)
#   ans
}

makeWork =
    #
    # Turn a <work> node into an R object.
    #
function(x)
{
  ans = getSimpleFields(x)

  be = x[["best_book"]]
  if(length(be)) {
      ans["title"] = xmlValue(be[["title"]])
      ans["author"] = xmlValue(be[["author"]][["name"]])
      ans["author_id"] = xmlValue(be[["author"]][["id"]])      
  }

  ans
}



bookReviewCounts =
    # bookReviewCounts(c(1400067677, 15926749, 9781743295533))
function(isbn, url = "https://www.goodreads.com/book/review_counts.json", curl = getCurlHandle(),
          key = getOption("GoodReadsKey", stop("no API key")))
{
   txt = getForm(url, isbns = paste(isbn, collapse = ","), key = key, curl = curl)
   ans = fromJSON(txt)[[1]]
   ans = do.call(rbind, lapply(ans, mkReviewCountObj))
   ans$average_rating =  as.numeric(ans$average_rating)

   ans
}

mkReviewCountObj =
function(x)
{
   i = sapply(x, is.null)
   if(any(i))
       x[i] = NA
   as.data.frame(x, stringsAsFactors = FALSE)
}


usersShelves =
    # ss = usersShelves(23187239)
function(id, page = 1L, max = NA, url = "https://www.goodreads.com/shelf/list.xml", curl = getCurlHandle(),
          key = getOption("GoodReadsKey", stop("no API key")), verbose = TRUE )
{
   txt = getForm(url, key = key, user_id = id, page = page, curl = curl)
   doc = xmlParse(txt)
   tmp = xpathApply(doc, "//user_shelf", getSimpleFields)
   do.call(rbind, lapply(tmp, function(x) x[!(names(x) %in% c("sort", "order"))]))
}

usersGroups =
function(id, url = "https://www.goodreads.com/group/list/", curl = getCurlHandle(),
          key = getOption("GoodReadsKey", stop("no API key")))
{
  u = sprintf("%s%s.xml", url, id)
  txt = getForm(u, key = key, curl = curl)
  doc = xmlParse(txt)
  groups = xpathApply(doc, "//group", getSimpleFields)
  do.call(rbind, groups)
}

userReadStatuses =
    #  userReadStatuses(11282263)
function(id, book = TRUE, user = TRUE, url = "https://www.goodreads.com/read_statuses/", curl = getCurlHandle(),
          key = getOption("GoodReadsKey", stop("no API key")))
{
  u = sprintf("%s%s.xml", url, id)
  txt = getForm(u, key = key, format = "xml", curl = curl, .opts = list(followlocation = TRUE))
  doc = xmlParse(txt)
  u = xmlRoot(doc)[["read_status"]]
  ans = getSimpleFields(u, dataframe = !(book || user))
  if(book)
      ans$book = mkBook(u[["book"]])
  if(user)
      ans$user = mkAuthor(u[["user"]])
  
  ans
}


groupMembers =
    # g = groupMembers(1)
function(id, name = NA, page = 1L, url = "https://www.goodreads.com/group/members/", curl = getCurlHandle(),
          key = getOption("GoodReadsKey", stop("no API key")), verbose = TRUE)
{
  u = sprintf("%s%s.xml", url, id)
  params = list(key = key, page = page)
  if(!is.na(name))
      params$q = name
  txt = getForm(u, .params = params, curl = curl)
  doc = xmlParse(txt)
  do.call(rbind, xpathApply(doc, "//group_user", mkGroupUser))
}

mkGroupUser =
function(x)
{
   ans = getSimpleFields(x)
   tmp = getSimpleFields(x[["user"]])
   ans[names(tmp)] = tmp
   ans
}

    
    
userBookReview =
    # r = userBookReview(11282263, 12262741 )
function(user_id, book_id, url = "https://www.goodreads.com/review/show_by_user_and_book.xml", curl = getCurlHandle(),
          key = getOption("GoodReadsKey", stop("no API key")))
{
    txt = getForm(url, key = key, user_id = user_id, book_id = book_id, include_review_on_work = 'false', curl = curl)
    doc = xmlParse(txt)
    r = xmlRoot(doc)[["review"]]
    ans = getSimpleFields(r, dataframe = FALSE)
    ans$user = getSimpleFields(r[["user"]])
    ans$book = mkBook(r[["book"]])
    ans
}



isbnReviews =
    #
    # w = isbnReviews(9781743295533)
    #
function(isbn,  url = "https://www.goodreads.com/book/isbn", curl = getCurlHandle(),
          key = getOption("GoodReadsKey", stop("no API key")))    
{
    txt = getForm(url, key = key, format = "xml", isbn = paste(isbn, collapse = ","), curl = curl)
    doc = xmlParse(txt)
    b = xmlRoot(doc)[["book"]]
    ans = getSimpleFields(b, dataframe = FALSE)

    ans$popular_shelves = getSimpleFields(b[["popular_shelves"]])
    ans$similar_books = xmlApply(b[["similar_books"]], mkBook)
    ans$work = makeWork(b[["work"]])
    ans$authors = xmlApply(b[["authors"]], mkAuthor)
    ans
}

mkBook =
function(node)
{
   ans = getSimpleFields(node, dataframe = FALSE)
   ans$authors = xmlApply(node[["authors"]], mkAuthor)
   ans
}

mkAuthor =
function(node)
{
  xmlSApply(node, xmlValue)
}

