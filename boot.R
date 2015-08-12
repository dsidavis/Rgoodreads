library(RCurl)
library(XML)

source(url("https://github.com/dsidavis/Rgoodreads/raw/master/R/api.R"))
source(url("https://github.com/dsidavis/Rgoodreads/raw/master/R/utils.R"))
source(url("https://github.com/dsidavis/Rgoodreads/raw/master/R/scrape.R"))

# Then run the following command without the # and the my key replaced by you actual developer key.
# options(GoodReadsKey = "my key")

# Then
u = getSearchResults("Wild")
rvs = getReviews(u[1], max = 100)


