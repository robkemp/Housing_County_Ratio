library(dplyr)
library(RSQLite)


#### Database Connection/ Creation

acs = src_sqlite("acs_oneyear.sqlite3", create=F)


##### Import Data into Database
acs13h= readr::read_csv("J:/ACS/ACS2013_1yr/ss13hco.csv")
copy_to(acs, acs13h , temporary=FALSE, indexes= list("SERIALNO"))
acs14h= readr::read_csv("J:/ACS/ACS2014_1yr/ss14hco.csv")
copy_to(acs, acs14h , temporary=FALSE, indexes= list("SERIALNO"))


acs13p= readr::read_csv("J:/ACS/ACS2013_1yr/ss13pco.csv")
acs13p$index=seq.int(nrow(acs13p))
copy_to(acs, acs13p , temporary=FALSE, indexes= list("index"))
acs14p= readr::read_csv("J:/ACS/ACS2014_1yr/ss14pco.csv")
acs14p$index=seq.int(nrow(acs14p))
copy_to(acs, acs14p , temporary=FALSE, indexes= list("index"))
