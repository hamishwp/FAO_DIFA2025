# How do we delimit some of the data variables, such as ISO3C codes?
delim<-dely<-"  :  "
# EM-DAT Token
emdat_token <-""
# IDMC token
idmc_token<-""
# Number of cores to use
ncores<-parallel::detectCores()
# ssh -i githubkeys ifrctgc0root@98.71.131.43
# Use this to see the keys for the PostgreSQL and Blobstorages
# ifrctgc0root@ifrctgc001vm:~/devops$ head *
