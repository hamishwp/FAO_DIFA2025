URL<-"https://bulks-faostat.fao.org/production/Production_Crops_Livestock_E_All_Data.zip"

outloc<-"./Data/RawData/"
# Download it and save it out
download.file(URL,outloc)
# Unzip it
unzip(outloc,exdir = str_split(outloc,".zip",simplify = T)[1,1])