library(plyr)
library(dplyr)

ele_path = "./data/elections_modified/"
code_path = "./other/htmlcode.csv"

htmlcodes <-read.csv(code_path)
  
# import of .csv election result files
temp = list.files(path=ele_path)
  
for (file in temp){
  # files reading
  file <- paste(ele_path,file,sep="")
  f <- read.csv(file)
  
  for(code in 1:nrow(htmlcodes)){
    f$Party <- gsub(htmlcodes[code,2],htmlcodes[code,1], f$Party)
    if("ListLetter" %in% colnames(f))
    {
      f$ListLetter <- gsub(htmlcodes[code,2],htmlcodes[code,1], f$ListLetter)
    }
    
  }
  print(file)
  write.csv(f, file, row.names=FALSE) 
}
