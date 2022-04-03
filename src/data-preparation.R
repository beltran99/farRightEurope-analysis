### GR == EL
countries = c('AT','BE','BG','CH','CZ','DE','DK','EE','ES','FI','FR','GK','HU','IE','IS','IT','LU','LV','NL','NO','PL','PT','SE','SI','SK','TR','UA','UK')

filename <- "./data/parties/politicalparties.csv"
parties <- read.csv(filename, na.strings=c("","NA"))
farright_parties <- subset(parties, PoliticalPosition1 == "Far-right" | PoliticalPosition2 == "Far-right")

flags <- c("b","bp","ep","be","e","p")


### unemployment ###
unemployment_f <- "./data/economy/unemployment.tsv"
unemployment <- read.table(file = unemployment_f, sep = '\t', header = TRUE, na.strings=c(":",": ","NA",""))
colnames(unemployment) <- c("AgeUnitSexGeo","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992")

# load stringr library
library(stringr)
# Split name column into firstname and last name
unemployment[c("Age", "Unit","Sex","Geo")] <- str_split_fixed(unemployment$AgeUnitSexGeo, ',', 4)
# Rearrange columns and remove original name column
unemployment <- unemployment[c("Age","Unit","Sex","Geo","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992")]

unemployment_sub = subset(unemployment, Age=="Y15-74" & Unit=='PC_ACT' & Sex=='T')
for(flag in flags){
  for(i in 1:nrow(unemployment_sub)){
    for(j in 1:ncol(unemployment_sub)){
      unemployment_sub[i,j] = gsub(flag, "", unemployment_sub[i,j])
    }
  }
}
### unemployment ###


### immigration ###
immigration_f <- "./data/demography/immigration.tsv"
immigration <- read.table(file = immigration_f, sep = '\t', header = TRUE, na.strings=c(":",": ","NA",""))
colnames(immigration) <- c("AgedefAgeUnitSexGeo","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990")
immigration[c("Agedef","Age","Unit","Sex","Geo")] <- str_split_fixed(immigration$AgedefAgeUnitSexGeo, ',', 5)
immigration <- immigration[c("Agedef","Age","Unit","Sex","Geo","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990")]
immigration_sub = subset(immigration, Agedef=='COMPLET' & Age=='TOTAL' & Sex=="T")
for(flag in flags){
  for(i in 1:nrow(immigration_sub)){
    for(j in 1:ncol(immigration_sub)){
      immigration_sub[i,j] = gsub(flag, "", immigration_sub[i,j])
    }
  }
}
### immigration ###


### population ####
population_f <- './data/demography/population.tsv'

population <- read.table(file = population_f, sep = '\t', header = TRUE, na.strings=c(":",": ","NA",""))
colnames(population) <- c("FreqUnitAgeSexGeo","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990")
population[c("Freq","Unit","Age","Sex","Geo")] <- str_split_fixed(population$FreqUnitAgeSexGeo, ',', 5)
population <- population[c("Freq","Unit","Age","Sex","Geo","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990")]
for(flag in flags){
  for(i in 1:nrow(population)){
    for(j in 1:ncol(population)){
      population[i,j] = gsub(flag, "", population[i,j])
    }
  }
}
### population ####


### italy plots ###
it = subset(unemployment_sub,Geo=="IT")
it = it[,5:33]

m = as.numeric(it)

bp <- barplot(rev(m),
        names.arg = rev(colnames(it)),
        main="Italy Unemployment Rate Evolution",
        xlab="Year",
        ylab="Unemployment Rate",
        border="black",
        col="red",
        density=75
)
text(bp,rev(m)-0.5,labels=rev(m),cex=.8)

it_imm = subset(immigration_sub,Geo=="IT")
it_imm = it_imm[,6:36]

m = as.numeric(it_imm)
options(scipen=10000)
bp <- barplot(rev(m),
              names.arg = rev(colnames(it_imm)),
              main="Italy Immigration Evolution",
              xlab="Year",
              ylab="Number of Immigrants",
              border="black",
              col="red",
              density=75
)
### italy plots ###


df <- data.frame(matrix(nrow=1, ncol=32))
colnames(df) <- rev(c("2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","Geo"))


######
empty_cols <- c('1991', '1990')
unemployment_sub[ , empty_cols] <- NA
unemployment_sub <- unemployment_sub[, rev(c("2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","Geo"))]
immigration_sub <- immigration_sub[, rev(c("2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","Geo"))]
population <- population[, rev(c("2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","Geo"))]
######

### imm percentage ###
imm_prop <- data.frame(immigration_sub)
colnames(imm_prop) <- rev(c("2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","Geo"))
imm_prop[, rev(c("2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","Geo"))]

for(i in 1:nrow(imm_prop)){
  
  c <- imm_prop[i,"Geo"]
  pop <- subset(population,Geo==c)
  
  for(j in 2:ncol(imm_prop)){
    
    if( is.na(imm_prop[i,j]) | is.na(pop[1,j]) ){
      imm_prop[i,j] <- NA
    } else {
      imm_prop[i,j] <- as.numeric(imm_prop[i,j]) / as.numeric(pop[1,j])
    }
  }
}
### imm percentage ###


frparties_names <- farright_parties$Party
for (country in unique(farright_parties$Country)){
  ele <- subset(electoral_results, Region==country)
  frp_results <- ele[ele$Party %in% frparties_names,]
  
  df[nrow(df),"Geo"] <- country
  for(year in unique(frp_results$Year)){
    res_y <- subset(frp_results,Year==year)
    df[nrow(df),as.character(year)] = sum(res_y$TotalSeatsPercentage)
  }
  if(nrow(df) < 25){
    df[nrow(df)+ 1,] <- NA
  }
}

