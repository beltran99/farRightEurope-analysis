new_imm_prop <- data.frame(imm_prop)
colnames(new_imm_prop) <- rev(c("2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","Geo"))
new_unemployment <- data.frame(unemployment_sub)
colnames(new_unemployment) <- rev(c("2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","Geo"))
new_frp_totals <- data.frame(df)
colnames(new_frp_totals) <- rev(c("2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","Geo"))

years <- rev(c("2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990"))

final_data <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(final_data) <- c('Unemployment','Immigration','FarRightSeats')
for(r in unique(new_frp_totals$Geo)){
  for(y in years){
    une=subset(new_unemployment,Geo==r)
    imm=subset(new_imm_prop,Geo==r)
    frp_s=subset(new_frp_totals,Geo==r)
    if(y %in% colnames(une) & y %in% colnames(imm))
    {
      final_data[nrow(final_data)+1,] = c(as.double(une[1,y]),as.double(imm[1,y]),as.double(frp_s[1,y]))
    } else {
      final_data[nrow(final_data)+1,] = c(NA,NA,as.double(frp_s[1,y]))
    }
    row.names(final_data)[nrow(final_data)] <- paste(r, y, sep="-")
  }
}

# Load data
dd <- dist(scale(na.omit(final_data)), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc)