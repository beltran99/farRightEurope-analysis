library(plyr)
library(dplyr)

mod_path = "./data/elections_modified/"

countries = c('AT','BE','BG','CH','CZ','DE','DK','EE','ES','FI','FR','GK','HU','IE','IS','IT','LU','LV','NL','NO','PL','PT','SE','SI','SK','TR','UA','UK')
coalition_names = c("Christian Democratic Union/Christian Social Union","Forza Italia, Lega, Fratelli d'Italia, UDC","PD, +Europa, SVP-PATT, Civica Popolare, Italia Europa Insieme","Pier Luigi Bersani","Silvio Berlusconi","Mario Monti","Walter Veltroni","Romano Prodi")
column_names = list(
  "DE"=c("Region","Year","Party","FirstRoundVotes","FirstRoundVotesPercentage","DirectSeats","SecondRoundVotes","SecondRoundVotesPercentage","ListSeats","TotalSeats"),
  "IT"=c("Region","District","Year","TypeOfBallot","Party","Votes","VotesPercentage","TotalSeats"),
  "ES"=c("Region","Region2","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "DK"=c("Region","District","Year","ListLetter","Party","Votes","VotesPercentage","DistrictSeats","AdditionalSeats","TotalSeats"),
  "GK"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "NL"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "FI"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "SE"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "BE"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "CH"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "CZ"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "EE"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "IS"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "LU"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "LV"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "NO"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats"),
  "PT"=c("Region","Year","Party","Votes","VotesPercentage","TotalSeats")
)

elections=list()  # all countries dfs' list

for (country in countries){
  
  assign(country,list())  # each country dfs' list ... one for DE, IT
  
  # import of .csv election result files
  temp = list.files(path=mod_path,pattern=country)
  
  for (file in temp){
    # files reading
    df_name <- gsub('.csv', '', file)
    file <- paste(mod_path,file,sep="")
    
    country_list=get(country)
    country_list[[df_name]] <- read.csv(file)
    
    assign(country,country_list)
  }
  
  # merge dataframes
  country_list=get(country)
  elections[[country]] <- rbind.fill(country_list)
  assign(country,country_list)
}

electoral_results <- rbind.fill(elections)