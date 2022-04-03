library(plyr)
library(dplyr)

# if using Rstudio change working directory with setwd() and the path of the project folder
ele_path = "./data/elections/"
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
  
  print(country)
  assign(country,list())  # each country dfs' list ... one for DE, IT
  
  # import of .csv election result files
  temp = list.files(path=ele_path,pattern=country)
  
  for (file in temp){
    print(file)
    # files reading
    df_name <- gsub('.csv', '', file)
    file <- paste(ele_path,file,sep="")

    country_list=get(country)
    country_list[[df_name]] <- read.csv(file)
    assign(country,country_list)
    
    # column naming
    if (country %in% names(column_names) ) {  
      #column_names[[country]]
      colnames(country_list[[df_name]]) <- column_names[[country]]
      #country_list[[df_name]]
    }
    country_list[[df_name]] <- subset(country_list[[df_name]], Party!="Voters" & Party!="Blank or Invalid Ballots" & 
                                                               Party!="Valid Votes" & Party!="Registered Electors" & 
                                                               Party!="Censo" & Party!="Votantes" & Party!="Nulos" &
                                                               Party != "Válidos" & Party!="V&aacute;lidos" & Party!="Blancos" & 
                                                               Party != "Blank and Invalid Votes" & Party != "Invalid Votes" &
                                                               Party != "Invalid Ballots" & Party != "Electores Inscritos" &
                                                               Party != "Envelopes Issued" & Party != "Envelopes Submitted" &
                                                               Party != "Blank and Invalid Ballots" & Party != "Valid Ballot Envelopes" &
                                                               Party != "Total Votes" & Party != "Rejected Votes" & Party != "Turnout")
    
    country_list[[df_name]] <- subset(country_list[[df_name]], Region==country | is.na(Region)==TRUE | Region=="")
    
    country_list[[df_name]] <- country_list[[df_name]] %>%
      mutate(Coalition = if_else(
        (Party %in% coalition_names == TRUE),
        "Yes",
        "No"
      ))
    
    country_list[[df_name]] <- subset(country_list[[df_name]], Coalition=="No")

    country_list[[df_name]] <- country_list[[df_name]] %>% rowwise() %>%
      mutate(TotalSeatsPercentage = TotalSeats / sum(country_list[[df_name]]$TotalSeats) )
    
    country_list[[df_name]][c("Region")] <- country
    
    mod_name =  paste(c(mod_path,df_name), collapse="")
    write.csv(country_list[[df_name]], paste(c(mod_name,"csv"), collapse="."), row.names=FALSE) 
    
    assign(country,country_list)
  }
  
  # merge dataframes
  country_list=get(country)
  elections[[country]] <- rbind.fill(country_list)
  # assign(country,country_list)
  
  elections[[country]]$SeatsPercentageDifference <- ave(elections[[country]]$TotalSeatsPercentage, factor(elections[[country]]$Party), FUN=function(x) c(NA,diff(x)))
  
  assign(country,country_list)
}

electoral_results <- rbind.fill(elections)