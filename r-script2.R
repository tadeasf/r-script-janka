#to-do
#1. Upload csv files into "input-files" folder. First row of the csv must be the header of the table, so delete all rows above that. You should keep the "summary rows" at the bottom of a csv file, though. 
##check them here
setwd("//cloud//project//input_files")
head(read.csv('Ads21.csv'))
tail(read.csv("Ads21.csv"))
#2. ADWORDS: Month colunm should be in "YY-MMM" format, if it isn't run this function
##changeDateFormat(fileName='ADWORDS.csv',dateFormat='Month-Year')
##for 'January 2020' use changeDateFormat(fileName='ADWORDS.csv',dateFormat='Month Year')
#3. BING: Month colunm should be in "DD/MM/YYYY" format, if it isn't run this function
##changeDateFormat(fileName='Bing18.csv',dateFormat="Month/Day/Year")
#3. Load functions: open tCPA_functions.R file; hit CTRL+A and "Run"; after this you shloud see the loaded functions listed in "Environment" windows on the right
#4. Set desired parameters bellow and run a chosen function.

#.......................................................1. aggregates adwords and bing information about camgaigns per route or per country
campaignInfo_per_route(adwordsFile = 'New19.csv',bingFile = 'Bing19.csv',convColumnName = 'Conversions',consider_direction=T,create.csv=F)

campaignInfo_per_country(adwordsFile = 'Ads20.csv',bingFile = 'Bing19.csv',convColumnName = 'Conversions',consider_direction=F,create.csv=F)

#.......................................................2. calculate number of confirmed trips per route/country
route_popularity("2021-12-07_confirmed_orders_summary.csv",from='2019-12-01',to='2020-02-02',per.month=T,consider_direction=T,exclude_TA=T,create.csv = F,locations=T)[1:30,]

countryToCountry_popularity("2021-12-07_confirmed_orders_summary.csv",from='2019-01-01',to='2019-05-15',per.month=T,consider_direction=F,exclude_TA=F,create.csv = F)#[1:30,]

#.......................................................3. summary table for campaigns aggregated per route/country for chosen year
ads_summary_per_route_yearly(
  adwordsFile = 'Ads21.csv',
  bingFile = 'Bing21.csv',
  ordersFile = "2021-12-07_confirmed_orders_summary.csv",
  year=2020,
  convColumnName = 'Conversions',
  consider_direction=T,
  adjust_nonadvertised_trips=T, #Set to False when using only subset of routes
  show_only_routes_from_Adwords=F, #Set to False if you want to see all conversions we had, otherwise only summary of advertised routes will be shown
  exclude_TA=F,
  add.routes.info=T, #this adds origin location and origin country if consider_direction=T
  create.csv=T)#[1:20,]

ads_summary_per_country_yearly(
  adwordsFile = 'Ads21.csv',
  bingFile = 'Bing21.csv',
  ordersFile= "2021-12-07_confirmed_orders_summary.csv",
  year=2021,
  convColumnName = 'All.conv.',
  consider_direction=T,
  exclude_TA=F,
  create.csv=F)#[1:30,]


#.......................................................4. summary table for campaigns aggregated per route for 3 chosen years - monthly comparison
ads_summary_per_route(
  adwordsFile1 = 'Ads19.csv',                                 # use a file for 2018 here
  bingFile1 = 'Bing19.csv',                                   # use a file for 2018
  
  adwordsFile2 = 'Ads20-new.csv',                                 # use a file for 2019
  bingFile2 = 'Bing20-new.csv',                                   # use a file for 2019
  
  adwordsFile3 = 'Ads21.csv',                                 # use a file for 2020
  bingFile3 = 'Bing21.csv',                                   # use a file for 2020
  
  ordersFile= "2020-11-20_confirmed_orders_summary.csv",
  
  convColumnName = 'All.conv.',
  consider_direction=F,
  adjust_nonadvertised_trips=T, #Set to False when using only subset of routes
  show_only_routes_from_Adwords=F, #Set to False if you want to see all conversions we had, otherwise only summary of advertised routes will be shown
  show_all_columns_in_output=F, #Set to False if you dont need following columns in the output: Impr., Clicks, Ads_Conv, Google_search_impr.
  exclude_TA=F, #Exclude Travel Agents booking
  create.csv=T)

#.......................................................5. summary table for campaigns aggregated per country for 3 chosen years - monthly comparison

ads_summary_per_country(
  adwordsFile1 = 'Ads19.csv',                                 # use a file for 2018 here
  bingFile1 = 'Bing19.csv',                                   # use a file for 2018
  
  adwordsFile2 = 'Ads20-new.csv',                                 # use a file for 2019
  bingFile2 = 'Bing20-new.csv',                                   # use a file for 2019
  
  adwordsFile3 = 'Ads21.csv',                                 # use a file for 2020
  bingFile3 = 'Bing21-new.csv',                                   # use a file for 2020
  
  year1 = 2019,
  year2 = 2020,
  year3 = 2021,
  
  # also this needs to be changed in the function definition to the recent year
  # and the subsequent rows to accurately filter the output columns
  # some future analyst can fix the function to use it's arguments correctly
  # colnames(out)[25:35]=paste(colnames(out)[25:35],"2021",sep="")
  
  ordersFile= "2021-12-07_confirmed_orders_summary.csv",
  
  convColumnName = 'All.conv.',
  consider_direction=F,
  show_all_columns_in_output=F, #Set to False if you dont need following columns in the output: Impr., Clicks, Ads_Conv, Google_search_impr.
  exclude_TA=F,
  create.csv=T)#[1:10,]
