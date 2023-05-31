  changeDateFormat=function(fileName,dateFormat='Month Year'){
    setwd("//cloud//project//input_files")
    #fileName='Ads Monthly 2019.csv'
    temp=read.csv(fileName)
    if(dateFormat=='Month-Year'){temp$Month=format(as.Date(paste('01-',temp$Month,sep=""),format='%d-%b-%y'),'%y-%b')}
    if(dateFormat=="Month/Day/Year"){temp$Month=format(as.Date(temp$Month,format='%m/%d/%Y'),'%d/%m/%Y')}
    if(dateFormat=="Month Year"){
      print("nice job Tom")
      temp$Month=format(as.Date(paste('01-',temp$Month,sep=""),format='%d-%B %Y'),'%y-%b')
      print(head(temp))
    }
    #if(dateFormat=='Year-Month'){temp$Month=format(as.Date(paste('01-',temp$Month,sep=""),format='%d-%y-%b'),'%y-%b')}
    write.csv(temp,fileName,row.names = F)
  }
  
  #-------------------------------------campaignInfo_per_route
  campaignInfo_per_route=function(adwordsFile=NA,bingFile=NA,convColumnName='Conversions',consider_direction=F, create.csv=F){
    print(paste("Aggrerating data from Adwords and Bing."))
    setwd("//cloud//project//input_files")
    
    #-----------------------------------adwords data
    if(!is.na(adwordsFile)){
      print(paste("2. Getting data from Adwords."))
      adwords=
        subset(
          subset(
            read.csv(adwordsFile,encoding='UTF-8'), Status!=''), select = c('Month','Campaign','Cost','Impr.','Clicks',convColumnName,"Search.impr..share"))#,'Campaign.ID'))
      head(adwords)
      colnames(adwords)=c('Date','Campaign','Cost','Impr.','Clicks','Ads_Conv','Google_search_impr')
      
      print(paste("3. Cleaning campaign names from Adwords."))
      adwords$Campaign=as.character(adwords$Campaign)
      adwords$Campaign[which(adwords$Campaign=="DAYTRIP brand")]='1234/45 Daytrip to Daytrip W xx-xx'
      adwords$Campaign[which(adwords$Campaign=="DYNAMIC REMARKETING")]='1234/45 Daytrip to Daytrip W xx-xx'
      adwords$Campaign[which(adwords$Campaign=="Display Remarketing New")]='1234/45 Daytrip to Daytrip W xx-xx'
      adwords$Campaign[which(substr(adwords$Campaign,1,5)=='zAuto')]='1234/45 zAuto to zAuto W xx-xx'
      adwords$Campaign[which(substr(adwords$Campaign,1,15)=='2nd Account DSA')]='1234/45 Daytrip to Daytrip W xx-xx'
      adwords$Campaign[which(adwords$Campaign=="DAYTRIP CZ startup")]='1234/45 Daytrip to Daytrip W xx-xx'
      adwords$Campaign[which(adwords$Campaign=="D2018/07 Display US")]='1234/45 Daytrip to Daytrip W xx-xx'
      
      #drop weird campaigns
      if(sum(substr(adwords$Campaign,1,7)=='NEW WEB')>0){adwords=adwords[-which(substr(adwords$Campaign,1,7)=='NEW WEB'),]}
      if(sum((grepl(' to ',adwords$Campaign)==F & adwords$Cost=='0'))>0){adwords=adwords[-which(grepl(' to ',adwords$Campaign)==F & adwords$Cost=='0'),]}
      
      adwords$Month=as.character(format(as.Date(paste('01-',as.character(adwords$Date),sep=''),format='%d-%y-%B'),'%m'))
      adwords$Year=as.character(format(as.Date(paste('01-',as.character(adwords$Date),sep=''),format='%d-%y-%B'),'%Y'))
      adwords$Cost=as.numeric(gsub(',','',as.character(adwords$Cost)))
      adwords$Impr.=as.numeric(gsub(',','',as.character(adwords$Impr.)))
      adwords$Clicks=as.numeric(gsub(',','',as.character(adwords$Clicks)))
      adwords$Ads_Conv=as.numeric(gsub(',','',as.character(adwords$Ads_Conv)))
      
      adwords$Google_search_impr=suppressWarnings( as.numeric(as.character(gsub(" --",'',gsub('%','',adwords$Google_search_impr))))/100 )
      
      campaignName=function(x){
        #x=adwords$Campaign[504]
        x=trimws(gsub("/:",'',
          gsub(" - ",' ',
          gsub("  ",' ',
          gsub("/ ",'',
         #gsub("zAuto-route -:  ","",
          gsub('\\d','',
          gsub(' US\\|CA ',' U ',
          gsub(' NAF ',' A ',
         # gsub(' BORDER ','',
          gsub(" US ", " U ",
          gsub(" SEA ", " S ",
          gsub(' - eCPC','',
          gsub(' - eCPC Mar19','',
          gsub(' - eCPC Mar 19','',
          gsub(" - tCPA jan 19","",
          gsub(" - lower tCPA","",
          gsub(" - tCPA Jan 19","",x))))))))))))))))#)
        
        if(substr(x,nchar(x)-2,nchar(x)-2)!='-'){x=paste(x,"xx-xx")  }
        x
        if(consider_direction==F){  return( paste(trimws(sort(strsplit(substr(x,1,nchar(x)-8),' to ')[[1]])),collapse=' - ' ) ) }
        if(consider_direction==T){  return( paste(trimws(    (strsplit(substr(x,1,nchar(x)-8),' to ')[[1]])),collapse=' to ') ) }
      }
      
      #library(stringr)
      #temp=unlist(lapply(adwords$Campaign, campaignName))
      #sort(unique(str_sub(temp,start=-2)))
      #adwords$Campaign[which(str_sub(temp,start=-2)=='Ne')]
      #head(adwords)
      
      #adwords$Campaign[c(1:10,123,444,567)]=paste(adwords$Campaign[c(1:10,123,444,567)],'* daco daco')
      adwords$Campaign=ifelse(regexpr('\\*',adwords$Campaign)>0,substr(adwords$Campaign,1,regexpr(' \\*',adwords$Campaign)),adwords$Campaign) #handles all suffixes created after 2019-05-15
      adwords$campaignName=unlist(lapply(adwords$Campaign, campaignName)) #all campaigns experiment suffixes up to 2019-05-15 should handled here
      adwords=subset(adwords,select=c("Cost","Impr.","Clicks","Ads_Conv","Month","Year","campaignName",'Google_search_impr'))
    }
    
    #choose only that bing campaigns that are in adwords
    bing[-which(bing$campaignName %in% adwords$campaignName),]
    bing=bing[which(bing$campaignName %in% adwords$campaignName),]
    
    print(paste("4. Bind Adwords and Bign together."))
    costCombined=rbind(adwords,bing)
    for(i in 1:4){costCombined[,i]=as.numeric(as.character(costCombined[,i]))}
    
    print(paste("5. Calculating weighted average of impression share."))
    #weighted average of impression share
    costCombined$Google_search_imprXImpr=as.numeric(as.character(round(costCombined$Google_search_impr*costCombined$Impr.,3))) #xi*ni
    temp=
      merge(aggregate(Google_search_imprXImpr ~ campaignName+Month+Year, data=costCombined, FUN=sum),
            aggregate(Impr.~ campaignName+Month+Year, data=adwords, FUN=sum),by=c('campaignName','Month','Year'))#2. sum(xi*ni)
    temp$Google_search_impr=round(temp$Google_search_imprXImpr/temp$Impr.,3) #3. sum(xi*ni)/sum(ni)
    temp=subset(temp,select = c('campaignName','Month','Year','Google_search_impr'))
    
    print(paste("6. Aggregating the data by month."))
    out=merge(merge(merge(merge(
      aggregate(Cost ~ campaignName+Month+Year, data=costCombined, FUN=sum),
      aggregate(Impr. ~ campaignName+Month+Year, data=costCombined, FUN=sum),by=c('campaignName','Month','Year')),
      aggregate(Clicks ~ campaignName+Month+Year, data=costCombined, FUN=sum),by=c('campaignName','Month','Year')), 
      aggregate(Ads_Conv ~ campaignName+Month+Year, data=costCombined, FUN=sum),by=c('campaignName','Month','Year')),
      temp,by=c('campaignName','Month','Year'),all.x=T)
    
    if(create.csv==T){
      a=format(min(as.Date(paste('01',out$Month, out$Year),format='%d %b %Y')),'%b-%Y')
      b=format(max(as.Date(paste('01',out$Month, out$Year),format='%d %b %Y')),'%b-%Y')
      setwd("//cloud//project//output_files")
      write.csv(out,paste('Campaign_cost_',a,"_to_",b,'.csv',sep=''),row.names = F)
      setwd("//cloud//project")
    }else{return(out)}
    
    setwd("//cloud//project")
  }
  
  #-------------------------------------campaign cost per route
  campaignInfo_per_country=function(adwordsFile=NA,bingFile=NA,convColumnName='Conversions',consider_direction=F, create.csv=F){
    print(paste("Aggrerating data from Adwords and Bing."))
    setwd("//cloud//project//input_files")

    #-----------------------------------adwords data
    if(!is.na(adwordsFile)){
      print(paste("2. Getting data from Adwords."))
      adwords=
        subset(
          subset(
            read.csv(adwordsFile), Status!=''), select = c('Month','Campaign','Cost','Impr.','Clicks',convColumnName,"Search.impr..share"))#,'Campaign.ID'))
      head(adwords)
      substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
      }
      #divne nazvy
      #temp=adwords[which(substrRight(as.character(adwords$Campaign), 1)=="W" & substr(as.character(adwords$Campaign),1,5)!="zAuto"),]
      #temp$Campaign
      #sum(temp$Cost)
      
      colnames(adwords)=c('Date','Campaign','Cost','Impr.','Clicks','Ads_Conv','Google_search_impr')
      
      print(paste("3. Cleaning campaign names from Adwords."))
      adwords$Campaign=as.character(adwords$Campaign)
      adwords$Campaign[which(adwords$Campaign=="DAYTRIP brand")]='1234/45 Daytrip to Daytrip W xx-xx'
      adwords$Campaign[which(adwords$Campaign=="DYNAMIC REMARKETING")]='1234/45 Daytrip to Daytrip W xx-xx'
      adwords$Campaign[which(adwords$Campaign=="Display Remarketing New")]='1234/45 Daytrip to Daytrip W xx-xx'
      adwords$Campaign[which(substr(adwords$Campaign,1,5)=='zAuto')]='1234/45 zAuto to zAuto W xx-xx'
      adwords$Campaign[which(substr(adwords$Campaign,1,15)=='2nd Account DSA')]='1234/45 Daytrip to Daytrip W xx-xx'
      adwords$Campaign[which(adwords$Campaign=="DAYTRIP CZ startup")]='1234/45 Daytrip to Daytrip W xx-xx'
      adwords$Campaign[which(adwords$Campaign=="2018/07 Display US")]='1234/45 Daytrip to Daytrip W xx-xx' #to-do in the future there will be more display campaigns
      #drop weird campaigns
      if(sum(substr(adwords$Campaign,1,7)=='NEW WEB')>0){adwords=adwords[-which(substr(adwords$Campaign,1,7)=='NEW WEB'),]}
      if(sum((grepl(' to ',adwords$Campaign)==F & adwords$Cost=='0'))>0){adwords=adwords[-which(grepl(' to ',adwords$Campaign)==F & adwords$Cost=='0'),]}
      
      adwords$Month=as.character(format(as.Date(paste('01-',as.character(adwords$Date),sep=''),format='%d-%y-%B'),'%m'))
      adwords$Year=as.character(format(as.Date(paste('01-',as.character(adwords$Date),sep=''),format='%d-%y-%B'),'%Y'))
      adwords$Cost=as.numeric(gsub(',','',as.character(adwords$Cost)))
      adwords$Impr.=as.numeric(gsub(',','',as.character(adwords$Impr.)))
      adwords$Clicks=as.numeric(gsub(',','',as.character(adwords$Clicks)))
      adwords$Ads_Conv=as.numeric(gsub(',','',as.character(adwords$Ads_Conv)))
      
      adwords$Google_search_impr=suppressWarnings( as.numeric(as.character(gsub(" --",'',gsub('%','',adwords$Google_search_impr))))/100 )
      
      campaignName=function(x){
        x=trimws(gsub("/:",'',
                 gsub(" - ",' ',
                 gsub("  ",' ',
                 gsub("/ ",'',
                #gsub("zAuto-route -:  ","",
                 gsub('\\d','',
                 gsub(' US\\|CA ',' U ',
                 sub(' NAF ',' A ',
                #gsub(' BORDER ','',
                 gsub(" US ", " U ",
                 gsub(" SEA ", " S ",
                 gsub(' - eCPC','',
                 gsub(' - eCPC Mar19','',
                 gsub(' - eCPC Mar 19','',
                 gsub(" - tCPA jan 19","",
                 gsub(" - lower tCPA","",
                 gsub(" - tCPA Jan 19","",x))))))))))))))))#)
        x=substr(x,nchar(x)-4,nchar(x))
        
        if(regexpr('-', x)==-1){x='NA-NA'}
        if(consider_direction==F){  return(paste(trimws(sort(strsplit(x,'-')[[1]])),collapse=' - ') ) }
        if(consider_direction==T){  return(paste(trimws(    (strsplit(x,'-')[[1]])),collapse=' to ') ) }
      }#to-do in the future - while cycle for erasing all values in drop vector
      
      adwords$Campaign=ifelse(regexpr('\\*',adwords$Campaign)>0,substr(adwords$Campaign,1,regexpr(' \\*',adwords$Campaign)),adwords$Campaign) #handles all suffixes created after 2019-05-15
      adwords$campaignName=unlist(lapply(adwords$Campaign, campaignName)) #all campaigns experiment suffixes up to 2019-05-15 should handled here
      #adwords[which(adwords$campaignName=="a W"),]
      #adwords[which(adwords$campaignName=="xx - xx"),]
      #adwords[which(adwords$Campaign=="2017/9 Florence to Treviso W"),]
      #adwords[which(adwords$campaignName=='NA - NA'),]
      adwords=subset(adwords,select=c("Cost","Impr.","Clicks","Ads_Conv","Month","Year","campaignName",'Google_search_impr'))
      #unique(adwords$campaignName)
    }
    head(adwords)

    #choose only that bing campaigns that are in adwords
    bing[-which(bing$campaignName %in% adwords$campaignName),]
    bing=bing[which(bing$campaignName %in% adwords$campaignName),]
    
    print(paste("4. Bind Adwords and Bign together."))
    costCombined=rbind(adwords,bing)
    for(i in 1:4){costCombined[,i]=as.numeric(as.character(costCombined[,i]))}
    
    print(paste("5. Calculating weighted average of impression share."))
    #weighted average of impression share
    costCombined$Google_search_imprXImpr=as.numeric(as.character(round(costCombined$Google_search_impr*costCombined$Impr.,3))) #xi*ni
    temp=
      merge(aggregate(Google_search_imprXImpr ~ campaignName+Month+Year, data=costCombined, FUN=sum),
            aggregate(Impr.~ campaignName+Month+Year, data=adwords, FUN=sum),by=c('campaignName','Month','Year'))#2. sum(xi*ni)
    temp$Google_search_impr=round(temp$Google_search_imprXImpr/temp$Impr.,3) #3. sum(xi*ni)/sum(ni)
    temp=subset(temp,select = c('campaignName','Month','Year','Google_search_impr'))
    
    print(paste("6. Aggregating the data by month."))
    out=merge(merge(merge(merge(
      aggregate(Cost ~ campaignName+Month+Year, data=costCombined, FUN=sum),
      aggregate(Impr. ~ campaignName+Month+Year, data=costCombined, FUN=sum),by=c('campaignName','Month','Year')),
      aggregate(Clicks ~ campaignName+Month+Year, data=costCombined, FUN=sum),by=c('campaignName','Month','Year')), 
      aggregate(Ads_Conv ~ campaignName+Month+Year, data=costCombined, FUN=sum),by=c('campaignName','Month','Year')),
      temp,by=c('campaignName','Month','Year'),all.x=T)
    
    if(create.csv==T){
      a=format(min(as.Date(paste('01',out$Month, out$Year),format='%d %b %Y')),'%b-%Y')
      b=format(max(as.Date(paste('01',out$Month, out$Year),format='%d %b %Y')),'%b-%Y')
      setwd("//cloud//project//output_files")
      write.csv(out,paste('Campaign_cost_',a,"_to_",b,'.csv',sep=''),row.names = F)
    }else{return(out)}
    setwd("//cloud//project")
  }
  
  #-------------------------------------route popularity
  route_popularity=function(ordersFile,from='2000-01-01',to='2345-01-01',per.month=F,consider_direction=F,exclude_TA=F,create.csv = F, locations=F){
    print(paste("Calculating route popularity"))
    setwd("//cloud//project//input_files")
    
    print(paste("1. Loading orders data from DB."))
    orders=read.csv(ordersFile)#,encoding = 'UTF-8')
    if(exclude_TA==T){
      print(paste("...without trips booked by travel agents"))
      orders=orders[-which(orders$customOrder=='TA'),]
    }
    
    head(orders)
    #fix type
    orders$createdAt=as.Date(orders$createdAt)
    orders$origin_Location=gsub(' - ',' ',as.character(orders$origin_Location))
    orders$destination_Location=gsub(' - ',' ',as.character(orders$destination_Location))
    #apply chosen time frame
    orders=subset(orders, createdAt>=from & createdAt<to)
    temp.to=max(orders$createdAt)
    #fix locations' names
    orders$origin_Location=gsub(' - ','-',orders$origin_Location)
    orders$destination_Location=gsub(' - ','-',orders$destination_Location)
    #create route name
    if(consider_direction==F){
      routeName=function(x){return(paste(as.character(sort(x)),collapse = ' - '))}
    }else{
      routeName=function(x){return(paste(as.character(x),collapse = ' to '))}
    }
    orders$routeName=apply(subset(orders, select = c("origin_Location","destination_Location")), 1,routeName)
    
    orders$One=1
    orders$finalTotalPrice=as.numeric(as.character(orders$finalTotalPrice))
    head(orders)
    print(paste("2. Aggregate the orders data."))
    if(per.month==T){
      orders$Month=format(as.Date(orders$createdAt),'%m')
      orders$Year=format(as.Date(orders$createdAt),'%Y')
  
      orders2=subset(orders, select = c('routeName','Year','Month','One','finalTotalPrice'))
      out=aggregate(.~routeName+Year+Month,data=orders2,sum)
      #out[which(out$Year=='2019'& out$Month=='05' & out$routeName=='Budapest to Prague'),]
      #sum(orders[which(orders$Year=='2019' & orders$Month=='05' & orders$routeName=='Budapest to Prague'),]$finalTotalPrice)
      colnames(out)=c("routeName",'Year','Month','DB_Conv','TripPrice')
    }else{
      orders2=subset(orders, select = c('routeName','One','finalTotalPrice'))
      out=aggregate(.~routeName,data=orders2,sum)
      #out[which(out$Year=='2019'& out$Month=='05' & out$routeName=='Budapest to Prague'),]
      #sum(orders[which(orders$Year=='2019' & orders$Month=='05' & orders$routeName=='Budapest to Prague'),]$finalTotalPrice)
      colnames(out)=c("routeName",'DB_Conv','TripPrice')
    }
    
    if(locations==T){
      print("...and adding origin country and location information columns.")
      temp=unique(subset(orders, select = c('origin_Location','destination_Location',"origin_Country",'routeName')))
      #x=c("bla",'cha')
      alpha_sort1=function(x){sort(x)[1]}
      alpha_sort2=function(x){sort(x)[2]}
      
      temp$Loc1=apply(temp[,1:2],1,alpha_sort1)
      temp$Loc2=apply(temp[,1:2],1,alpha_sort2)
      temp$origin_Country=as.character(temp$origin_Country)
      temp$origin_Country=ifelse(rep(consider_direction,nrow(temp)),temp$origin_Country,NA)
      temp$origin_Location=as.character(temp$origin_Location)
      temp$origin_Location=ifelse(rep(consider_direction,nrow(temp)),temp$origin_Location,NA)
      
      temp=unique(subset(temp, select=c('routeName','Loc1','Loc2',"origin_Country","origin_Location")))
      head(temp)
      
      out=merge(out, temp, by='routeName',all.x=T)
    }
    

    out=out[order(out$DB_Conv,decreasing = T),]
    head(out)
    
    out$routeName=gsub("[^A-Za-z0-9 -]","",out$routeName)
    
    if(create.csv==T){
      if(from=='2000-01-01'){from='fromBegining'}
      if(to=='2345-01-01'){to=temp.to}
      if(per.month==T){temp='_per_month'}else{temp=''}
      setwd("//cloud//project//output_files")
      write.csv(out,paste('Routes_frequency_',from,"_",to,temp,'.csv',sep=''),row.names = F)
      
    }else{return(out)}
    setwd("//cloud//project")
  }
  
  #-------------------------------------country to country popularity
  countryToCountry_popularity=function(ordersFile,countriesFile='countries.csv', from='2000-01-01',to='2345-01-01',per.month=F,consider_direction=F,exclude_TA=F,create.csv = F){
    print(paste("Calculating country to country popularity"))
    
    setwd("//cloud//project//input_files")
    
    print(paste("1. Loading orders data from DB."))
    countries=subset(read.csv(countriesFile),select=c('englishName','isoCode'))
    orders=
      merge(
        merge(
          read.csv(ordersFile),
          countries,by.x='origin_Country',by.y = 'englishName', all.x=T),
        countries,by.x='destination_Country',by.y = 'englishName', all.x=T, suffixes = c('_from','_to'))
    head(orders)
    
    if(exclude_TA==T){
      print(paste("...without trips booked by travel agents"))
      orders=orders[-which(orders$customOrder=='TA'),]
    }
    
    #fix type
    orders$createdAt=as.Date(orders$createdAt)
    orders$isoCode_from=as.character(orders$isoCode_from)
    orders$isoCode_to=as.character(orders$isoCode_to)
    #apply chosen time frame
    orders=subset(orders, createdAt>=from & createdAt<to)
    temp.to=max(orders$createdAt)
    
    #create route name
    if(consider_direction==F){
      routeName=function(x){return(paste(as.character(sort(x)),collapse = ' - '))}
    }else{
      routeName=function(x){return(paste(as.character(x),collapse = ' to '))}
    }
    orders$routeName=apply(subset(orders, select = c("isoCode_from","isoCode_to")), 1,routeName)
    
    orders$One=1
    orders$finalTotalPrice=as.numeric(as.character(orders$finalTotalPrice))
    
    print(paste("2. Aggregate the orders data."))
    if(per.month==T){
      orders$Month=format(as.Date(orders$createdAt),'%m')
      orders$Year=format(as.Date(orders$createdAt),'%Y')
      
      orders2=subset(orders, select = c('routeName','Year','Month','One','finalTotalPrice'))
      out=aggregate(.~routeName+Year+Month,data=orders2,sum)
      #out[which(out$Year=='2019'& out$Month=='05' & out$routeName=='Budapest to Prague'),]
      #sum(orders[which(orders$Year=='2019' & orders$Month=='05' & orders$routeName=='Budapest to Prague'),]$finalTotalPrice)
      colnames(out)=c("routeName",'Year','Month','DB_Conv','TripPrice')
      #sum(out$TripPrice[which(out$Year=='2018' & out$Month=='03')])
    }else{
      orders2=subset(orders, select = c('routeName','One','finalTotalPrice'))
      out=aggregate(.~routeName,data=orders2,sum)
      #out[which(out$Year=='2019'& out$Month=='05' & out$routeName=='Budapest to Prague'),]
      #sum(orders[which(orders$Year=='2019' & orders$Month=='05' & orders$routeName=='Budapest to Prague'),]$finalTotalPrice)
      colnames(out)=c("routeName",'DB_Conv','TripPrice')
    }
    
    out=out[order(out$DB_Conv,decreasing = T),]
    head(out)
    if(create.csv==T){
      if(from=='2000-01-01'){from='fromBegining'}
      if(to=='2345-01-01'){to=temp.to}
      if(per.month==T){temp='_per_month'}else{temp=''}
      setwd("//cloud//project//output_files")
      write.csv(out,paste('Routes_frequency_',from,"_",to,temp,'.csv',sep=''),row.names = F)
    }else{return(out)}
    setwd("//cloud//project")
  }
  
  #--------------------------------------ads_summary_per_route_yearly
  ads_summary_per_route_yearly=function(adwordsFile,bingFile,ordersFile,
                                        year,convColumnName='Conversions',consider_direction=F,adjust_nonadvertised_trips=F,exclude_TA=F,create.csv=F,done=T,
                                        add.routes.info=F,
                                        show_only_routes_from_Adwords=F){
    setwd("//cloud//project//input_files")
    cost=subset(campaignInfo_per_route(adwordsFile,bingFile,convColumnName=convColumnName,consider_direction=consider_direction),Year==year)
    head(cost)
    
    trips=route_popularity(ordersFile,from=paste(year,'-01-01',sep = ''),to=paste(year+1,'-01-01',sep = ''),per.month=T,create.csv = F,consider_direction = consider_direction,exclude_TA=exclude_TA,locations=F)
    if(add.routes.info==T){
      route_info=unique(subset(read.csv(ordersFile),select=c("origin_Country","origin_Location")))
    }
    
    if(adjust_nonadvertised_trips==T){
      print(paste('Adjust no. of advertised trips by non-advertised trips.'))
      #find trips that were not advertised
      custom=trips[-which(trips$routeName %in% cost$campaignName),]
      sum(custom$TripPrice)
      #advertised trips will be adjusted by non-advertised ones
      advertised=trips[which(trips$routeName %in% cost$campaignName),]
      sum(advertised$TripPrice)
      
      #split route name back to locations, create vectors loc.custom, loc,advertised
      if(consider_direction==F){
        custom$Loc1=unlist(strsplit(custom$routeName,' - '))[2*(1:nrow(custom))-1]
        custom$Loc2=unlist(strsplit(custom$routeName,' - '))[2*(1:nrow(custom))]
        loc.custom=as.data.frame(table(c(custom$Loc1,custom$Loc2)))
        
        advertised$Loc1=unlist(strsplit(advertised$routeName,' - '))[2*(1:nrow(advertised))-1]
        advertised$Loc2=unlist(strsplit(advertised$routeName,' - '))[2*(1:nrow(advertised))]
        loc.advertised=as.data.frame(table(c(advertised$Loc1,advertised$Loc2)))
        loc.advertised=loc.advertised$Var1[order(loc.advertised$Freq,decreasing = T)]
      }else{
        strsplit(as.character(custom$routeName[5958]), ' to ')
        custom$Loc1=unlist(strsplit(custom$routeName,' to '))[2*(1:nrow(custom))-1]
        custom$Loc2=unlist(strsplit(custom$routeName,' to '))[2*(1:nrow(custom))]
        loc.custom=as.data.frame(table(c(custom$Loc1,custom$Loc2)))
        head(loc.custom[order(loc.custom$Freq,decreasing = T),])
        
        advertised$Loc1=unlist(strsplit(advertised$routeName,' to '))[2*(1:nrow(advertised))-1]
        advertised$Loc2=unlist(strsplit(advertised$routeName,' to '))[2*(1:nrow(advertised))]
        loc.advertised=as.data.frame(table(c(advertised$Loc1,advertised$Loc2)))
        loc.advertised=loc.advertised$Var1[order(loc.advertised$Freq,decreasing = T)]
      }
      #print(paste(sum(custom[-which(custom$Loc1 %in% loc.advertised | custom$Loc2 %in% loc.advertised),]$DB_Conv),'DB conversions cannot be redistributed since origins and locations were not advertised'))
      #sum(custom[which(custom$Loc1=="Cesky Krumlov" | custom$Loc2=="Cesky Krumlov"),]$DB_Conv)
      #sum(advertised[which(advertised$Loc1=="Cesky Krumlov" | advertised$Loc2=="Cesky Krumlov"),]$DB_Conv)
      
      #go trough all advertised locations (from most advertised to the least advertised)
      for(r in loc.advertised){# r="Cesky Krumlov"
        temp.custom=custom[which(r == custom$Loc1 | r == custom$Loc2),] #custom orders for the given advertised location "r"
        
        if(nrow(temp.custom)>0){
          temp.years=sort(unique(temp.custom$Year))
          temp.months=sort(unique(temp.custom$Month))
          for(y in temp.years){ #y='2018'
            for(m in temp.months){#m='06'
              temp.custom.m=temp.custom[which(m==temp.custom$Month & y==temp.custom$Year),]
              #temp.custom.m
              #sum(temp.custom.m$DB_Conv)
              #sum(temp.custom.m$TripPrice)
              
              loc.temp.custom.m=c(temp.custom.m$Loc1,temp.custom.m$Loc2) #check the other location
              loc.temp.custom.m[-which(loc.temp.custom.m %in% loc.advertised)]=r #locations not in advertised are considered to be "r"
              loc.temp.custom.m=as.data.frame(table(loc.temp.custom.m))
              
              #now redistribute the adjusted no. of trips of custom trips to advertised
              redistribute      =sum(temp.custom.m$DB_Conv)  * loc.temp.custom.m$Freq[which(loc.temp.custom.m$loc.temp.custom.m==r)]/sum(loc.temp.custom.m$Freq)
              redistribute.price=sum(temp.custom.m$TripPrice)* loc.temp.custom.m$Freq[which(loc.temp.custom.m$loc.temp.custom.m==r)]/sum(loc.temp.custom.m$Freq)
              index=which((advertised$Loc1 == r | advertised$Loc2 == r) & advertised$Year == y & advertised$Month == m)
              index
              advertised$DB_Conv[index] = advertised$DB_Conv[index] + round(redistribute * advertised$DB_Conv[index]/sum(advertised$DB_Conv[index]),3) #done!
              advertised$TripPrice[index] = advertised$TripPrice[index] + round(redistribute.price * advertised$TripPrice[index]/sum(advertised$TripPrice[index]),3) #done!
            }
          }
        }
      }
      
      #redistribute orders, that DO NOT have origin and location in advertised loc.
      MEGAcustom=custom[which(!(custom$Loc1 %in% loc.advertised | custom$Loc2 %in% loc.advertised)),]
      redistribute=sum(MEGAcustom$DB_Conv)
      redistribute.price=sum(MEGAcustom$TripPrice)
      advertised$DB_Conv   = advertised$DB_Conv +   round(redistribute       * advertised$DB_Conv/sum(advertised$DB_Conv),3) #done!
      advertised$TripPrice = advertised$TripPrice + round(redistribute.price * advertised$TripPrice/sum(advertised$TripPrice),3) #done!
      
      #sum(custom$TripPrice)
      #sum(advertised$TripPrice)
      #redistribute.price
      #(sum(advertised$TripPrice)+redistribute.price-8004585)/8004585
      head(advertised)
      trips=advertised[,1:5]
    }
    
    print(paste('Getting summary/overall numbers from Adwords and Bing.'))
    #save total numbers for adwords
    total.adwords=
      subset(
        subset(
          read.csv(adwordsFile), Status=='' & Campaign.status=='Total: Manager account' & Month!=''), select = c('Month','Cost','Conversions'))#,'Campaign.ID'))
    if(nrow(total.adwords)==0){print("Rows called 'Total: Manager account' are missing in the Adwords file. Please check the input csv file and the account it was downloaded from.")}
    total.adwords$Year=as.character(format(as.Date(paste('01-',as.character(total.adwords$Month),sep=''),format='%d-%y-%B'),'%Y'))
    total.adwords$Month=as.character(format(as.Date(paste('01-',as.character(total.adwords$Month),sep=''),format='%d-%y-%B'),'%m'))
    total.adwords$Cost=as.numeric(gsub(',','',as.character(total.adwords$Cost)))
    total.adwords$Conversions=as.numeric(gsub(',','',as.character(total.adwords$Conversions)))
    total.adwords
    #save total numbers for bing
    total.bing=subset(subset(read.csv(bingFile),Campaign.ID !="-"),select = c('Month','Spend','Conv.'))
    colnames(total.bing)=c('Date','Cost','Conversions')
    total.bing$Month=as.character(format(as.Date(total.bing$Date,format='%d/%m/%Y'),'%m'))
    total.bing$Year=as.character(format(as.Date(total.bing$Date,format='%d/%m/%Y'),'%Y'))
    total.bing$Cost=as.numeric(gsub(',','',as.character(total.bing$Cost)))
    total.bing=total.bing[,-1]
    total.bing=aggregate(.~Month+Year, data = total.bing,sum)
    total.bing
    
    #prepare out table
    print(paste('Detecting and aggregating brand campaigns.'))
    if(show_only_routes_from_Adwords==T){
      out=merge(cost,trips, by.x=c('campaignName','Year','Month'),by.y=c('routeName','Year','Month'),all.x = T)
    }else{
      out=merge(cost,trips, by.x=c('campaignName','Year','Month'),by.y=c('routeName','Year','Month'),all   = T)
    }
    out$DB_Conv[is.na(out$DB_Conv)]=0
    sum(out$DB_Conv)
    head(out)
    
    #brand campaign
    if(consider_direction==F){
      brand=out[ which(out$campaignName=='Daytrip - Daytrip' | out$campaignName=='zAuto - zAuto'),]
      out=  out[-which(out$campaignName=='Daytrip - Daytrip' | out$campaignName=='zAuto - zAuto'),]
    }else{
      brand=out[ which(out$campaignName=='Daytrip to Daytrip' | out$campaignName=='zAuto to zAuto'),]
      out=  out[-which(out$campaignName=='Daytrip to Daytrip' | out$campaignName=='zAuto to zAuto'),]
    }
    head(brand)
    
    for(j in 4:9){brand[,j]=as.numeric(as.character(brand[,j]))}
    
    #aggregate cost and conversions per month and year
    brand=merge(
      aggregate(Cost ~ Month+Year, data=brand[,-1], FUN=sum),
      aggregate(Ads_Conv ~ Month+Year, data=brand[,-1], FUN=sum),by=c('Month','Year'))
    head(out)
    print(paste('Re-distributing brand campaigns cost and conversions.'))
    if(nrow(brand)>0){
      for(i in 1:nrow(brand)){#i=3
        out[16:18,]
        temp.year=brand[i,]$Year
        temp.month=brand[i,]$Month
        temp.cost=brand[i,]$Cost #cost for brand
        temp.conv=brand[i,]$Ads_Conv
        
        total.bing.cost=   total.bing$Cost[which(total.bing$Month==temp.month & total.bing$Year==temp.year)]
        total.adwords.cost=total.adwords$Cost[which(total.adwords$Month==temp.month & total.adwords$Year==temp.year)] 
        total.cost=sum(total.adwords.cost,total.bing.cost,na.rm = T) #ok
        
        total.bing.conv=   total.bing$Conversions[which(total.bing$Month==temp.month & total.bing$Year==temp.year)]
        total.adwords.conv=total.adwords$Conversions[which(total.adwords$Month==temp.month & total.adwords$Year==temp.year)] 
        total.conv=sum(total.adwords.conv,total.bing.conv,na.rm = T) 
        
        condition=which(out$Month==temp.month & out$Year==temp.year)
        
        temp.cost.ratio=out$Cost[condition]/(total.cost-temp.cost) #cost distribution as percentage
        temp.conv.ratio=out$Ads_Conv[condition]/(total.conv-temp.conv) #conv distribution as percentage
        
        out$Cost[condition]     = round( out$Cost[condition]    +(temp.cost*temp.cost.ratio), 3)
        out$Ads_Conv[condition] = round( out$Ads_Conv[condition]+(temp.conv*temp.conv.ratio), 3)
      }
    }
    
    out$DB_CR=round(out$DB_Conv/out$Clicks,3)
    out$DB_CPA=round(out$Cost/out$DB_Conv,3)
    out$TripPrice[is.na(out$TripPrice)]=0
    out$DT_Revenue=0.2*out$TripPrice-out$Cost
    head(out,30)
    route_info
    if(add.routes.info==T){
      split=ifelse(consider_direction," to "," - ")
      getFirst=function(x){return(x[1])}
      out$origin_Location=unlist(lapply(strsplit(out$campaignName,split),getFirst))
      out=merge(out,route_info,by="origin_Location",all.x=T)
    }
    if(done==T){print(paste('DONE :)'))}
    if(create.csv==T){
      setwd("//cloud//project//output_files")
      write.csv(out,paste('Ads_summary_',year,'.csv',sep=''),row.names = F)
      
    }else{return(out)}
    setwd("//cloud//project")
  }  
  
  #--------------------------------------ads_summary_per_country_yearly 
  ads_summary_per_country_yearly=function(adwordsFile,bingFile,ordersFile,countriesFile='countries.csv',
                                          year,convColumnName='Conversions',consider_direction=F,adjust_nonadvertised_trips=T,exclude_TA=F,create.csv=F,done=T){
    
    setwd("//cloud//project//input_files")
    
    cost=subset(campaignInfo_per_country(adwordsFile,bingFile,convColumnName=convColumnName,consider_direction=consider_direction),Year==year)
    cost
    trips=countryToCountry_popularity(ordersFile,from=paste(year,'-01-01',sep = ''),to=paste(year+1,'-01-01',sep = ''),
                                      per.month=T,create.csv = F,consider_direction = consider_direction,exclude_TA=exclude_TA)
    trips
    
    print(paste('Getting summary/overall numbers from Adwords and Bing.'))
    total.adwords=
      subset(
        subset(
          read.csv(adwordsFile), Status=='' & Campaign.status=='Total: Manager account' & Month!=''), select = c('Month','Cost','Conversions'))#,'Campaign.ID'))
    total.adwords
    total.adwords$Year=as.character(format(as.Date(paste('01-',as.character(total.adwords$Month),sep=''),format='%d-%y-%B'),'%Y'))
    total.adwords$Month=as.character(format(as.Date(paste('01-',as.character(total.adwords$Month),sep=''),format='%d-%y-%B'),'%m'))
    total.adwords$Cost=as.numeric(gsub(',','',as.character(total.adwords$Cost)))
    total.adwords$Conversions=as.numeric(gsub(',','',as.character(total.adwords$Conversions)))
    total.adwords
    
    total.bing=subset(subset(read.csv(bingFile),Campaign.ID !="-"),select = c('Month','Spend','Conv.'))
    colnames(total.bing)=c('Date','Cost','Conversions')
    total.bing$Month=as.character(format(as.Date(total.bing$Date,format='%m/%d/%Y'),'%m'))
    total.bing$Year=as.character(format(as.Date(total.bing$Date,format='%m/%d/%Y'),'%Y'))
    total.bing$Cost=as.numeric(gsub(',','',as.character(total.bing$Cost)))
    total.bing=total.bing[,-1]
    total.bing=aggregate(.~Month+Year, data = total.bing,sum)
    total.bing
    
    print(paste('Detecting and aggregating brand campaigns.'))
    out=merge(cost,trips, by.x=c('campaignName','Year','Month'),by.y=c('routeName','Year','Month'),all = T) #nestaci all.x=T? to bolo povodne
    out$DB_Conv[is.na(out$DB_Conv)]=0
    
    if(consider_direction==F){
      brand=out[ which(out$campaignName=='xx - xx' | out$campaignName=='xx - xx'),]
      out=  out[-which(out$campaignName=='xx - xx' | out$campaignName=='xx - xx'),]
    }else{
      brand=out[ which(out$campaignName=='xx to xx' | out$campaignName=='xx to xx'),]
      out=  out[-which(out$campaignName=='xx to xx' | out$campaignName=='xx to xx'),]
    }
    for(i in 4:9){brand[,i]=as.numeric(as.character(brand[,i]))}
    
    brand=merge(
      aggregate(Cost ~ Month+Year, data=brand[,-1], FUN=sum),
      aggregate(Ads_Conv ~ Month+Year, data=brand[,-1], FUN=sum),by=c('Month','Year'))
    
    print(paste('Re-distributing brand campaigns cost and conversions.'))
    for(i in 1:nrow(brand)){#i=1
      temp.year=brand[i,]$Year
      temp.month=brand[i,]$Month
      temp.cost=brand[i,]$Cost #cost for brand
      temp.conv=brand[i,]$Ads_Conv
      
      total.bing.cost=   total.bing$Cost[which(total.bing$Month==temp.month & total.bing$Year==temp.year)]
      total.adwords.cost=total.adwords$Cost[which(total.adwords$Month==temp.month & total.adwords$Year==temp.year)] 
      total.cost=sum(total.adwords.cost,total.bing.cost,na.rm = T) #ok
      
      total.bing.conv=   total.bing$Conversions[which(total.bing$Month==temp.month & total.bing$Year==temp.year)]
      total.adwords.conv=total.adwords$Conversions[which(total.adwords$Month==temp.month & total.adwords$Year==temp.year)] 
      total.conv=sum(total.adwords.conv,total.bing.conv,na.rm = T) 
      
      temp.cost.ratio=out$Cost[    which(out$Month==temp.month & out$Year==temp.year)]/(total.cost-temp.cost) #cost distribution as percentage
      temp.conv.ratio=out$Ads_Conv[which(out$Month==temp.month & out$Year==temp.year)]/(total.conv-temp.conv) #conv distribution as percentage
      
      out$Cost[    which(out$Month==temp.month & out$Year==temp.year)] = round( out$Cost[    which(out$Month==temp.month & out$Year==temp.year)]+(temp.cost*temp.cost.ratio),3)
      out$Ads_Conv[which(out$Month==temp.month & out$Year==temp.year)] = round( out$Ads_Conv[which(out$Month==temp.month & out$Year==temp.year)]+(temp.conv*temp.conv.ratio),3)
    }
    
    out$DB_CR=round(out$DB_Conv/out$Clicks,3)
    out$DB_CPA=round(out$Cost/out$DB_Conv,3)
    head(out)
    out$TripPrice[is.na(out$TripPrice)]=0
    out$DT_Revenue=0.2*out$TripPrice-out$Cost
    
    head(out)
    
    if(done==T){print(paste('DONE :)'))}
    if(create.csv==T){
      setwd("//cloud//project//output_files")
      write.csv(out,paste('Ads_summary_',year,'.csv',sep=''),row.names = F)
      
    }else{return(out)}
    setwd("//cloud//project")
  } 
  
  #--------------------------------------ads_summary_per_route 
  ads_summary_per_route= function(adwordsFile1,bingFile1,adwordsFile2,bingFile2,adwordsFile3, bingFile3, year1=2018,year2=2019,year3=2020,ordersFile,
                                  convColumnName='Conversions',consider_direction=F,adjust_nonadvertised_trips=T,exclude_TA=F,create.csv=F,
                                  show_only_routes_from_Adwords=F,show_all_columns_in_output=T){
    
    setwd("//cloud//project//input_files")
    print(paste("Year:",year1))
    temp2018=ads_summary_per_route_yearly(adwordsFile1,bingFile1,ordersFile,year1,convColumnName=convColumnName,consider_direction=consider_direction,
                                          adjust_nonadvertised_trips=adjust_nonadvertised_trips,done=F,show_only_routes_from_Adwords=show_only_routes_from_Adwords)#,month,create.csv=F)
    print(paste("Year:",year2))
    temp2019=ads_summary_per_route_yearly(adwordsFile2,bingFile2,ordersFile,year2,convColumnName=convColumnName,consider_direction=consider_direction,
                                          adjust_nonadvertised_trips=adjust_nonadvertised_trips,done=F,show_only_routes_from_Adwords=show_only_routes_from_Adwords)#,month,create.csv=F)
    print(paste("Year:",year3))
    temp2020=ads_summary_per_route_yearly(adwordsFile3,bingFile3,ordersFile,year3,convColumnName=convColumnName,consider_direction=consider_direction,
                                          adjust_nonadvertised_trips=adjust_nonadvertised_trips,done=F,show_only_routes_from_Adwords=show_only_routes_from_Adwords)#,month,create.csv=F)
    
    temp2020
    out=merge(merge(temp2018,temp2019, by=c("campaignName","Month"),all=T, suffixes = c(year1,year2)),temp2020,by=c("campaignName","Month"),all=T)
    colnames(out)[25:35]=paste(colnames(out)[25:35],"2020",sep="")
    colnames(out)
    if(show_all_columns_in_output==T){
      out=subset(out,select=c(
      "campaignName","Month",
      "Cost2018","Cost2019","Cost2020",
      "Impr.2018","Impr.2019","Impr.2020",
      "Clicks2018","Clicks2019","Clicks2020",
      "Ads_Conv2018","Ads_Conv2019","Ads_Conv2020",
      "Google_search_impr2018","Google_search_impr2019","Google_search_impr2020",
      "DB_Conv2018","DB_Conv2019","DB_Conv2020",
      "TripPrice2018","TripPrice2019","TripPrice2020",
      "DB_CR2018","DB_CR2019","DB_CR2020",
      "DB_CPA2018","DB_CPA2019","DB_CPA2020",
      "DT_Revenue2018","DT_Revenue2019","DT_Revenue2020"      ))
    }else{
      out=subset(out,select=c(
        "campaignName","Month",
        "Cost2018","Cost2019","Cost2020",
      # "Impr.2018","Impr.2019","Impr.2020",
      # "Clicks2018","Clicks2019","Clicks2020",
      # "Ads_Conv2018","Ads_Conv2019","Ads_Conv2020",
      # "Google_search_impr2018","Google_search_impr2019","Google_search_impr2020",
        "DB_Conv2018","DB_Conv2019","DB_Conv2020",
        "TripPrice2018","TripPrice2019","TripPrice2020",
        "DB_CR2018","DB_CR2019","DB_CR2020",
        "DB_CPA2018","DB_CPA2019","DB_CPA2020",
        "DT_Revenue2018","DT_Revenue2019","DT_Revenue2020"      ))
    }
    
    print(paste('DONE :)'))
  
    if(create.csv==T){
      setwd("//cloud//project//output_files")
      write.csv(out,paste('Ads_summary_',year1,'vs',year2,'vs',year3,'.csv',sep=''),row.names = F)
      
    }else{return(out)}
    setwd("//cloud//project")
  }
  
  #--------------------------------------ads_summary_per_country
  ads_summary_per_country= function(adwordsFile1,bingFile1,adwordsFile2,bingFile2,adwordsFile3,bingFile3,year1=2018,year2=2019,year3=2020,ordersFile,countriesFile='countries.csv',
                                    convColumnName='Conversions',consider_direction=F,exclude_TA=F,create.csv=F,show_all_columns_in_output=T){
    
    setwd("//cloud//project//input_files")
    print(paste("Year:",year1))
    temp2018=ads_summary_per_country_yearly(adwordsFile1,bingFile1,ordersFile,countriesFile,year1,convColumnName=convColumnName,consider_direction=consider_direction,done=F)#,month,create.csv=F)
    print(paste("Year:",year2))
    temp2019=ads_summary_per_country_yearly(adwordsFile2,bingFile2,ordersFile,countriesFile,year2,convColumnName=convColumnName,consider_direction=consider_direction,done=F)#,month,create.csv=F)
    print(paste("Year:",year3))
    temp2020=ads_summary_per_country_yearly(adwordsFile3,bingFile3,ordersFile,countriesFile,year3,convColumnName=convColumnName,consider_direction=consider_direction,done=F)#,month,create.csv=F)
    
    out=merge(merge(temp2018,temp2019, by=c("campaignName","Month"),all=T, suffixes = c(year1,year2)),temp2020,by=c("campaignName","Month"),all=T)
    colnames(out)[25:35]=paste(colnames(out)[25:35],"2021",sep="")
    colnames(out)
    if(show_all_columns_in_output==T){
      out=subset(out,select=c(
        "campaignName","Month",
        "Cost2021","Cost2019","Cost2020",
        "Impr.2021","Impr.2019","Impr.2020",
        "Clicks2021","Clicks2019","Clicks2020",
        "Ads_Conv2021","Ads_Conv2019","Ads_Conv2020",
        "Google_search_impr2021","Google_search_impr2019","Google_search_impr2020",
        "DB_Conv2021","DB_Conv2019","DB_Conv2020",
        "TripPrice2021","TripPrice2019","TripPrice2020",
        "DB_CR2021","DB_CR2019","DB_CR2020",
        "DB_CPA2021","DB_CPA2019","DB_CPA2020",
        "DT_Revenue2021","DT_Revenue2019","DT_Revenue2020"      ))
    }else{
      out=subset(out,select=c(
        "campaignName","Month",
        "Cost2021","Cost2019","Cost2020",
        # "Impr.2021","Impr.2019","Impr.2020",
        # "Clicks2021","Clicks2019","Clicks2020",
        # "Ads_Conv2021","Ads_Conv2019","Ads_Conv2020",
        # "Google_search_impr2021","Google_search_impr2019","Google_search_impr2020",
        "DB_Conv2021","DB_Conv2019","DB_Conv2020",
        "TripPrice2021","TripPrice2019","TripPrice2020",
        "DB_CR2021","DB_CR2019","DB_CR2020",
        "DB_CPA2021","DB_CPA2019","DB_CPA2020",
        "DT_Revenue2021","DT_Revenue2019","DT_Revenue2020"      ))
    }
    print(paste('DONE :)'))
    
    if(create.csv==T){
      setwd("//cloud//project//output_files")
      write.csv(out,paste('Ads_summary_',year1,'vs',year2,'vs',year3,'.csv',sep=''),row.names = F)
      
    }else{return(out)}
    setwd("//cloud//project")
  }
  
