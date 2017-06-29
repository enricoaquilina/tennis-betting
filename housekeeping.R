# install.packages(pkgs='plyr')
# install.packages('data.table')
library(plyr)
library(data.table)
args<-commandArgs(TRUE)

# this will store the temporarily unzipped files
# temppath<-'/home/enrico/Desktop/sds/temp/'
temppath<-paste(args[1], '/temp/',sep='')

# Set temporary directory where we later unzip the messy data
# tempcsv='/home/enrico/Desktop/sds/temp/csv'
tempcsv<-paste(args[1],'/temp/csv',sep='')

# this will store the processed files
# processedpath='/home/enrico/Desktop/sds/processed/'
processedpath<-paste(args[1],'/processed/',sep='')

# read unzipped, raw files from this path
# readpath='/home/enrico/Desktop/sds/data/'
readpath<-paste(args[1],'/data/',sep='')

# year<-2012
year<-args[2]

# Make sure to create the directories
dir.create(temppath, showWarnings = FALSE)
dir.create(tempcsv, showWarnings = FALSE)
dir.create(processedpath, showWarnings = FALSE)

# Set working directory 
setwd(tempcsv)

out.file<-""
# get zipped file names from the path
file.names<-dir(readpath,pattern=".zip")

# loop through zipped file names and extract data
for(i in 1:length(file.names))
{
  # Get the path of the file we're currently iterating through
  zippath<-paste(readpath,file.names[i],sep="")
  # Extract the file into the temp path 
  extractedpath<-paste(temppath,file.names[i],sep="")
  # Change working directory and Extract the file
  setwd(temppath)
  unzip(zipfile = zippath, exdir = extractedpath)
  setwd(tempcsv)
  
  # Get all the zip files in the current file we're working with
  filenames<-list.files(paste(extractedpath,'data',sep="/"),pattern = '*.zip|*.rar' )
  # Loop through each zipped file
  for(j in 1: length(filenames))
  {
    # Form the location of the zipped file
    zipcsv<-paste(extractedpath,'data',filenames[j],sep='/')
    # Unzip the zipped file into the temporary csv path
    system(paste('7z x', zipcsv))
    
    # Get the extracted file, be it .csv or .txt
    extracteddata<-list.files(tempcsv,pattern = '*.csv|*.txt' )
    
    if(length(extracteddata)>0){
      # Check  if file is csv or txt and read accordingly
      if(length(grep('.csv', extracteddata))==1){
        raw<-read.csv(paste(tempcsv, extracteddata,sep='/'),header=TRUE,sep=',')  
      }else{
        raw<-read.csv(paste(tempcsv, extracteddata,sep='/'),header=TRUE,sep='\t')  
      }
    }else{
      extractedfolder<-list.files(tempcsv)
      if(length(extractedfolder)>0){
        datafile<-list.files(extractedfolder,pattern = '*.csv|*.txt' )
        temppathTemp<-paste(tempcsv,extractedfolder,sep='/')
        extracteddatafile<-paste(temppathTemp,datafile,sep='/')
        if(length(grep('.csv', datafile))==1){
          raw<-read.csv(extracteddatafile,header=TRUE,sep=',')  
        }else{
          raw<-read.csv(extracteddatafile,header=TRUE,sep='\t')  
        }
      }
    }
    
    # Build a data frame consisting of the needed info
    # This is a check for otherdata.csv which have the actual_off column named differently
    if(length(extracteddata)>0 & regexpr('otherdata',tolower(extracteddata))[1]==1){
      sportFrame<-data.frame(raw$SPORTS_ID,raw$FULL_DESCRIPTION,
                             raw$EVENT,raw$SELECTION,raw$ODDS,
                             raw$NUMBER_BETS,raw$VOLUME_MATCHED,
                             raw$LATEST_TAKEN,raw$FIRST_TAKEN,
                             raw$WIN_FLAG,raw$ACTUAL_OFF,raw$IN_PLAY)
      
    }else{
      sportFrame<-data.frame(raw$SPORTS_ID,raw$FULL_DESCRIPTION,
                             raw$EVENT,raw$SELECTION,raw$ODDS,
                             raw$NUMBER_BETS,raw$VOLUME_MATCHED,
                             raw$LATEST_TAKEN,raw$FIRST_TAKEN,
                             raw$WIN_FLAG,raw$DT.ACTUAL_OFF,raw$IN_PLAY)
    }

    # We need tennis data and pre-event matches and games that went into play
    sportFrame<-subset(sportFrame, sportFrame$raw.SPORTS_ID==2 & sportFrame$raw.IN_PLAY == 'PE' & sportFrame$raw.DT.ACTUAL_OFF!='')
 
    # We also need Australian Open matches only
    ptn = paste('^Group A/Australian Open',year,'/Mens Tournament/.*?',sep='')
    
    # We match 'Australian Open' against the FULL_Description column
    ndx=grep(ptn, sportFrame$raw.FULL_DESCRIPTION,perl=T)
    selected = sportFrame[ndx,]
    
    #remove Doubles matches
    temp = data.table(selected)
    selected = temp[!raw.FULL_DESCRIPTION %like% 'Doubles Matches']
    
    selected<-selected[with(selected,order(raw.FULL_DESCRIPTION,raw.EVENT,raw.LATEST_TAKEN)),]
    
    # If there is data, write it
    if(nrow(selected)>0){
      write.csv(selected,file=paste(processedpath,'Australian_Open_',i,'_',j,'.csv',sep=""))  
    }
    # Remove processed file from system
    unlink(paste(tempcsv,'/*',sep = ''), recursive = TRUE)
  }
  # Remove obsolete files
  unlink(temppath, recursive = TRUE)
  dir.create(temppath, showWarnings = FALSE)
  dir.create(tempcsv, showWarnings = FALSE)
}
