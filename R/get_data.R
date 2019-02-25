############################################################################################
#' @title  Pull data for a site found with metScanR

#' @author Robert Lee \email{rlee@battelleecology.org}\cr
#' Josh Roberti\cr

#' @description This function takes an input of the site metadata from a metScanR search,
#' as well as start and end dates to download data for, and downloads
#'
#' @param site_meta a metScanR list element.
#' @param start_date A YYYY-MM-DD hh:mm:ss formated start time
#' @param end_date A YYYY-MM-DD hh:mm:ss formated end time
#' @param temp_agg The temporal agregation of the data (or period of reporting).
#'  Can be one of "monthly","daily", "hourly", or "subhourly".
#' @param token Optional, but required for Mesonet stations. See: \url{https://developers.synopticdata.com/mesonet/} for more information.
#'
#' @return Data frame data for the input site found with metScanR, if available.

#' @keywords USCRN, data, process quality, data quality, gaps, commissioning
#' @export
#' @examples
#' \dontrun{
#' # Get Data for one SCAN site near CPER
#'cper_sites=metScanR::getNearby(siteID="NEON:CPER", radius = 30)

#' uscrn_out=metDownloadR::getData(site_meta = cper_sites$USW00094074,
#'                               start_date = "2018-10-01",
#'                              end_date = "2018-10-31", 
#'                              temp_agg="monthly")

#' # Get all october 2018 data from sites within a 5 km radius of CPER
#' out=lapply(
#' metScanR::getNearby(siteID="NEON:CPER", radius = 30),
#' metDownloadR::getData,
#' start_date = "2018-10-01",
#' end_date = "2018-10-31", 
#' temp_agg="daily")
#' }
#' @seealso Currently none

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#   Josh Roberti (2018-12-17)
#     logic changes, reordering of sites and option to only grab data for N sites
##############################################################################################


getData<-function(site_meta, start_date, end_date, temp_agg, token=NA){
options(stringsAsFactors = FALSE)
  resTrackFilter=data.frame()
  #set out to no data right off the bat:
  out<-"NO DATA"
  #check to make sure temp_agg is appropriate
  
  if(!(tolower(temp_agg) %in% c("monthly","daily","hourly","subhourly"))){
    stop("temp_agg must equal 'monthly', 'daily', 'hourly', or 'subhourly'")
  }
  temp_agg=tolower(temp_agg)
  
  platform<-site_meta$platform
  identifiers<-site_meta$identifiers
  
  #make dataframe with site's platform and identifiers:
  stationMeta<-data.frame(platform,identifiers)
  
  #filter the resTracking to rows where the platform and temp_agg have a match (map be multiple)
  if(any(stationMeta$platform=="Mesonet")){
    resTrackFilter<-resTracking[which(resTracking$platform==site_meta$platform & resTracking$value=="nominal"),]
  }else{
    resTrackFilter<-resTracking[which(resTracking$platform==site_meta$platform & resTracking$value==temp_agg),]
  }

  #merge resTrackFilter with station identifiers:

  if(nrow(resTrackFilter)>0){
    mergedMeta<-merge(stationMeta,resTrackFilter,by=intersect(names(stationMeta),names(resTrackFilter)))

    #fget unique R packages in case it's repeating:
    useFuncR<-unique(mergedMeta$Rpackage)
    #logic to pull data
    if(length(useFuncR)!=0){ 
      if(length(useFuncR)==1){
        useTheseMeta<-mergedMeta[1,]
      }else if(length(useFuncR)>=1){#example site: USW00094074 (has IDs that could be used with ACIS or USCRN)
        #find the one with the more returns:
        useFuncR<-useFuncR[which.max(table(mergedMeta$Rpackage))]
        useTheseMeta<-mergedMeta[grep(useFuncR,mergedMeta$Rpackage)[1],]
      }
      #older logic using platform:
      # if(platform=="COOP"){
      #   sid<-useTheseMeta$id
      #   out<-getSingleACIS(sid = sid, start_date = start_date, end_date = end_date)
      # }
      #grab the first row of the mergedData (just want to run data pull function once per station, not per ID)
      if(useTheseMeta$Rpackage=="ACIS"){
        sid<-useTheseMeta$id
        out<-getACISData(sid = sid, start_date = start_date, end_date = end_date)
      }
      else if(useTheseMeta$Rpackage=="RNRCS"){
        if(useTheseMeta$idType=="SCAN"){
          idType<-site_meta$identifiers$idType[!grepl(x=site_meta$identifiers$idType, pattern = "Mesonet")]
          sid<-site_meta$identifiers[site_meta$identifiers$idType==idType, "id"]
          out<-RNRCS::grabNRCS.data(site_id = sid, network = idType, timescale = temp_agg, DayBgn = start_date, DayEnd =  end_date)
        }else if(useTheseMeta$idType=="BOR"){
          sid<-gsub(pattern = "BOR:", replacement = "", x = site_meta$identifiers[site_meta$identifiers$idType=="BOR", "id"])
          out<-RNRCS::grabBOR.data(site_id = sid, timescale = temp_agg, DayBgn = start_date, DayEnd =  end_date)
        }
      }
      else if(useTheseMeta$Rpackage=="USCRN"){
        sid<-useTheseMeta$id
        out<-getUSCRNData(sid = sid, temp_agg = temp_agg, start_date = start_date, end_date = end_date)
      }
      else if(useTheseMeta$Rpackage=="Mesonet"){
        out="Error- please input a Mesonet API token. See: https://developers.synopticdata.com/mesonet/ "
        if(!is.na(token)){
          #assign mesonet ID:
          mesoId=useTheseMeta$id
          #collapse the time stamps into strings without separators; need this for mesonet API
          timeBgn<-metget:::mesoTime(start_date)
          timeEnd<-metget:::mesoTime(end_date)
          # print mesonetID to end-user:
          # print(mesoId)
          out<-getMesonetData(token=token,mesoId=mesoId,timeBgn=timeBgn,timeEnd=timeEnd)
         }
      }
      ### input helper function to clean missing data rows out.
      if(class(out)=="data.frame"){
        out=.remove.nas(out)
      }
    }
  }
  return(out)
}

