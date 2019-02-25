############################################################################################
#' @title  Pull data from meterologic stations in the USCRN network

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description Provide a valid GHCND or WBAN ID for a site in the USCRN, the desired
#' timescale of measurements, and requested begin and end times in POSIX format,
#' and a data frame will be returned to the global environment.
#'
#' @param sid The WBAN ID of a valid USCRN site.
#' @param start_date YYYY-MM-DD hh:mm:ss formated start time, in UTC.
#' @param end_date A YYYY-MM-DD hh:mm:ss formated end time, in UTC.
#' @param temp_agg The time scale of measurements to return.
#' Enter 'monthly', 'daily', 'hourly', or 'subhourly'.
#'
#' @return Data frame of USCRN data.

#' @keywords USCRN, data, process quality, data quality, gaps, commissioning
#' @export
#' @examples
#' \dontrun{
# Example inputs:
#' temp_agg <- "subhourly"
#' sid <- "USW00003047"
#' start_date <- as.POSIXct("2014-04-01 00:00:01", format="%Y-%m-%d %H:%M:%S", tz="UTC")
#' end_date <- as.POSIXct("2015-02-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
#'
#' grabUSCRN(temp_agg = temp_agg, TimeBgn = TimeBgn, TimeEnd = TimeEnd, sid = sid)
#' }
#' @seealso Currently none

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#
##############################################################################################


getUSCRNData = function (temp_agg, sid, start_date, end_date){
  ### FN START OPTIONS
  #print(sid)
  functionStart = Sys.time()
  options(stringsAsFactors = FALSE)
  library(data.table)
  library(stringr)

  ### Handle timescales
  if (!temp_agg %in% c("monthly", "daily", "hourly", "subhourly")) {
    stop("Invalid 'temp_agg'! Please enter one of the following: 'monthly', 'daily', 'hourly', 'subhourly'.")
  }
  start_date = as.POSIXct(start_date,  tz="UTC")
  end_date = as.POSIXct(end_date,  tz="UTC")
  years=seq(substr(start_date, 0, 4), substr(end_date, 0, 4))
  ref.seq=metget:::.make.time.seq(start_date, end_date, temp_agg)

  #### Get table header info
  if (temp_agg == "hourly") {
    header <- readLines("ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/hourly02/HEADERS.txt")
  }else {
    header <- readLines(paste0("ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/",
                               temp_agg, "01/HEADERS.txt"))
  }
  header=unlist(strsplit(x = header[2], split = " "), recursive=F)

  # generate a master.df representing the data we want to get.
  # data returned thru the FTP will merge into this DF
  master.df=data.frame(matrix(nrow = length(ref.seq), ncol = length(header), data = NA))
  colnames(master.df)=header
  master.df$WBANNO=substr(sid, (nchar(sid) - 4), nchar(sid))

  # add date time info, including unique date time rownames
  if(temp_agg %in% c("hourly", "subhourly")){
    master.df$UTC_DATE=gsub(pattern = "-", replacement = "", x = substr(x=ref.seq, start=1, stop=10))
    master.df$UTC_TIME=gsub(pattern = ":", replacement = "", x = substr(x=ref.seq, start=12, stop=16))
    rownames(master.df)=paste0(master.df$UTC_DATE, "T", master.df$UTC_TIME)
  }else if(temp_agg=="daily"){
    master.df$LST_DATE=gsub(pattern = "-", replacement = "", x = substr(x=ref.seq, start=1, stop=10))
    rownames(master.df)=master.df$LST_DATE
  }else if(temp_agg=="monthly"){
    master.df$LST_YRMO=gsub(pattern = "-", replacement = "", x = substr(x=ref.seq, start=1, stop=7))
    rownames(master.df)=master.df$LST_YRMO
  }

  #### Generate the base FTP link
  baseLink=metget:::.make.base.link(temp_agg)

  ##### Extract the station name given the ID
  station.name=metget:::.get.uscrn.name(substr(sid, (nchar(sid) - 4), nchar(sid)))

  # do the downloading. Note that each temp_agg is different in what it needs
  if(temp_agg %in% c("subhourly", "hourly", "daily")){
    raw.data=lapply(years, function(x) .get.data(baseLink = baseLink, station.name=station.name, year = x, temp_agg = temp_agg, header=header))
    is.df.data=unlist(lapply(raw.data, class))=="data.frame"
    if(any(is.df.data)){
      raw.data=raw.data[is.df.data]
      ok.data=unlist(lapply(raw.data, ncol)==max(unlist(lapply(raw.data, ncol))))

      data.df=data.frame(do.call(rbind, raw.data[ok.data]))
      if(temp_agg %in% c("subhourly", "hourly")){
        rownames(data.df)=paste0(data.df$UTC_DATE, "T", data.df$UTC_TIME) #Rownames are datetime
      }else{
        rownames(data.df)=data.df$LST_DATE # Row names are dates
      }
    }else{data.df=NULL} #if none of the data returned are vaild, write noting out
  }else{
    data.df=.get.monthly.data(station.name = station.name, header=header)
    rownames(data.df)=data.df$LST_YRMO #Rownames are year mon
  }

  # Put data in rows common to both master df and data
  common.rows=intersect(x = rownames(master.df), y = rownames(data.df))
  master.df[common.rows,]=data.df[common.rows,]

  return(master.df)
}