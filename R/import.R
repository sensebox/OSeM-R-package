#returns senseBox as Spatial Point in a SpatialPointsDataFrame
#ToDo: allow list of senseBox IDs as argument
#' Import senseBox data from OSeM
#'
#' Loads a senseBox from the openSenseMap and converts it to a spatial object.
#'
#' @param senseBoxId a valid senseBox ID
#'
#' @return senseBox as spatial point in a SpatialPointsDataFrame
#' @export
#'
#' @examples
#' importSenseBox("56957f3ab3de1fe0052532da")
#'
importSenseBox <- function(senseBoxId)
{
  if (!requireNamespace("rjson", quietly = TRUE)) {
    stop("rjson needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("sp", quietly = TRUE)) {
    stop("sp needed for this function to work. Please install it.",
         call. = FALSE)
  }
  #get json over HTTP and convert to data.frame
  url <- paste("https://api.opensensemap.org/boxes/", senseBoxId, sep = "")
  jsonData <- rjson::fromJSON(file = url)
  jsonDataFrame <- as.data.frame(jsonData)

  x <- jsonDataFrame[1,"loc.geometry.coordinates"] #long
  y <- jsonDataFrame[2,"loc.geometry.coordinates"] #lat

  #Remove unnecessary and redundant data
  spatialDataFrame <- jsonDataFrame[-c(2),-c(5,6,7,8)] #ToDo: change fixed numbers to column names
  spatialDataFrame$lng <- x
  spatialDataFrame$lat <- y

  #Spatialize it
  sp::coordinates(spatialDataFrame) <- ~lng+lat
  sp::proj4string(spatialDataFrame) <- CRS("+proj=longlat +datum=WGS84")

  return(spatialDataFrame)
}


#ToDo: insert sensor type to list
#' Return info about the sensors of a senseBox
#'
#' This returns all sensor information, in particular the sensor IDs, from a certain senseBox as data.frame
#'
#' @param senseBoxId a valid senseBox ID
#'
#' @return Information about all sensors as data frame
#' @export
#'
#' @examples
#' getSensorsInfo("56957f3ab3de1fe0052532da")
#'
getSensorsInfo <- function(senseBoxId)
{
  if (!requireNamespace("rjson", quietly = TRUE)) {
    stop("rjson needed for this function to work. Please install it.",
         call. = FALSE)
  }
  url <- paste("https://api.opensensemap.org/boxes/", senseBoxId, sep = "")
  jsonData <- rjson::fromJSON(file = url)

  quantity <- lengths(jsonData["sensors"])
  sensorDataFrame <- data.frame(matrix(ncol=4, nrow=quantity))
  colnames(sensorDataFrame) <- c("ID","Phenomenon","Unit","Type")
  sensor_ids <- vector()
  for (i in 1:quantity){
    sensor_ids <- c(sensor_ids, jsonData$sensors[[i]]$`_id`)
    sensorDataFrame[i,1] <- jsonData$sensors[[i]]$`_id`
    sensorDataFrame[i,2] <- jsonData$sensors[[i]]$title
    sensorDataFrame[i,3] <- jsonData$sensors[[i]]$unit
    sensorDataFrame[i,4] <- jsonData$sensors[[i]]$sensorType
  }

  return(sensorDataFrame)
}

#ToDo: if no time intervall given, take last 24h
#' Get sensor data from a registered senseBox of openSenseMap
#'
#'Converts a time series of a senseBox sensor to a SpatialPointsDataFrame
#'
#' @param senseBoxId a valid senseBox ID
#' @param sensorId a sensor ID according to the senseBox ID
#' @param startDate starting point for time interval as ISO8601
#' @param endDate end point for time interval as ISO8601
#'
#' @return Time series of a sensor as SpatialPointsDataFrame
#' @export
#'
#' @examples
#' senseBox <- "56957f3ab3de1fe0052532da"
#' sensor   <- "56957f3ab3de1fe0052532e0"
#' start    <- "2016-11-22"
#' end      <- "2016-12-06"
#' getSensorData(senseBox, sensor, start, end)
#'
getSensorData <- function(senseBoxId, sensorId, startDate, endDate)
{
  if (!requireNamespace("rjson", quietly = TRUE)) {
    stop("rjson needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("sp", quietly = TRUE)) {
    stop("sp needed for this function to work. Please install it.",
         call. = FALSE)
  }
  baseUrl <- "https://api.opensensemap.org/boxes/"

  url <- paste(baseUrl, senseBoxId, sep = "")
  jsonData <- rjson::fromJSON(file = url)
  jsonDataFrame <- as.data.frame(jsonData)
  x <- jsonDataFrame[1,"loc.geometry.coordinates"] #lng
  y <- jsonDataFrame[2,"loc.geometry.coordinates"] #lat

  url <- paste(baseUrl, senseBoxId, "/data/", sensorId, "?format=json&from-date=", startDate, "&to-date=", endDate, sep = "")
  jsonData <- rjson::fromJSON(file = url)
  jsonData <- do.call("rbind", jsonData)

  jsonDataFrame <- as.data.frame(jsonData)
  jsonDataFrame <- jsonDataFrame[,c(2,1)]
  jsonDataFrame$createdAt <- as.POSIXlt(strptime(jsonDataFrame$createdAt, "%Y-%m-%dT%H:%M:%S", tz="Z"))
  jsonDataFrame$value <- as.numeric(jsonDataFrame$value)
  jsonDataFrame$lng <- x
  jsonDataFrame$lat <- y

  #Spatialize it
  sp::coordinates(jsonDataFrame) <- ~lng+lat
  #sp::coordinates(jsonDataFrame) <- ~jsonDataFrame$lng+jsonDataFrame$lat
  sp::proj4string(jsonDataFrame) <- sp::CRS("+proj=longlat +datum=WGS84")

  #plot(do.call("rbind",spatialDataFrame$value), type="l", xaxt="n")
  sensor <- getSensorsInfo(senseBoxId)
  yLabel <- sensor[is.element(sensor$ID, sensorId), ]
  yLabel <- paste(yLabel$Phenomenon," (",yLabel$Unit, ")", sep="")
  plot(jsonDataFrame@data, type="l", xaxt="n", xlab = "", ylab = yLabel)
  axis.POSIXct(1, jsonDataFrame@data$createdAt, format="%d.%m.%y %H:%M")

  return(jsonDataFrame)
}

