#' Retrieve data from Knoema using Knoema API
#' 
#' Takes datasetid, dimentions, time and frequency and pulls data from knoema.com
#' @param params A list of parameters for filtering data
#' @return list of time series objects
#' @usage knoema("IMFWEO2014Apr?Time=2004-2013&country=1000080&subject=1000010&Frequencies=A")
#' @importFrom httr GET
#' @export

knoema <- function(params){
  
  url = paste("http://knoema.com/api/1.0/data/", params, sep="");
  response = GET(url);
  json = content(response, as="parsed");
  
  ts = list();
  
  for (i in 1:length(json$data)) 
  { 
    frequency = json$data[[i]]$Frequency;
    name = frequency;    
    
    # get name of time series
    for (j in 1:length(json$stub))
    {
      dim = json$stub[[j]]$dimensionId;      
      name =  paste(name, json$data[[i]][[dim]], sep = ' - '); 
    }
    
    # create key-value list where time is the key
    
    if (is.null(ts[[name]]))    
      ts[[name]] = list();   
    
    if (frequency == "A")
      time = format(as.Date(json$data[[i]]$Time), "%Y");
    
    if (frequency == "Q" || frequency == "M")
      time = format(as.Date(json$data[[i]]$Time), "%Y-%m-%d");
        
    ts[[name]][time] = json$data[[i]]$Value;
  };
  
  result = list();
  
  # create list of regular time series objects 
  for (i in 1:length(ts)) 
  {
    title = names(ts[i]);             
    freq = substring(title, 1, 1);         
    time = names(ts[[i]][1]);
    
    if (freq == "A")
    {
      year = as.numeric(time); 
      month = 1;
      freq = 1; 
    }
    
    if (freq == "Q" || freq == "M")    
    {
      year = as.numeric(format(as.Date(time), "%Y")); 
      month = as.numeric(format(as.Date(time), "%m"));   
      freq = ifelse(freq ==  "Q", 4, 12)        
    }     
    
    result[[title]] = ts(ts[[i]], start = c(year, month), frequency = freq) 
  };  
  
  return (result);
}

