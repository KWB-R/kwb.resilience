# oxygen -----------------------------------------------------------------------

#' Simulated Dissolved Oxygen in River Spree
#'
#' Data series "oxygen" are included in kwb.resilience as test data
#' for the use of the package. The included data is referred to in
#' the supplied package tutorial (see vignettes), as well as in the supporting 
#' \href{https://www.researchgate.net/publication/326040304_Quantitative_Beschreibung_der_Resilienz_urbaner_Wassersysteme}{research paper}\cr
#' The data are simulated concentrations of dissolved oxygen (DO) in mg/l for different
#' management scenarios. Simulations and assumptions are described in detail in \href{https://www.researchgate.net/publication/306025705_Impacts_of_combined_sewer_overflows_on_a_large_urban_river_-_Understanding_the_effect_of_different_management_strategies}{Riechel et al. 2016}.\cr
#' 
#' Columns in data.frame:\cr
#' \itemize{
#'   \item timestamp (POSIXct). 
#'   \item S2_storage_2020 (numeric).
#'   \item S3_storage_increase (numeric).
#'   \item S4_red_Imp_Surface (numeric).
#'   \item S5_increase_in_DO (numeric).
#' }
#'
#' @docType data
#' @keywords datasets
#' @name oxygen
#' @usage data(oxygen)
#' @format A data frame with 23425 rows and 5 variables
NULL

# resilience.summary -----------------------------------------------------------

#' Calculate Overall Resilience Index for One or Several Performance Time Series
#'
#' Calculates resilience indices (see \href{https://www.researchgate.net/publication/326040304_Quantitative_Beschreibung_der_Resilienz_urbaner_Wassersysteme}{Matzinger et al. 2018})
#' for entire time series of performance P(t).
#' Failure is defined by acceptable
#' performance Pa and maximal failure Pmax. 
#' Entire time series is considered
#' 
#' @param time_stamp vector containing timestamp (sorted in ascending order)
#' @param Pt vector or data.frame (if several colums) with performance P(t)
#'   (same length as timestamp)
#' @param Pa accpetable performance
#' @param Pmax maximal failure
#' @param evtSepTime "event separation time" in seconds. Maximal allowed time
#'   difference between two consecutive timestamps within the same event.
#' @param signalWidth "signal width" in seconds. Length of time interval that
#'   one timestamp is representing, e.g. 5*60 = 300 if each timestamp
#'   respresents a time interval of five minutes (as e.g. a time series is
#'   recorded on a five minute time scale). This parameter is needed to
#'   calculate event durations.
#'
#' @return Returns data.frame containing one row by time series. Columns are:
#' \itemize{
#'    \item num_events: number of failure events in time series
#'    \item worst_P: P(t) closest to Pmax within time series
#'    \item total_dur: total duration of failure events in seconds
#'    \item total_trec: total recovery time of failure events in seconds
#'    \item mean_trec_percent: trec relative to event duration in per cent, averaged
#'   over all failure events in time series
#'    \item Sev: severity over entire time series (=0 if no exceedance of Pa)
#'    \item Res0: resilience index over entire time series (=1 if no exceedance
#'   of Pa)
#' }
#' 
#' @export
#'
resilience.summary <- function(
  time_stamp, Pt, Pa, Pmax, evtSepTime, signalWidth
)
{
  # Pt is vector or data.frame?
  col_nums <- if (is.vector(Pt)) 1 else dim(Pt)[2]

  # Output format
  summary.table <- data.frame(
    time_series = seq_len(col_nums),
    num_events = 0,
    worst_P = 0,
    total_dur = 0,
    total_trec = 0,
    mean_trec_percent = 0,
    Sev = 0,
    Res0 = 0
  )
  
  # Calculate resilience indices for each time series 
  for (col_num in seq_len(col_nums)) {
    
    Pt_col <- if (col_nums == 1) Pt else Pt[, col_num]
    
    # Calculate severity Sev and resilience index Res0
    summary.table$Sev[col_num] <- resilience.severity(
      time_stamp = time_stamp, 
      Pt = Pt_col, 
      Pa = Pa, 
      Pmax = Pmax
    )
    
    summary.table$Res0[col_num] <- 1 - summary.table$Sev[col_num]
    
    # Test-Variable
    P.testvar <- (Pa - Pt_col) / (Pa - Pmax)
    
    # Fill in worst performance
    index.worst_P <- match(max(P.testvar), P.testvar)
    summary.table$worst_P[col_num] <- Pt_col[index.worst_P]
    
    any.exceed <- any(P.testvar >= 0)
    
    if (any.exceed) {
      
      # Calculate resilience by event if Pa exceeded
      x.events <- resilience.events(
        time_stamp = time_stamp, 
        Pt = Pt_col, 
        Pa = Pa, 
        Pmax = Pmax, 
        evtSepTime = evtSepTime, 
        signalWidth = signalWidth
      )
      
      # Fill in indices
      summary.table$num_events[col_num] <- length(x.events$tBeg)
      summary.table$total_dur[col_num] <- sum(x.events$dur)
      summary.table$total_trec[col_num] <- sum(x.events$trec)
      summary.table$mean_trec_percent[col_num] <- mean(x.events$trec_percent)
    }
  }
  
  summary.table
}

# resilience.events ------------------------------------------------------------

#' Calculate Resilience by Failure Event
#'
#' Calculates resilience indices (see \href{https://www.researchgate.net/publication/326040304_Quantitative_Beschreibung_der_Resilienz_urbaner_Wassersysteme}{Matzinger et al. 2018})
#' for each failure event in time series of performance P(t).
#' Failure is defined by acceptable
#' performance Pa and maximal failure Pmax. 
#' 
#' @param time_stamp vector containing timestamp (sorted in ascending order)
#' @param Pt vector with performance P(t) (same length as timestamp)
#' @param Pa accpetable performance
#' @param Pmax maximal failure
#' @param evtSepTime "event separation time" in seconds. Maximal allowed time
#'   difference between two consecutive timestamps within the same event.
#' @param signalWidth "signal width" in seconds. Length of time interval that
#'   one timestamp is representing, e.g. 5*60 = 300 if each timestamp
#'   respresents a time interval of five minutes (as e.g. a time series is
#'   recorded on a five minute time scale). This parameter is needed to
#'   calculate event durations.
#'
#' @return Returns data.frame containing one row by failure event.
#' First columns are identical to kwb.event::hsEvents. Following columns
#' are additional resilience indices:
#' \itemize{
#' \item Sev: severity by event
#' \item Res0: resilience index by event
#' \item trec: recovery time in seconds
#' \item trec_percent: trec relative to event duration in per cent
#' \item worst_P: P(t) closest to Pmax within event
#' }
#' script is stopped if no failure event with message "Pa never
#'   exceeded"
#'
#'   
#' @importFrom kwb.event hsEvents
#' @export
#'
resilience.events <- function(time_stamp, Pt, Pa, Pmax, evtSepTime, signalWidth)
{
  # Initialise the performance data frame
  P <- init_performance(time_stamp, Pt, Pa, Pmax)

  # Analyse exceedance events (if any)
  if (! any(P$exceeding)) {
    
    stop("Pa never exceeded")
  }
    
  # Continue only with the exceeding rows  
  P.exceed <- P[P$exceeding, ]

  # Define events
  x.events <- kwb.event::hsEvents(
    tseries = P.exceed$timestamp, 
    evtSepTime = evtSepTime, 
    signalWidth = signalWidth
  )
  
  # Add result columns
  x.events$Sev <- 0
  x.events$Res0 <- 0
  x.events$trec <- 0
  x.events$trec_percent <- 0
  x.events$worst_P <- 0
    
  # Calculate resilience indicators by event
    
  for (event in seq_len(nrow(x.events)))  {
    
    # Select values for event
    P.event <- P.exceed[x.events$iBeg[event]:x.events$iEnd[event], ]
    
    # Calculate severity and resilience index Res0
    x.events$Sev[event] <- resilience.severity(
      time_stamp = P.event$timestamp, 
      Pt = P.event$Pt, 
      Pa = Pa, 
      Pmax = Pmax
    )
    
    x.events$Res0[event] <- 1 - x.events$Sev[event]
    
    # Find last occurrence of worst performance (maximal failure) within event
    index.last.worst <- max(which(P.event$testvar == max(P.event$testvar)))
    
    # Output worst P
    x.events$worst_P[event] <- P.event$P_in[index.last.worst]
    
    # Calculate trec and trec_percent
    x.events$trec[event] <- as.numeric(x.events$tEnd[event]) -
      as.numeric(P.event$timestamp[index.last.worst]) 
    
    x.events$trec_percent[event] <- x.events$trec[event] / x.events$dur[event] * 100
  }
  
  x.events[, -(1:2)]
}

# init_performance -------------------------------------------------------------
init_performance <- function(time_stamp, Pt, Pa, Pmax)
{
  # Join vectors to data.frame
  P <- data.frame(timestamp = time_stamp, P_in = Pt)
  
  # Add Test-Variable
  P$testvar <- (Pa - P$P_in) / (Pa - Pmax)

  # Which rows exceed Pa?
  P$exceeding <- (P$testvar >= 0)
  
  # Set P(t)
  P$Pt <- Pa
  
  P$Pt[P$exceeding] <- P$P_in[P$exceeding]
  
  P
}

# resilience.severity ----------------------------------------------------------

#' Calculate Severity
#'
#' calculates severity Sev (see \href{https://www.researchgate.net/publication/326040304_Quantitative_Beschreibung_der_Resilienz_urbaner_Wassersysteme}{Matzinger et al. 2018})
#' of failures for time series of performance P(t). 
#' Entire time period is used, failure is defined by acceptable
#' performance Pa and maximal failure Pmax.
#'
#' @param time_stamp vector containing timestamp (sorted in ascending order)
#' @param Pt vector with performance P(t) (same length as timestamp)
#' @param Pa accpetable performance
#' @param Pmax maximal failure (worst case)
#' @param integral_method either 1 or 2. Switches between two different versions
#'   of integral calculation. Both methods should return the same but method
#'   2 should be much faster when applied to long vectors. Default is method 2.
#' 
#' @return Returns severity integrated over entire time series (one number)
#' @export
#'
resilience.severity <- function(time_stamp, Pt, Pa, Pmax, integral_method = 2)
{
  # Turn time_stamp to numeric for the following calculations
  P <- init_performance(as.numeric(time_stamp), Pt, Pa, Pmax)

  # Calculate function in integral for each time step
  P$integrant <- Pa - P$Pt

  integral <- integrate(P$timestamp, P$integrant, method = integral_method)

  # Calculate ranges  
  P_range <- (Pa - Pmax)
  t_range <- (P$timestamp[nrow(P)] - P$timestamp[1])
  
  # Calculate severity
  1 / P_range * 1 / t_range * sum(integral)
}

# integrate --------------------------------------------------------------------
integrate <- function(timestamps, integrants, method = 1)
{
  # Number of timestamps
  n_timestamps <- length(timestamps)

  if (method == 1) {

    # Integral column
    integral <- rep(0, n_timestamps)
    
    # Calculate integral (backward differences)
    for (i in 2:n_timestamps) {
      
      integral[i] <- mean(integrants[(i-1):i]) * (timestamps[i] - timestamps[i-1])
    }

    # Return the integral    
    integral
    
  } else if (method == 2) {
    
    mean_integrants <- (c(0, integrants[-n_timestamps]) + integrants) / 2
    
    time_differences <- c(0, diff(timestamps))
    
    # Calculate and return the integral
    mean_integrants * time_differences
    
  } else {
    
    stop("method ", method, " not supported")
  }
}
