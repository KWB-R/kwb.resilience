# oxygen -----------------------------------------------------------------------

#' Simulated Dissolved Oxygen in River Spree
#'
#' Simulated concentrations of dissolved oxygen (DO) in mg/l for different
#' scenarios:\cr
#' S2: storage as it exists in year 2020\cr
#' S3: increased storage compared to the storage in year 2020\cr
#' S4: reduced impervious area\cr
#' S5: increased DO (what does that mean?)
#' 
#' \itemize{
#'   \item timestamp (POSIXct). Timestamp
#'   \item S2_storage_2020 (numeric). D
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
#' calculates resilience indices (see Matzinger et al. 2018)
#' for entire time series of performance P(t)
#' failure is defined by acceptable
#' performance Pa and maximal failure Pmax 
#' entire time series is considered
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
#' @return data.frame containing one row by time series
#' @return num_events: number of failure events in time series
#' @return worst_P: P(t) closest to Pmax within time series
#' @return total_dur: total duration of failure events in seconds
#' @return total_trec: total recovery time of failure events in seconds
#' @return mean_trec_percent: trec relative to event duration in %, averaged
#'   over all failure events in time series
#' @return Sev: severity over entire time series (=0 if no exceedance of Pa)
#' @return Res0: resilience index over entire time series (=1 if no exceedance
#'   of Pa)
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
#' calculates resilience indices (see Matzinger et al. 2018)
#' for each failure event in time series of performance P(t)
#' failure is defined by acceptable
#' performance Pa and maximal failure Pmax 
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
#' @return data.frame containing one row by failure event
#' @return first columns are identical to kwb.event::hsEvents
#' @return Sev: severity by event
#' @return Res0: resilience index by event
#' @return trec: recovery time in seconds
#' @return trec_percent: trec relative to event duration in %
#' @return worst_P: P(t) closest to Pmax within event
#' @return script is stopped if no failure event with message "Pa never
#'   exceeded"
#'   
#' @importFrom kwb.event hsEvents
#' @export
#'
resilience.events <- function(time_stamp, Pt, Pa, Pmax, evtSepTime, signalWidth)
{
  # Join vectors to data.frame
  P <- data.frame(timestamp = time_stamp, P_in = Pt)
  
  # Add Test-Variable
  P$testvar <- (Pa - P$P_in) / (Pa - Pmax)
  
  # Any rows exceed Pa?
  any.exceed <- any(P$testvar >= 0)
  
  # Analyse exceedance events (if any)
  
  if (! any.exceed) {
    
    stop("Pa never exceeded")
  }
    
  # Which rows exceed Pa?
  index.exceed <- which(P$testvar >= 0)
    
  # Set P(t)
  P$Pt <- Pa 
  P$Pt[index.exceed] <- P$P_in[index.exceed]
  P.exceed <- P[index.exceed, ]
    
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
    
    # Find worst performance in event
    worst_testvar <- max(P.event$testvar) 
    indices.worst <- which(P.event$testvar == worst_testvar)
    
    # Last occurrence of maximal failure within event
    index.last <- max(indices.worst)
    
    # Output worst P
    x.events$worst_P[event] <- P.event$P_in[index.last]
    
    # Calculate trec and trec_percent
    x.events$trec[event] <- as.numeric(x.events$tEnd[event]) -
      as.numeric(P.event$timestamp[index.last]) 
    
    x.events$trec_percent[event] <- x.events$trec[event] / x.events$dur[event] * 100
  }
  
  x.events[, -(1:2)]
}
  
# resilience.severity ----------------------------------------------------------

#' Calculate Severity
#'
#' calculates severity Sev (see Matzinger et al. 2018)
#' of failures for time series of performance P(t) 
#' entire time period is used, failure is defined by acceptable
#' performance Pa and maximal failure Pmax
#'
#' @param time_stamp vector containing timestamp (sorted in ascending order)
#' @param Pt vector with performance P(t) (same length as timestamp)
#' @param Pa accpetable performance
#' @param Pmax maximal failure (worst case)
#' @param integral_method either 1 or 2. Switches between two different versions
#'   of integral calculation. Both methods should return the same but method
#'   2 should be much faster when applied to long vectors
#' 
#' @return severity integrated over entire time series (one number)
#' @export
#'
resilience.severity <- function(time_stamp, Pt, Pa, Pmax, integral_method = 1)
{
  # Join vectors to data.frame, time_stamp is turned numeric for following
  # calculations
  P <- data.frame(timestamp = as.numeric(time_stamp), P_in = Pt)
  
  # Add Test-Variable
  P$testvar <- (Pa - P$P_in) / (Pa - Pmax)
  
  # Which rows exceed Pa?
  test_is_positive <- (P$testvar >= 0)
    
  # Set P(t) (also works if index.exceed = integer (0))
  P$Pt <- Pa 
  P$Pt[test_is_positive] <- P$P_in[test_is_positive]
  
  # Calculate function in integral for each time step
  P$integrant <- Pa - P$Pt

  P$integral <- integrate(P$timestamp, P$integrant, method = integral_method)
  
  # Calculate severity
  A <- 1 / (Pa - Pmax)
  B <- 1 / (P$timestamp[nrow(P)] - P$timestamp[1])
  
  A * B * sum(P$integral)
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
    
  } else if (method == 2) {
    
    last_integrants <- c(0, integrants[-n_timestamps])
    integral <- (integrants + last_integrants) / 2 * c(0, diff(timestamps))
    
  } else {
    
    stop("method ", method, " not supported")
  }
  
  integral
}
