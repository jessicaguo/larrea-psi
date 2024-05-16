### Function to convert VWC into SWP via a lookup table
# Lookup table encodes the Gardner equation with parameters inv.b and log.a
# Inputs are VWC, param, and stat
# VWC should be a scalar or vector of volumetric soil water content expressed as cm^3 cm^-3
# param indicates which Gardner parameters should be used: "loamy sand", "sandy loam"
# stat indicates which SWP should be output: "median", "lower", or "upper"
# Note: "lower" and "upper" denote the central 50th percentile
# Note: function will return "VWC out of range" message and NA as output if VWC < 0.015 or SWC >= 0.45

# Load lookup table
if(file.exists("source/mrc_lookup.Rdata")) {
  load("source/mrc_lookup.Rdata")
} else {
  load("../../source/mrc_lookup.Rdata")
}


vwc2swp <- function(vwc, param = "loamy sand", stat = "median") {
  if(min(vwc) < 0.015 | max(vwc) >= 0.45){
    warning("SWC out of range")
  } 
  
  # if(all(param == "loamy sand")) {
  #   SWP <- lookup[get_inds(vwc), 3:5]
  # } else if (all(param == "sandy loam")) {
  #   SWP <- lookup[get_inds(vwc), 6:8]
  # }
  
  # vectorized form, only 2 options
  col_st <- ifelse(param == "loamy sand", 3, 6)
  col_en <- ifelse(param == "loamy sand", 5, 8)
  SWP <- lookup[get_inds(vwc), unique(col_st):unique(col_en)]
  
  if(stat == "median") {
    return(SWP[,1])
  } else if (stat == "lower") {
    return(SWP[,2])
  } else if (stat == "upper") {
    return(SWP[,3])
  }
}

get_ind <- function(vwc) {
  which.min(abs(lookup$vwc - vwc))
}
get_inds <- Vectorize(get_ind)
