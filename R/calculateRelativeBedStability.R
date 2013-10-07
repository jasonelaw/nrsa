#' Calculate several versions of critical diameter: ltest, ldmb_bw4, ldmb_bw5, s_ldmb_bw5 and ldcbf_g08
#' 
#' These functions can be used to calculate several versions of the critical particle
#' diameter:
#' \itemize{
#'  \item a crude estimate via \code{calculateCrudeCriticalDiameter}
#'  \item several more refined ones via \code{calculateRefinedCriticalDiameter}
#'  \item a research based one due to Kauffman 2007 via \code{calculateCriticalDiameterKaufmann}
#' }
#' 
#' \code{calculateRefinedCriticalDiameter} is used to calculate the metrics 
#' ldmb_bw4 (with old.version = T), ldmb_bw5, and s_ldmb_bw5 (when the rp100
#' argument is as returned by \link{calculateS_RP100}).
#' @parm xdepth average depth
#' @param xslope average slope
#' @param rp100 average residual depth
#' @param xbkf_h average bankfull height
#' @param v1w_msq volume lwd  in cubic meters / 100 square meeters
#' @param old.version Calculate old version of logged critical diameter that were based on the
#' wood density when it accidentally used the wetted widths instead of the
#' bankfull widths.
#' @param xbkf_w average bankfull width; must be supplied when old.version = T.
#' @param xwidth average width; must be supplied when old.version = T.
#' @param lsub_dmm mean log10(substrate diameter)
#' @return the log10(critical diameter)
#' @references
#' Kaufmann, P.R. et al., A roughness-corrected index of relative bed stability
#' for regional stream surveys.  Geomorphology (2007)
#' @export
#' @examples
#' library(reshape2)
#'x <- structure(list(UID = c("1", "10", "11", "2", "3", "4", "5", "6", "7", "8", "9"), 
#'                    lsub_dmm = c(1, -1.31451, 1.18991, 1, 1, 1, 0.5408, 3.204, 0.75694, 0.37664, 0.31911), 
#'                    lsub2dmm = c(2, -1.31451, NA, 2, 2, 2, 0.5408, 3.19253, 0.7244, 0.37664, 0.29885), 
#'                    rp100 = c(1, 10.3489, 112.73, 1, 1, 1, 19.0993, 28.3768, 6.4191, 27.2566, 22.2393), 
#'                    sddepth = c(3.95494014, 8.8661, 1.69386, 3.95494014, 3.95494014, 
#'                                3.95494014, 27.7557, 44.2636, 10.4878, 22.6112, 28.7622), 
#'                    v1w_msq = c(1, NA, 0.005818, NA, NA, 0.3, 4e-05, 0.01461, 0.37977, 0.00015, NA), 
#'                    xbkf_h = c(1, 0.33636, 1.47273, 1, 1, 1, 0.75545, 0.61818, 0.3, 0.4, 0), 
#'                    xbkf_w = c(1, 3.718, 255.273, 1, 1, 1, 21.818, 6.673, 2.927, 3.4818, 8.745), 
#'                    xdepth = c(1, 42.67, 3.29555, 1, 1, 1, 51.16, 71.19, 24.007, 31.033, 66.51), 
#'                    xfc_lwd = c(1, 0.00455, 0.00455, 1, 0, NA, 0, 0.01364, 0.11818, 0, 0), 
#'                    xslope = c(1, 0.775, 0.036, 1, 1, 1, 1.22, 9.75, 8.05, 0, 1.57), 
#'                    xwidth = c(1, 3.085, 163.364, 1, 1, 1, 10.0105, 5.255, 2.445, 2.385, 9.1474), 
#'                    PROTOCOL = c("WADEABLE", "WADEABLE", "BOATABLE", "WADEABLE", 
#'                                 "WADEABLE", "WADEABLE", "WADEABLE", 
#'                                 "WADEABLE", "WADEABLE", "WADEABLE", "WADEABLE")), 
#'               .Names = c("UID", "lsub_dmm", "lsub2dmm", "rp100", "sddepth", "v1w_msq", "xbkf_h", 
#'                          "xbkf_w", "xdepth", "xfc_lwd", "xslope", "xwidth", "PROTOCOL"),
#'               row.names = c(NA, -11L), 
#'               class = "data.frame")
#' x$xslope  <- forceSlopeGt0(x$xslope)
#' x$xdepth  <- fixBoatableDepthUnits(x$xdepth, x$PROTOCOL == 'WADEABLE')
#' x$sddepth <- fixBoatableDepthUnits(x$sddepth, x$PROTOCOL == 'WADEABLE')
#' x$s_rp100 <- calculateS_RP100(x$xslope, x$sddepth)
#' x$v1w_msq <- fillMissingLWD(x$v1w_msq, x$xfc_lwd)

#' x$ltest      <- with(x, calculateCrudeCriticalDiameter(xdepth, xslope))
#' x$ldmb_bw5   <- with(x, calculateRefinedCriticalDiameter(xdepth, xslope, rp100, xbkf_h, v1w_msq))
#' x$s_ldmb_bw5 <- with(x, calculateRefinedCriticalDiameter(xdepth, xslope, s_rp100, xbkf_h, v1w_msq))
#' x$ldmb_bw4   <- with(x, calculateRefinedCriticalDiameter(xdepth, xslope, rp100, xbkf_h, v1w_msq, T, xbkf_w, xwidth))
#' x <- cbind(x, with(x, calculateCriticalDiameterKaufmann(xdepth, xslope, rp100, xbkf_h, v1w_msq, lsub_dmm)))
#' x <- cbind(x, with(x, calculateRelativeBedStability(lsub_dmm, lsub2dmm, ltest, ldmb_bw4, ldmb_bw5, s_ldmb_bw5, ldcbf_g08)))
#' metsExpected <- 
#'   structure(list(UID = c("1", "1", "1", "1", "1", "1", "1", "1", 
#'                          "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2", "2", 
#'                          "2", "2", "2", "2", "2", "2", "2", "3", "3", "3", "3", "3", "3", 
#'                          "3", "3", "3", "3", "3", "3", "3", "3", "4", "4", "4", "4", "4", 
#'                          "4", "4", "4", "4", "4", "4", "4", "4", "4", "5", "5", "5", "5", 
#'                          "5", "5", "5", "5", "5", "5", "5", "5", "5", "5", "6", "6", "6", 
#'                          "6", "6", "6", "6", "6", "6", "6", "6", "6", "6", "6", "7", "7", 
#'                          "7", "7", "7", "7", "7", "7", "7", "7", "7", "7", "7", "7", "8", 
#'                          "8", "8", "8", "8", "8", "8", "8", "8", "8", "8", "8", "8", "8", 
#'                          "9", "9", "9", "9", "9", "9", "9", "9", "9", "9", "9", "9", "9", 
#'                          "9", "10", "10", "10", "10", "10", "10", "10", "10", "10", "10", 
#'                          "10", "10", "10", "10", "11", "11", "11", "11", "11", "11", "11", 
#'                          "11", "11", "11", "11", "11", "11", "11"), 
#'                  METRIC = c("ltest", 
#'                             "lrbs_tst", "ldmb_bw5", "s_ldmb_bw5", "ldmb_bw4", "lrbs_bw4", 
#'                             "lrbs_bw5", "s_lrbs_bw5", "lrbs_bw6", "s_lrbs_bw6", "Dcbf_g08", 
#'                             "ldcbf_g08", "lrbs_g08", "s_rp100", "ltest", "lrbs_tst", "ldmb_bw5", 
#'                             "s_ldmb_bw5", "ldmb_bw4", "lrbs_bw4", "lrbs_bw5", "s_lrbs_bw5", 
#'                             "lrbs_bw6", "s_lrbs_bw6", "Dcbf_g08", "ldcbf_g08", "lrbs_g08", 
#'                             "s_rp100", "ltest", "lrbs_tst", "ldmb_bw5", "s_ldmb_bw5", "ldmb_bw4", 
#'                             "lrbs_bw4", "lrbs_bw5", "s_lrbs_bw5", "lrbs_bw6", "s_lrbs_bw6", 
#'                             "Dcbf_g08", "ldcbf_g08", "lrbs_g08", "s_rp100", "ltest", "lrbs_tst", 
#'                             "ldmb_bw5", "s_ldmb_bw5", "ldmb_bw4", "lrbs_bw4", "lrbs_bw5", 
#'                             "s_lrbs_bw5", "lrbs_bw6", "s_lrbs_bw6", "Dcbf_g08", "ldcbf_g08", 
#'                             "lrbs_g08", "s_rp100", "ltest", "lrbs_tst", "ldmb_bw5", "s_ldmb_bw5", 
#'                             "ldmb_bw4", "lrbs_bw4", "lrbs_bw5", "s_lrbs_bw5", "lrbs_bw6", 
#'                             "s_lrbs_bw6", "Dcbf_g08", "ldcbf_g08", "lrbs_g08", "s_rp100", 
#'                             "ltest", "lrbs_tst", "ldmb_bw5", "s_ldmb_bw5", "ldmb_bw4", "lrbs_bw4", 
#'                             "lrbs_bw5", "s_lrbs_bw5", "lrbs_bw6", "s_lrbs_bw6", "Dcbf_g08", 
#'                             "ldcbf_g08", "lrbs_g08", "s_rp100", "ltest", "lrbs_tst", "ldmb_bw5", 
#'                             "s_ldmb_bw5", "ldmb_bw4", "lrbs_bw4", "lrbs_bw5", "s_lrbs_bw5", 
#'                             "lrbs_bw6", "s_lrbs_bw6", "Dcbf_g08", "ldcbf_g08", "lrbs_g08", 
#'                             "s_rp100", "ltest", "lrbs_tst", "ldmb_bw5", "s_ldmb_bw5", "ldmb_bw4", 
#'                             "lrbs_bw4", "lrbs_bw5", "s_lrbs_bw5", "lrbs_bw6", "s_lrbs_bw6", 
#'                             "Dcbf_g08", "ldcbf_g08", "lrbs_g08", "s_rp100", "ltest", "lrbs_tst", 
#'                             "ldmb_bw5", "s_ldmb_bw5", "ldmb_bw4", "lrbs_bw4", "lrbs_bw5", 
#'                             "s_lrbs_bw5", "lrbs_bw6", "s_lrbs_bw6", "Dcbf_g08", "ldcbf_g08", 
#'                             "lrbs_g08", "s_rp100", "ltest", "lrbs_tst", "ldmb_bw5", "s_ldmb_bw5", 
#'                             "ldmb_bw4", "lrbs_bw4", "lrbs_bw5", "s_lrbs_bw5", "lrbs_bw6", 
#'                             "s_lrbs_bw6", "Dcbf_g08", "ldcbf_g08", "lrbs_g08", "s_rp100", 
#'                             "ltest", "lrbs_tst", "ldmb_bw5", "s_ldmb_bw5", "ldmb_bw4", "lrbs_bw4", 
#'                             "lrbs_bw5", "s_lrbs_bw5", "lrbs_bw6", "s_lrbs_bw6", "Dcbf_g08", 
#'                             "ldcbf_g08", "lrbs_g08", "s_rp100"), 
#'                  RESULT = c(-0.164309429, 
#'                             1.164309429, 0.835690571, 0.831325766, -0.164309429, 1.164309429, 
#'                             0.164309429, 0.168674234, 1.164309429, 1.168674234, 113.9277151, 
#'                             2.056629387, -1.056629387, 2, -0.164309429, 1.164309429, NA, 
#'                             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2, -0.164309429, 1.164309429, 
#'                             1.835690571, 1.831325766, 1.835690571, -0.835690571, -0.835690571, 
#'                             -0.831325766, 0.164309429, 0.168674234, NA, NA, NA, 2, -0.164309429, 
#'                             1.164309429, 1.437750563, 1.426755179, 0.437750563, 0.562249437, 
#'                             -0.437750563, -0.426755179, 0.562249437, 0.573244821, 146.4599268, 
#'                             2.165718813, -1.165718813, 2, 1.630980938, -1.090180938, 1.95385339, 
#'                             1.941619453, 1.953783007, -1.412983007, -1.41305339, -1.400819453, 
#'                             -1.41305339, -1.400819453, 105.9303106, 2.025020246, -1.484220246, 
#'                             22.08840009, 2.67711418, 0.52688582, 2.832055426, 2.842904539, 
#'                             2.815915646, 0.388084354, 0.371944574, 0.361095461, 0.360474574, 
#'                             0.349625461, 1588.155449, 3.200893009, 0.003106991, 25.80377156, 
#'                             2.121824344, -1.364884344, 1.418982992, 1.436906832, 0.418982992, 
#'                             0.337957008, -0.662042992, -0.679966832, -0.694582992, -0.712506832, 
#'                             148.8617733, 2.172783188, -1.415843188, 4.41366392, -0.672485667, 
#'                             1.049125667, -0.523367109, -0.768424143, -0.523802116, 0.900442116, 
#'                             0.900007109, 1.145064143, 0.900007109, 1.145064143, 0.32636017, 
#'                             -0.486302849, 0.862942849, 46.120606, 1.854477172, -1.535367172, 
#'                             1.677706613, 1.680788736, 1.677706613, -1.358596613, -1.358596613, 
#'                             -1.361678736, -1.378856613, -1.381938736, NA, NA, NA, 21.92365464, 
#'                             1.355114917, -2.669624917, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
#'                             NA, NA, 5.80084933, 0.909920977, 0.279989023, 0.951821206, -0.080588008, 
#'                             0.949639991, 0.240270009, 0.238088794, 1.2704980082, NA, NA, 
#'                             17.97017283, 1.254552254, -0.064642254, 441.9807675)),
#'             .Names = c("UID", "METRIC", "RESULT"), row.names = c(NA, 154L), class = "data.frame")
#' mets <- melt(subset(x, select = -PROTOCOL), id.var = 'UID', variable.name = 'METRIC', value.name = 'RESULT')
#' res  <- merge(metsExpected, mets, by = c('UID', 'METRIC'), all.x = T)
#' res$diff <- abs(res$RESULT.x - res$RESULT.y)
#' res
calculateRefinedCriticalDiameter <- function(xdepth, xslope, rp100, xbkf_h, 
                                             v1w_msq, old.version = F, xbkf_w = NULL, xwidth = NULL){
  if (old.version){
    stopifnot(!is.null(xbkf_w), !is.null(xwidth))
    Rw        <- v1w_msq * 1000 * xbkf_w / xwidth
    is.na(Rw) <- xwidth <= 0
  } else {
    Rw <- v1w_msq * 1000
  }
  Rbf      <- 0.5 * ((xdepth - rp100) * 10 + xbkf_h * 1000)
  Rbf      <- pmax(0.1 * Rbf, Rbf - Rw)
  critdia  <- 13.7 * Rbf * xslope / 100
  is.na(critdia) <- critdia <= 0
  return(log10(critdia))
}

#' @rdname calculateRefinedCriticalDiameter
#' @export
calculateCrudeCriticalDiameter <- function(xdepth, xslope){
  critdia <- 13.7 * (0.5 * xdepth * 10) * (xslope / 100)
  is.na(critdia) <- critdia <= 0
  return(log10(critdia))
}

#' @rdname calculateRefinedCriticalDiameter
#' @export
calculateCriticalDiameterKaufmann <- function(xdepth, xslope, rp100, xbkf_h, v1w_msq, lsub_dmm){
  rho       <- 998
  rhoSed    <- 2650
  g         <- 9.807
  Dbf_th    <- xbkf_h + (xdepth / 100)
  Rb3       <- 0.65 * Dbf_th
  rp        <- rp100 / 100
  s         <- xslope / 100
  viscosity <- 1.02e-6
  sub_dm    <- 10 ^ (lsub_dmm - 3)# is (10^lsub_dmm) / 1000 because 10^lsub_dmm * 10^-3 = 10^(lsub_dmm - 3)
  
  # Total hydraulic resistance, eqn 13b commented out.  Original parameterization
  # of eqn 13a is used.
  #Ct_rpwd <- 1.21 * (rp^1.08) * ((rp + v1w_msq)^0.638) * (Dbf_th^-3.32)
  reg.mat  <- log10(cbind(10, rp, rp + v1w_msq, Dbf_th))
  reg.pars <- c(0.0835, 1.08, 0.638, -3.32)
  Ct_rpwd <- 10 ^ drop(reg.mat %*% reg.pars)
  
  # Hydraulic resistance due to particles
  Cp3_mill <- pmax(0.002, (1/8) * (2.03 * log10(12.2 * Rb3 / sub_dm))^(-2))

  # Hydraulic resistance due to particles, eqn 10b, with restriction as described for eqn 16
  Rrpw3 <- Rb3 * (pmin(1, Cp3_mill / Ct_rpwd) ^ (1 / 3))
  
  # Reynolds number at bankfull, eqn 14
  ReyP3 <- sqrt(g * Rb3 * s) * sub_dm / viscosity
  
  # Shields parameter, eqn 15a, 15b.  Note 15a uses an abbreviated value of
  # the exponent in the case of small Reynolds numbers
  Shld_Px3 <- ifelse(ReyP3 < 26,
                     0.04 * ReyP3^(-0.24),
                     0.5 * ((0.22 * (ReyP3^(-0.6))) + 0.06 * (10^(-7.7 * (ReyP3^(-0.6))))))
  is.na(Shld_Px3) <- ReyP3 <= 0
  
  # Bed surface particle critical diameter (mm) Dcbf* from eqn 16.
  Dcbf_g08  <- 1000 * (rho * g * Rrpw3 * s) / (Shld_Px3 * (rhoSed - rho) * g)
  ldcbf_g08 <- log10(Dcbf_g08)
  return(cbind(Dcbf_g08, ldcbf_g08))
}

#' Calculate relative bed stability metrics.
#' 
#' Calculates several version of relative bed stability metrics using two versions
#' of substrate diamter and several critical diameter versions.
#' @param lsub_dmm mean log10(substrate diameter)
#' @param lsub2dmm mean log2(substrate diameter)
#' @param ltest critical diameter (crude)
#' @param ldmb_bw4 critical diameter (refined) old version
#' @param ldmb_bw5 critical diameter (refined)
#' @param s_ldmb_bw5 critical diameter (refined) with s_rp100 rather than rp100
#' @param ldcbf_g08 critial diameter from Kauffman 2007
#' @return a matrix of relative bed stability metrics.
#' @export
calculateRelativeBedStability <- function(lsub_dmm, lsub2dmm, ltest, ldmb_bw4, ldmb_bw5, s_ldmb_bw5, ldcbf_g08){
  ld   <- cbind(ltest, ldmb_bw4, ldmb_bw5, s_ldmb_bw5, ldcbf_g08)
  ans1 <- lsub_dmm - ld
  ans2 <- lsub2dmm - ld[, 3:4, drop = F]
  ans  <- cbind(ans1, ans2)
  colnames(ans) <- c('lrbs_tst', 'lrbs_bw4', 'lrbs_bw5', 's_lrbs_bw5', 'lrbs_g08', 'lrbs_bw6', 's_lrbs_bw6')
  progressReport('Finished calculating relative bed stability metrics.')
  return(ans)
}

#' Fill in missing LWD volume.
#' 
#' Fill in missing LWD volume as 0 when no LWD fishcover (i.e., xfc_lwd == 0) otherwise we really can't guess.
#' @param v1w_msq a vector of v1w_msq metrics for each site; m^3 of lwd / m^2 channel
#' @param xfc_lwd a vector of xfc_lwd metrics for each site
#' @return a modified vector of v1w_msq metrics
#' @export
#' @examples
#' fillMissingLWD(NA, 1)
#' fillMissingLWD(NA, 0)
#' fillMissingLWD(3, 1)
fillMissingLWD <- function(v1w_msq, xfc_lwd){
  nas    <- is.na(v1w_msq)
  no.lwd <- xfc_lwd == 0
  v1w_msq[nas & no.lwd] <- 0
  return(v1w_msq)
}

#' Not even sure what this modified metric is...
#' 
#' Wish I knew.
#' @param xslope mean site slope
#' @param sddepth standard deviation of thalweg depth
#' @return the s_rp100 metric
#' @export
calculateS_RP100 <- function(xslope, sddepth){
  k1 <- -0.44767
  k2 <-  1.25381
  k3 <- -0.20675
  s_rp100 <- 10 ^ (k1 + k2 * log10(sddepth) + k3 * log10(xslope))
  return(s_rp100)
}

#' Make slope slightly positive
#' 
#' Make slopes slightly positive if they are less than 0.01.
#' @param slope a vector of slopes
#' @return a vector of slopes with slopes < .01, replaced by 0.01
#' @export
forceSlopeGt0 <- function(slope){
  pmax(0.01, slope)
}

#' Convert boatable depth units from m to cm
#' 
#' Convert boatable depth units from m to cm.
#' @param x a depth based metric measured in m
#' @param x logical, \code{TRUE} if the site was wadeable
#' @return a vector of depth metrics in cm
#' @export
fixBoatableDepthUnits <- function(x, is.wadeable){
  i <- !is.wadeable
  x[i] <- x[i] * 100
  return(x)
}
