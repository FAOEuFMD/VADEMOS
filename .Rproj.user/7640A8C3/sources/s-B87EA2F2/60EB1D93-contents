#####################################################################################################
## Plots outputs to display 
## hyojung.lee@fao.org - Mar.2021 ~
#####################################################################################################

###################################################
# 0 - Load libraries
###################################################
library(ggplot2)
library(tidyr)
library(dplyr)


# ----------------------------------
# Interactive plot 1: PCP projection
get_plot_pcp <- function(df1, df2, countryname, year_to_predict){
  
  cols <- c("1" = "#ef8125", "2" = "#fccc19", "3" = "#4f8e32", "4" = "#15592b", "above" = "#000000")
  g <- ggplot(df1[df1$Year<2021, ], mapping = aes(x=Year, y=as.integer(PCP), color=factor(PCP)), show.legend=T) +
    geom_point(size=4)+
    geom_point(df2, mapping = aes(x=Year, y=as.integer(PCP), color=factor(PCP)), show.legend=T, size=4, alpha=0.5) +
    ylab(NULL) + #'PCP-FMD'
    expand_limits(y = c(0, NA)) + # yaxis starts from 0
    xlim(2011, as.integer(year_to_predict)) +
    theme_classic(base_size = 15) +
    ggtitle(paste("PCP-FMD")) +
    theme(title = element_text(size=15),
          axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),
          axis.title.x = element_text(size = 18, angle = 0, vjust = .5, face = "plain"),
          axis.title.y = element_text(size = 18, angle = 90, vjust = .5, face = "plain"),
          legend.text=element_text(size=12),
          legend.position="bottom") +
    scale_colour_manual(name = "PCP Stage", values = cols) + # pcp-specific colors, change legend title
    scale_x_continuous(breaks = seq(from = 2010, to = 2030, by = 5))
  print(g)
}

# ----------------------------------
# Interactive plot 2: Livestock population projection
get_plot_pop <- function(df1, df2, countryname, year_to_predict){
  g <- ggplot(data=df1, aes(x=Year, y=Pop, color=Species, shape=Species), show.legend=T) +
    geom_point(size=4) +
    ylab(NULL) +
    geom_point(data=df2[df2$Year>2020, ], aes(x=Year, y=Pop_predicted, color=Species, shape=Species),
               alpha=.4, size=4, show.legend=T) +
    expand_limits(y = c(0, NA)) +
    xlim(2011, as.integer(year_to_predict)) +
    theme_classic(base_size = 15) +
    ggtitle("Livestock (Head)") +
    theme(title = element_text(size=15),
          axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
          axis.title.x = element_text(size = 18, angle = 0, vjust = .5, face = "plain"),
          axis.title.y = element_text(size = 18, angle = 90, vjust = .5, face = "plain"),
          legend.text=element_text(size=12),
          legend.position="bottom") +
    scale_fill_brewer(palette = "Pastel2") +
    scale_x_continuous(breaks = seq(from = 2010, to = 2030, by = 5))
  print(g)
}


# ----------------------------------
# Interactive plot 3: FMD outbreaks projection
get_plot_on <- function(df1, df2, countryname, year_to_predict){
  g <- ggplot(data=df1, aes(x=Year, y=TotalOutbreaks, color=TotalOutbreaks), show.legend=T) +
    geom_point(size=4) +
    ylab(NULL) +
    geom_point(data=df2, aes(x=Year, y=TotalOutbreaks, color=TotalOutbreaks),
               alpha=.4, size=4, show.legend=T) +
    expand_limits(y = c(0, NA)) +
    xlim(2011, as.integer(year_to_predict)) +
    theme_classic(base_size = 15) +
    ggtitle("FMD outbreaks (N)") +
    theme(title = element_text(size=15),
          axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
          axis.title.x = element_text(size = 18, angle = 0, vjust = .5, face = "plain"),
          axis.title.y = element_text(size = 18, angle = 90, vjust = .5, face = "plain"),
          legend.text=element_text(size=12),
          legend.position="bottom") + 
    scale_colour_gradient2(
      low = "black", mid = "grey", high = "red",
      space = "Lab", na.value = "grey50",
      guide = "colourbar", aesthetics = "colour"
    ) +
    scale_x_continuous(breaks = seq(from = 2010, to = 2030, by = 5))
  print(g)
}


# ----------------------------------
# Result plot: vaccine estimates projection (modified by Rob, 16/02/22)
plot_totalvaccine_traj <- function(countrycode, vaccine_estimates){
  if (countrycode != "TUR"){
    p <- ggplot(vaccine_estimates, aes(Year, TotalVaccines/10^6))+
      geom_point()+
      geom_line(data=vaccine_estimates)+
      xlim(2020, 2030) +
      # expand_limits(y = c(0, NA)) +
      geom_ribbon(aes(ymin=CI_low/10^6, ymax=CI_high/10^6), alpha=0.3, fill="#b3bfa4") +
      theme_classic(base_size = 20) +
      theme(plot.title = element_text(size=20, hjust = .5,  vjust = .5, face = "bold" ),
            axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = .5, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
            axis.title.x = element_text(size = 18, angle = 0, vjust = .5, face = "plain"),
            axis.title.y = element_text(size = 18, angle = 90, vjust = .5, face = "plain"),
            legend.text=element_text(size=12),
            legend.position="bottom")+ 
      ggtitle(paste("Predicted Total Vaccines")) +
      xlab("Years") + ylab("Total vaccines (in millions)") +
      scale_x_continuous(breaks = seq(from = 2020, to = 2030, by = 1))+
      scale_y_continuous(n.breaks = 10, expand = expand_scale(mult = .1))
    
    
  }else{
    p <- ggplot(vaccine_estimates, aes(Year, TotalVaccines/10^6))+
      geom_point()+
      geom_line(data=vaccine_estimates) +
      xlim(2020, 2030) +
      # expand_limits(y = c(0, NA)) +
      geom_ribbon(aes(ymin=CI_low/10^6, ymax=CI_high/10^6), alpha=0.3, fill="#b3bfa4") + 
      theme_classic(base_size = 20) +
      theme(plot.title = element_text(size=20, hjust = .5,  vjust = .5, face = "bold" ),
            axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = .5, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
            axis.title.x = element_text(size = 18, angle = 0, vjust = .5, face = "plain"),
            axis.title.y = element_text(size = 18, angle = 90, vjust = .5, face = "plain"),
            legend.text=element_text(size=12),
            legend.position="bottom")+ 
      ggtitle(paste("Predicted Total Vaccines")) +
      xlab("Years") + ylab("Total vaccines (in millions)") +
      scale_x_continuous(breaks = seq(from = 2020, to = 2030, by = 1))+
      scale_y_continuous(n.breaks = 10, expand = expand_scale(mult = .1))
  }
  return(p)
}

# Result plot:  LARGE animals vaccine estimates projection ## Added by Rob (16/02/22)


plot_totalvaccine_LG_traj <- function(mcresult){
  # as.character(max(vaccine_estimates$Year)) -> annoprova
  p <- hist(mcresult()$RVN_LR/10^6, main="", xlab="", ylab="", yaxt = "n", ann = FALSE, col = "lightgoldenrod1")
  # title(main = " Vaccines for Large Ruminants for last year", line = 3.3, font.lab=2, cex.main = 1.3 )
  title(xlab = "Total Vaccines (in millions)", font.lab=2, cex.lab = 1.2, sub = "Blue line = Median, Red lines = CI 0.025 & 0.975")
  abline(v= median(mcresult()$RVN_LR/10^6), col="blue", lwd=2)
  # abline(v = quantile(unmc(RVN_LR)/10^6, 0.025), col = "red", lwd=3, lty=2) # Rob - replace with median with 0.025 uncertainty 28/02/22
  # abline(v = quantile(unmc(RVN_LR)/10^6, 0.975), col = "red", lwd=3, lty=2) # Rob - replace with median with 0.975 uncertainty 28/02/22
  abline(v = quantile(apply(unmc(RVN_LR)/10^6, 1, median), probs = 0.025), col = "red", lwd=3, lty=2) # It's the uncertainty around the median
  abline(v = quantile(apply(unmc(RVN_LR)/10^6, 1, median), probs = 0.975), col = "red", lwd=3, lty=2) # It's the uncertainty around the median
 

  return(p)
} 

# Result plot:  SMALL animals vaccine estimates projection ## Added by Rob (16/02/22)


plot_totalvaccine_SR_traj <- function(mcresult){
  p <- hist(mcresult()$RVN_SR/10^6, main="", xlab="", ylab="", yaxt = "n", ann = FALSE, col = "seagreen1")
  # title(main = "Predicted Vaccines for Small Ruminants for last year", line = 3.3, font.lab=2, cex.main = 1.3 )
  title(xlab = "Total Vaccines (in millions)", font.lab=2, cex.lab = 1.2, sub = "Blue line = Median")
  abline(v= median(mcresult()$RVN_SR/10^6), col="blue", lwd=2)
  # abline(v = quantile(apply(unmc(RVN_SR)/10^6, 1, median), probs = 0.025), col = "red", lwd=3, lty=2) # Rob 01/03/22: Calculate the CI interval around the median (median uncertainty)
  # abline(v = quantile(apply(unmc(RVN_SR)/10^6, 1, median), probs = 0.975), col = "red", lwd=3, lty=2) # Rob 01/03/22: Calculate the CI interval around the median (median uncertainty)
  
  return(p)
}


# Result plot: PIG vaccine estimates projection ## Added by Rob (16/02/22)


plot_totalvaccine_P_traj <- function(mcresult){
  
    if (mean(mcresult()$RVN_P >1)) {
      PV <- mcresult()$RVN_P }
  else {
    PV <- 0
  }
  p <- hist(PV/10^6, main="", xlab="", ylab="", yaxt = "n", ann = FALSE, col = "pink")
  # title(main = "Predicted Vaccines for Pigs for last year", line = 3.3, font.lab=2, cex.main = 1.3 )
  title(xlab = "Total Vaccines (in millions)", font.lab=2, cex.lab = 1.2, sub = "Blue line = Median"  )
  abline(v= median(mcresult()$RVN_P/10^6), col="blue",lwd=2 )
   # abline(v = median(apply(unmc(RVN_P)/10^6, 1, quantile, probs = 0.025)), col = "red", lwd=3, lty=2) # Rob 01/03/22: Calculate the CI interval around the median (median uncertainty)
  # abline(v = median(apply(unmc(RVN_P)/10^6, 1, quantile, probs = 0.975)), col = "red", lwd=3, lty=2) # Rob 01/03/22: Calculate the CI interval around the median (median uncertainty)
  
  return(p)
}

# Result plot: EMERGENCY Vaccines estimates projection ## Added by Rob (16/02/22)


plot_totalvaccine_EV_traj <- function(mcresult){
      if (mean(mcresult()$Total_Emergency_Vaccines >1)) {
      EV <- mcresult()$Total_Emergency_Vaccines }
  else {
    EV <- 0
  }
  p <- hist(EV/10^6, main="", xlab="", ylab="", yaxt = "n", ann = FALSE, col = "steelblue1")
  # hist(mcresult()$Total_Emergency_Vaccines/10^6, main="", xlab="", ylab="", yaxt = "n", ann = FALSE, col = "dark grey")
  # title(main = "Predicted Emergency Vaccines for last year", line = 3.3, font.lab=2, cex.main = 1.3 )
  title(xlab = "Total Vaccines (in millions)", font.lab=2, cex.lab = 1.2, sub = "Blue line = Median, Red lines = CI 0.025 & 0.975")
  abline(v= median(mcresult()$Total_Emergency_Vaccines/10^6), col="blue", lwd=2)
  abline(v = quantile(apply(unmc(Total_Emergency_Vaccines)/10^6, 1, median), probs = 0.025), col = "red", lwd=3, lty=2) # Rob 01/03/22: Calculate the CI interval around the median (median uncertainty)
  abline(v = quantile(apply(unmc(Total_Emergency_Vaccines)/10^6, 1, median), probs = 0.975), col = "red", lwd=3, lty=2) # Rob 01/03/22: Calculate the CI interval around the median (median uncertainty)
  
  
  return(p)
}

