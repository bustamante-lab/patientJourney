###############################################################
#
# Package: PatientTrajectories
# Date: Oct 02, 2017
#
###############################################################

require(ggplot2)
require(gridExtra)

events <- read.csv("data/event-code-patient-time-example.csv")
umls <- read.csv("data/UMLS-codes-descriptions.csv", row.names = 1)

#------------------------
#' Create a plot of cohort timelines
#'
#' @param events A dataframe of events for the cohort
#' @param umls A dataframe of umls codes, groupings and colors
#' @return plot
#' @example
#' cohortTimeline(events, umls)
#'
cohortTimeline <- function(events, umls) {
  patients <- unique(events$PatientID)

  plot.list <- list()
  for (patient in patients) {
    #Obtain only the indexes for a particular patient
    patient.indexes <- which(events$PatientID == patient)
    single.patient <- events[patient.indexes, ]

    #Calculate the timeline rows (one row is one year)
    pat <- calculateTimeline(single.patient, umls)
    plot.list <- c(plot.list, list(patientTimeline(pat)))
  }

  #Create the grid of plots
  h <- do.call(grid.arrange, plot.list)

  return(h)
}



#------------------------
#' Calculate the span distances for each event in the plot
#'
#' @param events A dataframe of events for a single patient
#' @param umls A dataframe of umls codes, groupings and colors
#' @return plot
#' @example
#' calculateTimeline(events, umls)
#'
calculateTimeline <- function(patient, umls) {
  patient$year.label <-
    paste("year", ceiling(patient$Time_days / 365), sep = " ")
  patient$year <- ceiling(patient$Time_days / 365)
  patient$color <- umls[as.character(patient$UMLScode), "color"]

  #order by time, adding start and end of segment
  patient <- patient[order(patient$Time_days),]

  start <- c()
  end <- c()
  time.start <- 0
  time.end <- 0
  for (i in 1:length(patient$Time_days)) {
    j <- i
    #In case two events occur at the same time
    while ((patient[j, "Time_days"] == patient[j - 1, "Time_days"]) &&
           (j > 1)) {
      j <- j - 1
    }

    if (j == 1) {
      time.start <- 0
    }
    else{
      if (patient[j, "year"] == patient[j - 1, "year"])
        time.start <- patient[j - 1, "Time_days"] + 1
      else{
        time.start <- 0
      }
    }
    time.end <- patient[i, "Time_days"]


    # change all values to 1-year format
    while (time.start > 365) {
      time.start <- time.start - 365
    }

    while (time.end > 365) {
      time.end <- time.end - 365
    }

    start <- c(start, time.start)
    end <- c(end, time.end)
  }
  patient$start <- start
  patient$end <- end

  # Add next year's event to current year for completion
  pat2 <- patient
  j <- 1
  for (i in 1:(dim(patient)[1] - 1)) {
    if (patient[i, "year"] != patient[i + 1, "year"]) {
      #when the year difference is more than 1
      for (y in patient[i, "year"]:(patient[i + 1, "year"] - 1)) {
        #Event to be added
        pevent <- patient[(i + 1),]
        pevent$end <- 365
        if (y == patient[i, "year"]) {
          pevent$start <- patient$end[i] + 1
        }
        else{
          pevent$start <- 0
        }
        pevent$year <- y
        pevent$year.label <- paste("year", y, sep = " ")
        pat2 <-
          rbind(pat2[1:j,], pevent , pat2[(j + 1):dim(pat2)[1],])
        j <- j + 1
      }
    }
    j <- j + 1
  }

  return(pat2)
}


#------------------------
#' Create a plot for a single patient
#'
#' @param segments A dataframe with patient information and calculated segments
#' @return plot
#' @example
#' patientTimeline(events, umls)
#'
patientTimeline <- function(segments) {

  #Unique labels for years
  years <- unique(segments$year.label)
  years <- years[order(years, decreasing = TRUE)]

  #Get patient ID
  patientID = unique(segments[,1])

  #Get legend labels and colors
  mycolors = as.vector(segments[, "color"])
  mycodes = as.vector(segments[, "UMLScode"])

  #Create the ggplot object
  g <- ggplot(segments) +
    geom_segment(
      aes(
        x = start,
        xend = end,
        y = year.label,
        yend = year.label,
        colour = color
      ),
      size = 14,
      lineend = "butt"
    ) +
    scale_y_discrete(limits = years) +
    ggtitle(paste("Patient ", patientID, sep = "")) +
    scale_colour_manual(values = mycolors,
                        limits = mycolors,
                        label = mycodes) +
    labs(fill = "UMLS grouping") +
    theme(
      #Add a title
      plot.title = element_text(hjust = 0.5, size = 50),
      #Remove elements
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      #Add a border
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        size = 1
      )
    )

  return(g)

}
