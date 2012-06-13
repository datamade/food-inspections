library(MASS)

#bakery, banquet hall, candy store, caterer, coffee shop, day care center (for ages less than 2), day care center (for ages 2 ¡V 6), day care center (combo, for ages less than 2 and 2 ¡V 6 combined), gas station, Golden Diner, grocery store, hospital, long term care center(nursing home), liquor store, mobile food dispenser, restaurant, paleteria, school, shelter, tavern, social club, wholesaler, or Wrigley Field Rooftop.


mean.center <- function(x){
  return(x - mean(x))
}

agg.test <- function(x, threshold) {
  x.table <- table(x)
  return(names(x.table)[x.table > threshold])
}


# Make sure that a license is associated with one and only one
# facility type
testData <- function(inspections) {
  licenses <- unique(inspections$License..)
  for (license in licenses) {
    license.inspections = inspections[inspections$License.. == license, ]
    if (all(license.inspections$Facility.Type == license.inspections$Facility.Type[1]) == FALSE) { 
      print(license.inspections[, c("License..", "Address", "DBA.Name", "AKA.Name", "Facility.Type")])
    }
  }
}

interVisitTimes <- function(inspections, facility.type) {
  ivt = c()
  #subset inspections
  inspections = inspections[inspections$Facility.Type == facility.type,
    c("License..", "date.num", "Inspection.Type", "Results")]

  print(dim(inspections))

  licenses <- unique(inspections$License..)
  for (license in licenses) {
    visits <- inspections[inspections$License.. == license,]
    visits <- visits[order(visits$date.num),]    

    visits.num = dim(visits)[1]
    if (visits.num > 2) {
      print(visits)
      for (i in 1:(visits.num-1)) {
        if (visits[i, "Inspection.Type"] == "Canvass" &
            visits[i, "Results"] == 1 &
            visits[i+1, "Inspection.Type"] == "Canvass") {
          inter.visit.time <- (visits[i+1, "date.num"]
                               - visits[i, "date.num"])
          if (inter.visit.time > 600) {
            print(visits)
          }
          ivt = c(ivt, visits[i+1, "date.num"] - visits[i, "date.num"])
        }
      }
    }
  }
  return(ivt)
}

inspections <- read.csv("./ChicagoFoodInspections.csv")

print(dim(inspections))

inspections <- na.omit(inspections)
  
inspections$Inspection.Date <- as.Date(inspections$Inspection.Date)
inspections$date.num <- as.numeric(inspections$Inspection.Date)

inspections$Facility.Type <- factor(tolower(inspections$Facility.Type))

inspections$Facility.Type[inspections$Facility.Type == "wrigley roof top"] <- "wrigley rooftop"
inspections$Facility.Type[inspections$Facility.Type == "wrigley roof top"] <- "wrigley rooftop"

common.facility.types <- agg.test(inspections$Facility.Type, 100)

inspections$Risk <- as.ordered(inspections$Risk)
inspections$Facility.Type <- factor(inspections$Facility.Type)
inspections$Facility.Type <- relevel(inspections$Facility.Type,
                                     ref="restaurant"
                                     )

inspections$Results <- as.ordered(inspections$Results)

#testData(inspections)
#ivt = interVisitTimes(inspections, "restaurant")
  
             
