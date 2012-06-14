library(MASS)
library(RSQLite)

#bakery, banquet hall, candy store, caterer, coffee shop, day care center (for ages less than 2), day care center (for ages 2 ¡V 6), day care center (combo, for ages less than 2 and 2 ¡V 6 combined), gas station, Golden Diner, grocery store, hospital, long term care center(nursing home), liquor store, mobile food dispenser, restaurant, paleteria, school, shelter, tavern, social club, wholesaler, or Wrigley Field Rooftop.


mean.center <- function(x){
  return(x - mean(x))
}

agg.test <- function(x, threshold) {
  x.table <- table(x)
  return(names(x.table)[x.table > threshold])
}

interVisitTimes <- function(inspections, facility.type) {
  ivt = c()
  #subset inspections
  inspections = inspections[inspections$facility_type == facility.type,
    c("license_no", "inspection_date", "inspection_type", "results")]

  print(dim(inspections))

  licenses <- unique(inspections$license_no)
  for (license in licenses) {
    visits <- inspections[inspections$license_no == license,]
    visits <- visits[order(visits$inspection_date),]    

    visits.num = dim(visits)[1]
    if (visits.num > 2) {
      for (i in 1:(visits.num-1)) {
        inter.visit.time <- (visits[i+1, "inspection_date"]
                             - visits[i, "inspection_date"])
        #if (inter.visit.time > 600) {
        #  print(visits)
        #}
        #if (visits[i, "results"] == "Out of Business" & i != visits.num) {
        #  print(visits)
        #}
        ivt = c(ivt, inter.visit.time)
      }
    }
  }
  return(ivt)
}

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "./inspections.db")

inspections<- dbGetQuery(con, "SELECT * FROM inspections_clean")

last.inspections <- dbGetQuery(con, "SELECT *
    FROM inspections_clean INNER JOIN (SELECT license_no AS ln, MAX(inspection_date) AS last_inspection FROM inspections_clean GROUP BY license_no) ON license_no=ln AND inspection_date = last_inspection WHERE results != 'Out of Business' and facility_type = 'restaurant'")



dbDisconnect(con)

print(dim(inspections))

inspections$inspection_date <- as.Date(inspections$inspection_date)
last.inspections$inspection_date <- as.Date(last.inspections$inspection_date)


ivt = interVisitTimes(inspections, "restaurant")


br = seq(0, 949, 73)
hist(ivt, breaks=br, xaxt="n", ylim=c(0,2000))
axis(1, at=365)

hist(as.numeric(Sys.Date() - last.inspections$inspection_date),
     breaks=br,
     xaxt="n",
     ylim=c(0,2000))
     )
axis(1, at=365)
