library(dplyr)
library(pheatmap)
library(stringr)
library(R.utils)
library(viridis)
library(gridExtra)
library(grid)

# read-in data ####
dt <- readRDS("Data/chl_l3_03.rds")
dt_lambda <- select(dt, -lagoslakeid, -Lat, -Lon)
# head(dt_lambda)
row.names(dt_lambda) <- dt$lagoslakeid
# hist(colMeans(dt_lambda))
dt_lambda <- t(dt_lambda)

# define keys ####
key <- data.frame(rbind(c("tmean", "temperature"),
      c("tmax",  "temperature"),
      c("tmin",  "temperature"),
      c("enso",  "index"),
      c("nao",   "index"),
      c("ppt",   "precipitation"), 
      c("precip","precipitation"), 
      c("palmer","drought")), 
      stringsAsFactors = FALSE)
names(key) <- c("value", "key")

date_key <- data.frame(rbind(c("annual", 0, "annual"), 
                             c("winter", 1, "seasonal"), 
                             c("spring", 2, "seasonal"), 
                             c("summer", 3, "seasonal"), 
                             c("fall",   4, "seasonal")), 
                       stringsAsFactors = FALSE)
names(date_key) <- c("period", "rank", "time")
date_key <- rbind(date_key, 
                  data.frame(
                    period = month.name, 
                    rank = 1:12 + 4, 
                    time = "monthly", stringsAsFactors = FALSE))

# create row annotations ####
annotation_row <- data.frame(stringr::str_split_fixed(
  row.names(dt_lambda), "\\.", 2), stringsAsFactors = FALSE)
names(annotation_row) <- c("prefix", "suffix")
annotation_row$suffix <- gsub("\\.x1", "", annotation_row$suffix)

annotation_row_intermediate <- dplyr::left_join(
  annotation_row, 
  key, 
  by = c("suffix" = "value"))

names(annotation_row_intermediate)[3] <- "intermediate"
annotation_row_intermediate <- dplyr::left_join(
  annotation_row_intermediate, 
  key, 
  by = c("prefix" = "value"))

annotation_row_intermediate <- dplyr::left_join(
  annotation_row_intermediate, 
  date_key, 
  by = c("suffix" = "period"))

names(annotation_row_intermediate)[5] <- "first_key"
annotation_row_intermediate <- dplyr::left_join(
  annotation_row_intermediate, 
  date_key, 
  by = c("prefix" = "period"))

annotation_row$varType <- apply(annotation_row_intermediate, 
         1, function(x) {
                          if(!is.na(x["intermediate"])){
                            x["intermediate"]
                          }else{
                            x["key"]}
                        })
  
annotation_row$rank <- apply(annotation_row_intermediate, 
                             1, function(x) {
                               if(!is.na(x["first_key"])){
                                 x["first_key"]
                               }else{
                                 x["rank"]}
                             })
annotation_row$timePeriod <- apply(annotation_row_intermediate, 
                             1, function(x) {
                               if(!is.na(x["time.x"])){
                                 x["time.x"]
                               }else{
                                 x["time.y"]}
                             })
annotation_row$timePeriod[is.na(annotation_row$timePeriod)] <- "interannual"

annotation_row$rank <- as.numeric(annotation_row$rank)
annotation_row <- select(annotation_row, -prefix, -suffix)
row.names(annotation_row) <- row.names(dt_lambda)
annotation_row_col_names <- row.names(annotation_row)
annotation_row$names <- row.names(annotation_row)

# order by vartype and rank ####
row_order <- order(annotation_row$varType, 
                   annotation_row$rank, 
                   annotation_row$names,
                   decreasing = c(TRUE, FALSE, TRUE))
annotation_row <- dplyr::arrange(annotation_row, 
                                 varType, rank, names)
row.names(annotation_row) <- annotation_row_col_names[rev(row_order)] 
dt_lambda <- dt_lambda[rev(row_order),]

# format labels ####
top_labs <- names(rowMeans(dt_lambda)[
  order(abs(rowMeans(dt_lambda)), decreasing = TRUE)][1:15])
labs <- row.names(dt_lambda)
# labs[!(labs %in% top_labs)] <- ""
labs <- gsub("tmin", "", labs)
labs <- gsub("tmax", "", labs)
labs <- gsub("precip", "", labs)
labs <- gsub("index", "", labs)
labs <- gsub("\\.", "", labs)
labs <- gsub("x1", "", labs)
labs <- gsub("tmean", "", labs)
labs <- gsub("ppt", "", labs)
labs <- gsub("palmer", "", labs)
labs <- gsub("espi", "", labs)
labs <- gsub("annual", "", labs)
labs <- R.utils::capitalize(labs)

labs[which(nchar(labs) >= 6 & labs %in% month.name)] <- substring(
  labs[which(nchar(labs) >= 6 & labs %in% month.name)], 0, 3)

labs[nchar(labs) == 0] <- c(rep("Annual", 2), 
                            "Max", "Max", 
                            "Mean", "Mean",
                            "Min", "Min")
# data.frame(a = row.names(dt_lambda), b = labs)

# assign colors ####
# pal <- choose_palette()
var_colors     <- c("#555555", "#989898", "#CACACA", "#E2E2E2")
time_colors    <- c("#46024E", "#007393", "#00C387", "#FDE333")
ann_colors = list(
  varType    = c(drought       = var_colors[1],
                 index         = var_colors[2], 
                 precipitation = var_colors[3],
                 temperature   = var_colors[4]), 
  timePeriod = c(interannual =  time_colors[1],
                 annual      =  time_colors[2],
                 seasonal    =  time_colors[3],
                 monthly     =  time_colors[4]))


annotation_row <- dplyr::select(annotation_row, -rank, -names)

# arrange plot ####
test <- pheatmap(dt_lambda, 
         breaks = round(seq(-0.02, 0.02, 
                            length.out = 7), 3),
         color = rev(RColorBrewer::brewer.pal(6, "RdBu")),
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         show_rownames = TRUE,
         show_colnames = FALSE,
         labels_row = labs, 
         annotation_row = annotation_row,
         annotation_colors = ann_colors,
         cellheight = 7, 
         silent = TRUE)

w <- c(0.9, 1, 8, 0.3, 1.3, 1.9)
blank <- rectGrob(gp = gpar(col = "white"))

grid.arrange(test$gtable$grobs[[2]], test$gtable$grobs[[3]], test$gtable$grobs[[1]], blank, test$gtable$grobs[[6]], test$gtable$grobs[[5]], nrow = 1, widths = w, heights = rep(1, 1))
