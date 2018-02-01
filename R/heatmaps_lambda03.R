library(dplyr)
library(pheatmap)
library(stringr)
library(R.utils)
library(viridis)
library(gridExtra)
library(grid)
library(magick)

lg_hmap <- function(dt, include_legends = c(1, 2), top_buffer = 0.02, 
                    bottom_buffer = 0.22, right_buffer = 0.1, left_buffer = 0){

  dt_lambda <- select(dt, -lagoslakeid, -Lat, -Lon)
  # head(dt_lambda)
  row.names(dt_lambda) <- dt$lagoslakeid
  # hist(colMeans(dt_lambda))
  dt_lambda <- t(dt_lambda)
  
  # define keys ####
  key <- data.frame(rbind(c("tmean", "temperature"),
        c("tmax"  , "temperature"),
        c("tmin"  , "temperature"),
        c("enso"  , "index"),
        c("nao"   , "index"),
        c("ppt"   , "precipitation"), 
        c("precip", "precipitation"), 
        c("palmer", "drought")), 
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
  
  # create row annotation join ####
  annotation_row <- data.frame(stringr::str_split_fixed(
    row.names(dt_lambda), "\\.", 2), stringsAsFactors = FALSE)
  names(annotation_row) <- c("prefix", "suffix")
  annotation_row$suffix <- gsub("\\.x1", "", annotation_row$suffix)
  
  annotation_row$priorYear <- "current"
  annotation_row$priorYear[grep("x1", row.names(dt_lambda))] <- "prior"
  
  annotation_row_intermediate <- dplyr::left_join(
    annotation_row, 
    key, 
    by = c("suffix" = "value"))
  
  names(annotation_row_intermediate)[4] <- "intermediate"
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
  
  # assign annotation_row ids ####
  annotation_row$varType <- apply(annotation_row_intermediate, 
           1, function(x) {
                            if(!is.na(x["intermediate"])){
                              x["intermediate"]
                            }else{
                              x["first_key"]}
                          })
    
  annotation_row$rank <- apply(annotation_row_intermediate, 
                               1, function(x) {
                                 if(!is.na(x["rank.x"])){
                                   x["rank.x"]
                                 }else{
                                   x["rank.y"]}
                               })
  
  annotation_row$timePeriod <- apply(annotation_row_intermediate, 
                               1, function(x) {
                                 if(!is.na(x["time.x"])){
                                   x["time.x"]
                                 }else{
                                   x["time.y"]}
                               })
  annotation_row$timePeriod[is.na(annotation_row$timePeriod)] <- "annual"
  
  annotation_row$rank       <- as.numeric(annotation_row$rank)
  annotation_row            <- select(annotation_row, -prefix, -suffix)
  row.names(annotation_row) <- row.names(dt_lambda)
  annotation_row_col_names  <- row.names(annotation_row)
  annotation_row$names      <- row.names(annotation_row)
  
  # order by vartype and rank ####
  row_order <- order(annotation_row$varType,
                     annotation_row$priorYear,
                     annotation_row$timePeriod,
                     annotation_row$rank, 
                     annotation_row$names,
                     decreasing = c(TRUE, TRUE, TRUE, FALSE, TRUE))
  annotation_row <- dplyr::arrange(annotation_row, 
                                   desc(varType),
                                   desc(priorYear), 
                                   desc(timePeriod),
                                   rank, 
                                   desc(names))
  row.names(annotation_row) <- annotation_row_col_names[row_order] 
  dt_lambda <- dt_lambda[row_order,]
  # data.frame(a = row.names(dt_lambda), b = row.names(annotation_row))
  
  # format labels ####
  top_labs <- names(rowMeans(dt_lambda)[
    order(abs(rowMeans(dt_lambda)), decreasing = TRUE)][1:6])
  labs <- row.names(dt_lambda)
  top_labs <- which(labs %in% top_labs)
  
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
  
  labs[nchar(labs) == 0] <- c("Min", "Mean",
                              "Max", "Min",
                              "Mean", "Max",
                              "Annual", "Annual")
  
  labs[!(seq_len(length(labs)) %in% top_labs)] <- "" # comment out to show all
  # data.frame(a = row.names(dt_lambda), b = labs)
  
  # assign colors ####
  # pal <- choose_palette()
  var_colors     <- c("#555555", "#989898", "#CACACA", "#E2E2E2")
  # time_colors    <- c("#46024E", "#007393", "#00C387", "#FDE333")
  time_colors    <- rev(var_colors)
  ann_colors = list(
    varType    = c(temperature   = var_colors[4], 
                   precipitation = var_colors[3],
                   index         = var_colors[2],
                   drought       = var_colors[1]), 
    timePeriod = c(annual      =  time_colors[2],
                   seasonal    =  time_colors[3],
                   monthly     =  time_colors[4]))
  
  annotation_row <- dplyr::select(annotation_row, -rank, -names)
  annotation_row_ordered <- annotation_row[,c(3, 1, 2)]
  annotation_row_ordered <- dplyr::select(annotation_row_ordered, -priorYear)
  
  # arrange plot ####
  raw_hmap <- pheatmap(dt_lambda, 
           breaks = round(seq(-0.03, 0.03, 
                              length.out = 9), 3),
           color = rev(RColorBrewer::brewer.pal(8, "RdBu")),
           cluster_rows = FALSE,
           cluster_cols = FALSE,
           show_rownames = TRUE,
           show_colnames = FALSE,
           labels_row = labs, 
           annotation_row = annotation_row_ordered,
           annotation_colors = ann_colors,
           cellheight = 8, 
           silent = TRUE, 
           fontsize = 14)
  
  w     <- c(1.4, 1, 8, 0.3, 1.6, 2.1)
  blank <- rectGrob(gp = gpar(col = "white"))
  
  if(length(include_legends) != 0){
    hmap         <- arrangeGrob(
                                raw_hmap$gtable$grobs[[2]],
                                raw_hmap$gtable$grobs[[3]],
                                raw_hmap$gtable$grobs[[1]],
                                blank,
                                raw_hmap$gtable$grobs[[include_legends + 5]],
                                blank,
                                widths = w)
    if(include_legends == 1){
      bottom_panel <- arrangeGrob(blank, 
                                  raw_hmap$gtable$grobs[[4]], 
                                  blank, 
                                  blank, 
                                  blank, 
                                  blank, 
                                  widths = w)
      }else{ 
        bottom_panel <- arrangeGrob(blank, 
                                    blank, 
                                    blank, 
                                    blank, 
                                    blank, 
                                    blank, 
                                    widths = w)
      }
    
    arrangeGrob(hmap, bottom_panel, 
                nrow = 2, heights = c(1, 0.5), top = " ")
  }else{
    w <- c(left_buffer, 1.4, 1, 8, 0.3, 0.1, right_buffer)
    hmap         <- arrangeGrob(blank,
                                raw_hmap$gtable$grobs[[2]], 
                                raw_hmap$gtable$grobs[[3]], 
                                raw_hmap$gtable$grobs[[1]],
                                blank, 
                                blank, 
                                blank, 
                                widths = w)
    
    bottom_panel <- arrangeGrob(blank,
                                blank, 
                                blank, 
                                blank, 
                                blank, 
                                blank, 
                                blank, 
                                widths = w)
    
    arrangeGrob(bottom_panel, hmap, bottom_panel, 
                nrow = 3, heights = c(top_buffer, 1, bottom_buffer), top = " ")
  }
}

#### execution block ####

# set single map
grid.arrange(lg_hmap(readRDS("Data/sec_l3_03.rds"),
        include_legends = 1,
        top_buffer = 0.34, bottom_buffer = 0.12, right_buffer = 3.2))

# build all maps
res <- grid.arrange(
  lg_hmap(readRDS("Data/chl_l3_03.rds"), 
          include_legends = NULL, 
          top_buffer = 0.34, bottom_buffer = 0.12, left_buffer = 3), 
  lg_hmap(readRDS("Data/sec_l3_03.rds"), 
          include_legends = 0, 
          top_buffer = 0.34, bottom_buffer = 0.12, right_buffer = 3.2), 
  lg_hmap(readRDS("Data/n_l3_03.rds"), 
          include_legends = NULL, left_buffer = 3, bottom_buffer = 0.5, 
          top_buffer = 0.01), 
  lg_hmap(readRDS("Data/p_l3_03.rds"), include_legends = 1))

ggplot2::ggsave(file = "Figures/res.png", plot = res, width = 18.5, 
                height = 17, units = "in")

# Trim and label panels ####
img <- image_read("Figures/res.png")
img <- image_annotate(img, "A. chl", size = 90, gravity = "South", location = "-2000+4500")
img <- image_annotate(img, "B. Secchi", size = 90, gravity = "South", location = "+2000+4500")
img <- image_annotate(img, "C. TP", size = 90, gravity = "South", location = "-2000+2500")
img <- image_annotate(img, "D. TN", size = 90, gravity = "South", location = "+2000+2500")
image_write(image_trim(img), "Figures/res_trim.png")

# gtable::gtable_show_layout(hmap)
