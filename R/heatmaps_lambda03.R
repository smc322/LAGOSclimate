library(fuzzyjoin)
suppressMessages(library(dplyr))
library(pheatmap)
library(stringr)
suppressMessages(library(R.utils))
library(viridis)
suppressMessages(library(gridExtra))
library(grid)
suppressMessages(library(magick))

lg_hmap <- function(dt, include_legends = c(1, 2), top_buffer = 0.02, 
                    bottom_buffer = 0.22, right_buffer = 0, left_buffer = 0, 
                    manual_labs = NULL, suppress_cat_key = FALSE){

  col_order <- order(dt$Lon)
  dt_lambda <- select(dt, -lagoslakeid, -Lat, -Lon)
  row.names(dt_lambda) <- dt$lagoslakeid
  dt_lambda <- t(dt_lambda)
  dt_lambda <- dt_lambda[,col_order]
  
  # find banding
  # test <- apply(dt_lambda, 2, function(x) sum(x, na.rm = TRUE))
  # plot(test)
  # abline(v = c(8000, 9000))
  # dt$Lon[c(8000, 9000)]
  # library(sf)
  # mapview::mapview(
  #   st_as_sf(
  #   data.frame(id = c(1, 2), lon = dt$Lon[c(8000, 9000)], lat = dt$Lat[c(8000, 9000)]), 
  #          coords = c("lon", "lat"), crs = 4326))
  
  # define keys #
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
  
  # re-calculate ranks accounting for priorYear
  date_key_prior <- bind_rows(
    mutate(date_key, priorYear = "prior"),
    mutate(date_key, priorYear = "current")) %>%
    mutate(period = factor(period, levels = c("annual", 
                                              "winter", "spring", "summer", "fall",
                                              month.name)), 
           priorYear = factor(priorYear, levels = c("prior", "current")), 
           time = factor(time, levels = c("annual", "seasonal", "monthly"))) %>%
    arrange(time, priorYear) %>%
    mutate(rank = row_number() - 1)
  
  annotation_row <- regex_left_join(annotation_row, date_key_prior, 
                          by = c("names" = "period", 
                                 "timePeriod" = "time",
                                 "priorYear" = "priorYear")) %>%
    dplyr::select(priorYear = priorYear.x, 
                  varType,
                  rank = rank.y,
                  timePeriod,
                  names)
  
    annotation_row[is.na(annotation_row$rank), "rank"] <- 0
    annotation_row[annotation_row$rank == 0 & 
                     annotation_row$priorYear == "current", "rank"] <- 1
  
  # order by vartype and rank ####
    
  row_order <- order(annotation_row$varType,
                     # annotation_row$priorYear,
                     # annotation_row$timePeriod,
                     -rank(annotation_row$rank), 
                     decreasing = TRUE)
    
  annotation_row <- annotation_row[row_order,]
  row.names(annotation_row) <- annotation_row_col_names[row_order] 
  dt_lambda <- dt_lambda[row_order,]
  
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
  var_colors     <- c("#555555", "#989898", "#CACACA")
  # time_colors    <- c("#46024E", "#007393", "#00C387", "#FDE333")
  time_colors    <- rev(var_colors)
  ann_colors = list(
    varType    = c(temperature   = var_colors[3], 
                   precipitation = var_colors[2],
                   index         = var_colors[1]),
    timePeriod = c(annual      =  time_colors[1],
                   seasonal    =  time_colors[2],
                   monthly     =  time_colors[3]))
  
  annotation_row[annotation_row$varType == "drought", "varType"] <- "index"
  annotation_row         <- dplyr::select(annotation_row, -rank, -names)
  annotation_row_ordered <- annotation_row[,c(3, 1, 2)]
  annotation_row_ordered <- dplyr::select(annotation_row_ordered, -priorYear)

  if(length(manual_labs) == 0){ # & any(nchar(manual_labs) > 0)
    labs_save <- labs
    labs <- rep(" ", length(labs))
  }else{
    labs[nchar(manual_labs) > 0] <- manual_labs[nchar(manual_labs) > 0]
    labs_save <- labs
  }
  
  # rename legend keys
  names(annotation_row_ordered) <- c("time scale", "var type")
  names(ann_colors) <- c("var type", "time scale")
  
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
           cellheight = 14, 
           fontsize_row = 13, 
           silent = TRUE, 
           fontsize = 14)
  
  w     <- c(left_buffer, 1.4, 1, 8, 0.3, 2.5, right_buffer) 
  blank <- rectGrob(gp = gpar(col = "white"))
  
  if(length(include_legends) != 0){
    hmap         <- arrangeGrob(
                                blank,
                                raw_hmap$gtable$grobs[[2]],
                                raw_hmap$gtable$grobs[[3]],
                                raw_hmap$gtable$grobs[[1]],
                                blank,
                                raw_hmap$gtable$grobs[[include_legends + 5]],
                                blank,
                                widths = w)
    top_panel <- arrangeGrob(blank, 
                             blank, 
                             blank, 
                             blank, 
                             blank, 
                             blank, 
                             widths = w)
    if(include_legends == 1){
      bottom_panel <- arrangeGrob(blank, 
                                  blank, 
                                  blank,
                                  blank, 
                                  blank, 
                                  blank, 
                                  widths = w)
      res <- arrangeGrob(top_panel, hmap, bottom_panel, 
                  nrow = 3, heights = c(top_buffer, 1, bottom_buffer), top = " ")
      }else{ 
        if(suppress_cat_key){
          # browser()
          # gtable::gtable_show_layout(tp_hmap[[1]])
          w     <- c(left_buffer, 1.4, 1, 8, 0, 0.3, right_buffer) 
          hmap         <- arrangeGrob(
            blank,
            raw_hmap$gtable$grobs[[2]],
            raw_hmap$gtable$grobs[[3]],
            raw_hmap$gtable$grobs[[1]],
            blank,
            blank,
            blank,
            widths = w)
          bottom_panel <- arrangeGrob(blank, 
                                      blank, 
                                      raw_hmap$gtable$grobs[[4]], 
                                      blank, 
                                      blank, 
                                      blank, 
                                      widths = w)
          res <- arrangeGrob(top_panel, hmap, bottom_panel, 
                             nrow = 3, heights = c(top_buffer, 1, bottom_buffer), top = " ")
          
        }else{
          bottom_panel <- arrangeGrob(blank, 
                                      blank, 
                                      raw_hmap$gtable$grobs[[4]], 
                                      blank, 
                                      blank, 
                                      blank, 
                                      widths = w)
          res <- arrangeGrob(top_panel, hmap, bottom_panel, 
                      nrow = 3, heights = c(top_buffer, 1, bottom_buffer), top = " ")
        }
      }
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
    
    res <- arrangeGrob(bottom_panel, hmap, bottom_panel, 
                nrow = 3, heights = c(top_buffer, 1, bottom_buffer), top = " ")
  }
  res <- list(res, labs_save)
  res
}

#### execution block ####

# build single map
# grid.arrange(lg_hmap(readRDS("Data/sec_l3_03.rds"),
#         include_legends = 1,
#         top_buffer = 0.34, bottom_buffer = 0.12, right_buffer = 3.2)[[1]])

# build all maps

# include_legends for hmap itself
sec_hmap <- lg_hmap(readRDS("Data/sec_l3_03.rds"),
                    include_legends = 1,
                    right_buffer = 1.5, bottom_buffer = 0.12, top_buffer = 0.34)
sec_labs <- sec_hmap[[2]]

# include_legends for nothing
chl_hmap <-lg_hmap(readRDS("Data/chl_l3_03.rds"),
              include_legends = NULL,
              top_buffer = 0.34, bottom_buffer = 0.12, left_buffer = 3,
              manual_labs = sec_labs)[[1]]

# include_legends for row categories
tn_hmap <- lg_hmap(readRDS("Data/n_l3_03.rds"), include_legends = 0,
                   right_buffer = 1.5, top_buffer = 0.01, bottom_buffer = 0.5)
tn_labs <- tn_hmap[[2]]

# include_legends for row categories labels without key
tp_hmap <- lg_hmap(readRDS("Data/p_l3_03.rds"),
              include_legends = 0,
              left_buffer = 3, bottom_buffer = 0.5, top_buffer = 0.01,
              manual_labs = tn_labs, suppress_cat_key = TRUE)

# gtable::gtable_show_layout(tp_hmap[[1]])
# plot(tp_hmap[[1]])
# res <- grid.arrange(chl_hmap, sec_hmap[[1]], tn_hmap, tp_hmap[[1]])
res <- arrangeGrob(grobs = list(chl_hmap, sec_hmap[[1]], tp_hmap[[1]], tn_hmap[[1]]), 
                   nrow = 2, ncol = 2, padding = unit(0.1, "line"), clip = "on", 
                   widths = c(2,2))

ggplot2::ggsave(file = "Figures/res.png", plot = res, width = 18.5, 
                height = 17, units = "in")

# Trim and label panels ####
img <- image_read("Figures/res.png")
img <- image_annotate(img, "a. Chlorophyll", size = 90, gravity = "South", location = "-1520+4500")
img <- image_annotate(img, "b. Secchi", size = 90, gravity = "South", location = "+500+4500")
img <- image_annotate(img, "c. TP", size = 90, gravity = "South", location = "-1700+2500")
img <- image_annotate(img, "d. TN", size = 90, gravity = "South", location = "+450+2500")
image_write(image_trim(img), "Figures/res_trim.png")
