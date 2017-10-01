library(dplyr)
library(pheatmap)
library(stringr)
library(R.utils)
library(viridis)



dt <- readRDS("Data/chl_l3_03.rds")
dt_lambda <- select(dt, -lagoslakeid, -Lat, -Lon)
# head(dt_lambda)
row.names(dt_lambda) <- dt$lagoslakeid
# hist(colMeans(dt_lambda))
dt_lambda <- t(dt_lambda)

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

annotation_row$vartype <- apply(annotation_row_intermediate, 
         1, function(x) {
                          if(!is.na(x["intermediate"])){
                            x["intermediate"]
                          }else{
                            x["key"]}
                        })
  
annotation_row <- select(annotation_row, -prefix, -suffix)
row.names(annotation_row) <- row.names(dt_lambda)
annotation_row_col_names <- row.names(annotation_row)

# order by vartype
row_order <- order(annotation_row$vartype, 
                   decreasing = TRUE)
annotation_row <- dplyr::arrange(annotation_row, 
                                 vartype)
row.names(annotation_row) <- annotation_row_col_names[rev(row_order)] 
dt_lambda <- dt_lambda[rev(row_order),]

top_labs <- names(rowMeans(dt_lambda)[
  order(abs(rowMeans(dt_lambda)), decreasing = TRUE)][1:15])
labs <- row.names(dt_lambda)
labs[!(labs %in% top_labs)] <- ""
labs <- gsub("tmin", "", labs)
labs <- gsub("tmax", "", labs)
labs <- gsub("precip", "", labs)
labs <- gsub("index", "", labs)
labs <- gsub("\\.", "", labs)
labs <- gsub("x1", "", labs)
labs <- gsub("tmean", "", labs)
labs <- gsub("ppt", "", labs)
labs <- R.utils::capitalize(labs)
labs[which(nchar(labs) > 6)] <- substring(labs[which(nchar(labs) > 6)], 0, 3)

# rowVars <- function(x) {
#   rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
# }
# labels_row <- rep("", nrow(dt_lambda))
# min_rows <- names(rowVars(dt_lambda)[
#   order(abs(rowVars(dt_lambda)), decreasing = FALSE)][1:100])
# labels_row[as.numeric(min_rows)] <- as.numeric(min_rows)

viridis_sample <- viridis::viridis(4)
ann_colors = list(
  vartype = c(drought = viridis_sample[1],
    index = viridis_sample[2], 
    precipitation = viridis_sample[3],
    temperature = viridis_sample[4]))

test <- pheatmap(dt_lambda, 
         breaks = round(seq(-0.02, 0.02, 
                            length.out = 7), 3),
         color = rev(RColorBrewer::brewer.pal(6, "RdBu")),
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         show_rownames = TRUE,
         show_colnames = FALSE,
         labels_row = labs, 
         labels_col = labels_row,
         annotation_row = annotation_row,
         annotation_colors = ann_colors,
         cellheight = 7, 
         silent = TRUE)

w <- c(0.5, 1, 8, 0.3, 1.3, 1.9)
blank <- rectGrob(gp = gpar(col = "white"))

grid.arrange(test$gtable$grobs[[2]], test$gtable$grobs[[3]], test$gtable$grobs[[1]], blank, test$gtable$grobs[[6]], test$gtable$grobs[[5]], nrow = 1, widths = w)
