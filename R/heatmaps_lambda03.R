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

annotation_col <- data.frame(stringr::str_split_fixed(
  colnames(dt_lambda), "\\.", 2), stringsAsFactors = FALSE)
names(annotation_col) <- c("prefix", "suffix")
annotation_col$suffix <- gsub("\\.x1", "", annotation_col$suffix)

annotation_col_intermediate <- dplyr::left_join(
  annotation_col, 
  key, 
  by = c("suffix" = "value"))

names(annotation_col_intermediate)[3] <- "intermediate"
annotation_col_intermediate <- dplyr::left_join(
  annotation_col_intermediate, 
  key, 
  by = c("prefix" = "value"))

annotation_col$vartype <- apply(annotation_col_intermediate, 
         1, function(x) {
                          if(!is.na(x["intermediate"])){
                            x["intermediate"]
                          }else{
                            x["key"]}
                        })
  
annotation_col <- select(annotation_col, -prefix, -suffix)
row.names(annotation_col) <- names(dt_lambda)
annotation_col_row_names <- row.names(annotation_col)

# order by vartype
col_order <- order(annotation_col$vartype, 
                   decreasing = TRUE)
annotation_col <- dplyr::arrange(annotation_col, 
                                 vartype)
row.names(annotation_col) <- annotation_col_row_names[rev(col_order)] 
dt_lambda <- dt_lambda[,rev(col_order)]

top_labs <- names(colMeans(dt_lambda)[
  order(abs(colMeans(dt_lambda)), decreasing = TRUE)][1:15])
labs <- names(dt_lambda)
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

pheatmap(dt_lambda, 
         breaks = round(seq(-0.02, 0.02, 
                            length.out = 7), 3),
         color = rev(RColorBrewer::brewer.pal(6, "RdBu")),
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         show_rownames = FALSE, 
         labels_col = labs, 
         labels_row = labels_row,
         main = "Lambda 3 chla", 
         annotation_col = annotation_col,
         annotation_colors = ann_colors,
         cellwidth = 7)
