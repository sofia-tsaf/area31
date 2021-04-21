## Extract results of interest, write TAF output tables

## Before: results.rds (model)
## After:  stock_tables/*.csv, all_effort.txt, current_status.csv, results.rds,
##         stock_timeseries.csv (output)

library(TAF)
library(dplyr)   # case_when, count, group_by, select, filter, mutate
library(tidyr)   # pivot_wider, unnest

mkdir("output")

## Read model results and make available in 'output' folder
stocks <- readRDS("model/results.rds")
cp("model/results.rds", "output")

## Categorize stock status by comparing B/Bmsy to 0.8 and 1.2
current_status <- stocks %>%
  select(stock, taxa, sraplus_summary) %>%
  unnest(cols = sraplus_summary) %>%
  filter(variable == "b_div_bmsy") %>%
  mutate(status = case_when(mean > 1.2 ~ "underfished",
                            mean > 0.8 ~ "fully fished",
                            TRUE ~ "overfished"))
write.taf(current_status, "output/current_status.csv")
current_status %>%
  group_by(status) %>%
  count()

## Write stock tables containing model results by year
mkdir("output/stock_tables")
for(i in 1:nrow(stocks)){
  write.csv(stocks$sraplus_fit[i][[1]]$results,
            file=paste0("output/stock_tables/",stocks$stock[i],".csv"))
  write.table(stocks$sraplus_fit[i][[1]]$results,
              file="output/all_effort.txt", append=TRUE)
}

## Examine diagnostics
stocks$sraplus_diagnostics[[1]]$final_gradient

## Tabulate B/Bmsy and F/Fmsy time series for each stock
n <- length(stocks$stock)
resList <- vector(mode="list", length=n)
for(i in 1:n){
  tmp <- stocks$sraplus_fit[[i]]$results %>%
    filter(variable %in% c("b_div_bmsy","u_div_umsy")) %>%
    pivot_wider(id_cols="year", names_from="variable", values_from="mean")
  resList[[i]] <- cbind(stock=stocks$stock[i], tmp)
}
resTab <- Reduce(rbind, resList)
newResTab <- resTab
names(newResTab) <- c("Stock", "yr", "bbmsy", "ffmsy")
newResTab$bbmsy.effEdepP <- newResTab$bbmsy
newResTab$ffmsy.effEdepP <- newResTab$ffmsy
write.taf(newResTab, "output/stock_timeseries.csv")
