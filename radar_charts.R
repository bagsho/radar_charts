library(readxl)
library(fmsb)
source("my_radarchart_functions.R")

# Load data ---------------------------
senaryolar<-get_data()

# Charts without categories ---------------------------
create_bau_chart(senaryolar)
create_scenarios_chart(senaryolar)

# Charts with categories ---------------------------
create_bau_chart_ctg(senaryolar)
create_scenarios_chart_ctg(senaryolar)