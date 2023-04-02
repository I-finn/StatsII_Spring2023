# Imelda Finn
# 22334657
# App Stats II - 2023

# Replication
library(here)
source(here("R","clean_env.R"))

#https://wiwi.uni-mainz.de/files/2020/01/mmbruteluck_paper.pdf

lapply("devtools", pkgTest)
install_github("fair-nhh/mmbruteluck")

#lab exp
mmbruteluck::bldata
# online exp
bl_kantar <- mmbruteluck::bl_kantar
bl_mturkworkers <-mmbruteluck::mturk_workers

