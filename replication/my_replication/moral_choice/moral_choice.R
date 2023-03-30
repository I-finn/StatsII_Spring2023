# Imelda Finn
# 22334657
# App Stats II - 2023

# Replication

source("clean_env.R")


#https://wiwi.uni-mainz.de/files/2020/01/mmbruteluck_paper.pdf

lapply("devtools", pkgTest)
install_github("fair-nhh/mmbruteluck")

??mmbruteluck
vignette(bl_kantar)

source("R/c_online.R")
source("R/B_lab.R")
source("R/c_mturk.R")
