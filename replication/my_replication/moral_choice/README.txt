Readme file to document the data and analysis files from the paper
"Choice and personal responsibility: What is a morally relevant choice?" by
Alexander W. Cappelen, Sebastian Fest, Erik Ø. Sørensen, and Bertil Tungodden,
to be published in The Review of Economics and Statistics.

There are three different datafiles, these correspond to the different samples, three analysis files,
this readme-file, and a R-project file:
    - bl_online.csv: Data from the online experiment
    - bl_lab.csv: Data from the lab experiment.
    - bl_mturkworkers.csv: Data from the mturk stakeholders that work and are paid
      as part of the online experiment.
    - A-online-experiment.Rmd: Generates analysis on the online experiment (creates A-online-experiment.html, , 
      included for reference) when rendered in R. Se below for what software versions were used.
    - B-lab-experiment.Rmd: Generates analysis on the lab experiment (creates B-lab-experiment.html, 
      included for reference) when rendered in R.
    - C-bl-mturk-workers.Rmd: Generates balance statistics on the mTurk workers (creates C-bl-mturk-workers.html, 
      included for reference) when rendered in R.
    - README.txt: This readme file.
    - restat.Rproj: R-studio project definitions.

The variables in the *.csv files are described fully below. 

There are three analysis files (".Rmd"-filesthat generate all the output in
".html" files. These also output tables and graphs as seperate files, and there should
be made subdirectories "graphs/" and "tables/" to contain the output from these. 
The replication files also produce some extra output requested at points from 
referees and other interested parties. These are more easily accessed at 
https://fair-nhh.github.io/mmbruteluck/index.html , where all analysis is implemented
as vignettes in an R package that can be downloaded and installed from github (instructions
online). 

Software versions used for the calculation of results:
#> R version 4.0.3 (2020-10-10)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: Ubuntu 18.04.5 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
#> LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=nb_NO.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=nb_NO.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=nb_NO.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=nb_NO.UTF-8 LC_IDENTIFICATION=C       
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#>  [1] here_0.1           multiwayvcov_1.2.3 multcomp_1.4-14    TH.data_1.0-10    
#>  [5] MASS_7.3-53        survival_3.2-7     mvtnorm_1.1-1      stargazer_5.2.2   
#>  [9] forcats_0.5.0      stringr_1.4.0      dplyr_1.0.2        purrr_0.3.4       
#> [13] readr_1.4.0        tidyr_1.1.2        tibble_3.0.4       ggplot2_3.3.2     
#> [17] tidyverse_1.3.0   
#> 
#> loaded via a namespace (and not attached):
#>  [1] httr_1.4.2       jsonlite_1.7.1   splines_4.0.3    modelr_0.1.8    
#>  [5] assertthat_0.2.1 highr_0.8        blob_1.2.1       cellranger_1.1.0
#>  [9] yaml_2.2.1       pillar_1.4.6     backports_1.1.10 lattice_0.20-41 
#> [13] glue_1.4.2       digest_0.6.25    rvest_0.3.6      colorspace_1.4-1
#> [17] sandwich_3.0-0   htmltools_0.5.0  Matrix_1.2-18    pkgconfig_2.0.3 
#> [21] broom_0.7.1      haven_2.3.1      scales_1.1.1     generics_0.0.2  
#> [25] farver_2.0.3     ellipsis_0.3.1   withr_2.3.0      cli_2.1.0       
#> [29] magrittr_1.5     crayon_1.3.4     readxl_1.3.1     evaluate_0.14   
#> [33] fs_1.5.0         fansi_0.4.1      xml2_1.3.2       tools_4.0.3     
#> [37] hms_0.5.3        lifecycle_0.2.0  munsell_0.5.0    reprex_0.3.0    
#> [41] compiler_4.0.3   rlang_0.4.8      grid_4.0.3       rstudioapi_0.11 
#> [45] labeling_0.3     rmarkdown_2.4    boot_1.3-25      gtable_0.3.0    
#> [49] codetools_0.2-16 DBI_1.1.0        R6_2.4.1         zoo_1.8-8       
#> [53] lubridate_1.7.9  knitr_1.30       utf8_1.1.4       rprojroot_1.3-2 
#> [57] stringi_1.5.3    parallel_4.0.3   Rcpp_1.0.5       vctrs_0.3.4     
#> [61] dbplyr_1.4.4     tidyselect_1.1.0 xfun_0.18

Data file descriptions:

Online data file: bl_online.csv (7124 observations and 22 variables):
Respondent_Serial
    Non-informative identifier of individual participant
comp
    Did the person complete the experiment (1: yes, 2: no)
treatment
    Treatment name (characters)
treatment_kantar
    Treatment names as from kantar (characters)
treatmentgroup
    Grouping of treatments into "Base", "Forced Choice" and "Nominal Choice")
y
    Transfer to the unlucky (numeric)
workp
    Was there a work requirement in this treatment? (logical)
org_strictp
    Indicator for treatment mapping 1:1 to lab experiment (logical)
org_weakp
    Indicator for treatment being in lab experiment, but possibly without a work requirement (logical)
cr1
    First cognitive reflection question (0/1)
cr2
    Second cognitive reflection question (0/1)
cr3
    Third cognitive reflection question (0/1)
crt
    Cognitive reflection score (0-3)
understanding1
    First understanding question, characters ("Ja" (yes), "Nei" (no), "Vet ikke" (don't know))
understanding2
    Second understanding question, characters
age
    Age in years (numeric)
gender
    Gender, characters ("Kvinne": female, "Mann": male)
education
    Education (characters)
        Grunnskole
             Primary school
        Fagutdanning
            Vocational
        Videregaaende
            High school
        Universitet/hoyskole I
            Some university / college
        Universitet/hoyskole II
            University / college
indincome
    Individual income bracket (characters)
occupation
    Occupation (characters)
stortingsvalg
    Vote in last election
    R
        "Rødt" ("Red", far left)
    SV
        "Sosialistisk Venstreparti" ("socialist left")
    Ap
        "Arbeiderpartiet" ("Labour")
    MDG
        "Miljøpartiet De Grønne" ("Environmental Green Party")
    Sp
        "Senterpartiet" ("Center party" (farm and rural party))
    KrF
        "Kristelig Folkeparti" ("Christian democrats", centrist)
    Kp
        "Kystpartiet" ("Coast party")
    V
        "Venstre" ("Left", European liberal party, centrist )
    H
        "Høyre" ("Right", conservative party)
    Frp
        "Fremskrittspartiet" ("Progress party", right-populist party)
    Andre partier og lister
        "Some other party"
    Vil ikke oppgi parti"
        "Don't want to report"
    Stemte ikke
        "Did not vote"
    Hadde ikke stemmerett"
        "Did not have a right to vote"
    Husker ikke/vet ikke"
        "Don't remember/don't know"
leftp
    Code for not voting to the right (logical, recode of "stortingsvalg")



Lab data file bl_online.csv (422 observations and 15 variables):
pid
    Non-informative identifier of individual participant
location
    Location of experiment:
        1. University of Bergen
        2. NHH Norwegian School of Economics
sesid
    Id of session (not chronological)
T
    Treatment:
        1. No Choice
        2. Nominal Choice
        3. Forced Choice
lotteryposition
    Lotterychoice or allocation (character):
        G: Green ball
        S: Safe alternative
        Y: Yellow ball
transfer
    Transfer from Lucky to Unlucky (0-800, in NOK).
payment
    Final payment received, showup fee inclusive (100-900, in NOK).
kull
    Program of study.
        1. Bachelor
        2. Master
        9. Other program
sex
    Self-reported sex of participant:
        1. Male
        2. Female
age
    Age (in years).
polparty
    Vote for in election?
        1. SV
        2. Ap
        3. Sp
        4. KrF
        5. V
        6. H
        7. FrP
cr
    Cognitive reflection score (0-3).
cr1
    Cognitive reflection 1: Ball and a bat..., 1 if correct.
cr2
    Cognitive reflection 2: 5m/5/5min, what about 100m..., 1 if correct.
cr3
    Cognitive reflection 3: 48 days to cover pond..., 1 if correct.


mTurk datafile bl_mturkworkers.csv (2437 observations and 16 variables):
id
    Non-informative identifier of individual participant
female
    Indicator for mTurker being female (0/1)
age
    Age of mTurker in years (numeric)
education
    Education category
        1. Less than High School
        2. High School/GED
        3. Some College
        4. 2 year College Degree
        5. 4 year College Degree
        6. Masters Degree
        7. Masters Degree
        8. Professional Degree (JD, MD)
political
    Political identification.Numeric, [-2, - 2], with -2 being "very liberal" and 2 "very conservative.
treatment_str
    Treatment (characters)
risky
    Did the mTurker choose the risky alternative in the Forced Choice treatment? (0/1), 1 if they chose the risky alternative
green
    Indicator for whether the worker chose the green ball in the nominal choice treatments (0/1), 1 if they did so.
duration_survey
    Duration of HIT completion (numeric, in seconds)
bonus_startup
    Bonus account at startup (numeric, 0.5 USD)
winner
    Indicator for whether the worker is a lottery winner (0/1).
loser
    Indicator for whether the worker is a lottery loser (0/1).
bonus_ingame
    bonus in game (0.5USD) - 0.25 USD if worker chose costly safe alternative in that treatment
bonus_lottery
    Outcome of lottery choice prior to redistribution (numeric, in USD)
bonus_final
    The sum of bonus_ingame and the post redistribution income from lottery (numeric, in USD)
repeater
    Has the worker attempted to start the survey more than once? (0/1), 1 if they did so.
