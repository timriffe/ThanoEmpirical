setwd("~/randMstataSE") # location of RAND data product
library(foreign)

# read in full HRS rand data
temp=read.dta("rndhrs_m.dta") 

# extract needed variables
dat=temp[,c("hhidpn","hacohort","rawtsamp",     # person id, cohort, sampling weight
            c(paste("inw",1:10,sep="")),        # interview response flag
            c(paste("r",1:10,"iwmid",sep="")),  # interview mid-point date
            "rabmonth","rabyear","rabdate",     # birth month, year, SAS date
            "radmonth","radyear","raddate",     # death month, year, SAS date
            c(paste("r",1:10,"agem_m",sep="")), # age at interview mid-point
            "ragender",                         # gender
            c(paste("r",2:10,"adlwa",sep="")),  # ADL: 3-point summary
            c(paste("r",2:10,"adla",sep="")),   # ADL: 5-point summary
            c(paste("r",2:10,"iadla",sep="")),  # IADL: 3-point summary
            c(paste("r",2:10,"iadlza",sep="")), # IADL: 5-point summary
            c(paste("r",2:10,"cesd",sep="")),   # CESD: 8-point summary
            "raracem","rahispan","raedyrs",     # race, hispanic, edu (yrs)
            "rabplace","rameduc","rafeduc",     # birth place, mom's edu, pop's edu
            c(paste("r",1:10,"cenreg",sep="")), # census region
            c(paste("r",1:10,"cendiv",sep="")), # census division 
            c(paste("r",1:10,"mstat",sep="")),  # marital status
            c(paste("r",1:10,"hlthlm",sep="")), # health limits work
            c(paste("r",1:10,"shlt",sep="")),   # self-reported health
            c(paste("r",1:10,"bmi",sep="")),    # bmi
            c(paste("r",1:10,"back",sep="")),   # back problems
            c(paste("r",1:10,"wtresp",sep="")), # person-level weights
            
            c(paste("r",1:10,"hosp",sep="")),   # overnight hopsitalization
            c(paste("r",1:10,"hsptim",sep="")), # num hospital stays
            c(paste("r",1:10,"hspnit",sep="")), # num hospital nights
            c(paste("r",1:10,"nrshom",sep="")), # overnight nursing home stays
            c(paste("r",1:10,"nrstim",sep="")), # num nursing home stays
            c(paste("r",1:10,"nrsnit",sep="")), # num nursing home nights
            c(paste("r",3:10,"nhmliv",sep="")), # currently living nurs. home
            c(paste("r",3:10,"nhmmvm",sep="")), # month moved to nurs. home
            c(paste("r",3:10,"nhmmvy",sep="")), # year moved to nurs. home
            c(paste("r",3:10,"nhmday",sep="")), # days in nurs. home
            c(paste("r",1:10,"doctor",sep="")), # doctor visit
            c(paste("r",1:10,"doctim",sep="")), # num doctor visits
            c(paste("r",1:10,"homcar",sep="")), # home health care
            c(paste("r",2:10,"drugs",sep="")),  # regular prescription drugs
            c(paste("r",2:10,"outpt",sep="")),  # outpatient surgery
            c(paste("r",2:10,"dentst",sep="")), # dentist visit
            c(paste("r",2:10,"spcfac",sep="")), # special health facility visit
            c(paste("r",3:6,"totmbi",sep="")),  # total imputed med. expenditure
            c(paste("r",2:10,"walkra",sep="")), # adl: walking
            c(paste("r",2:10,"dressa",sep="")), # adl: dressing
            c(paste("r",2:10,"batha",sep="")),  # adl: bathing
            c(paste("r",2:10,"eata",sep="")),   # adl: eating
            c(paste("r",2:10,"beda",sep="")),   # adl: bed
            c(paste("r",2:10,"toilta",sep="")), # adl: toilet
            c(paste("r",2:10,"mapa",sep="")),   # iadl: map
            "r2calca",                          # iadl: calculator
            c(paste("r",2:10,"phonea",sep="")), # iadl: phone
            c(paste("r",2:10,"moneya",sep="")), # iadl: money
            c(paste("r",2:10,"medsa",sep="")),  # iadl: medications
            c(paste("r",2:10,"shopa",sep="")),  # iadl: shopping
            c(paste("r",2:10,"mealsa",sep="")), # iadl: meals
            c(paste("r",2:10,"mobila",sep="")), # mobility difficulty index
            c(paste("r",2:10,"lgmusa",sep="")), # large muscle difficulty index
            c(paste("r",2:10,"grossa",sep="")), # gross motor difficulty index
            c(paste("r",2:10,"finea",sep="")),  # fine motor difficulty index
            c(paste("r",1:10,"hibpe",sep="")),  # high blood pressure
            c(paste("r",1:10,"diabe",sep="")),  # diabetes
            c(paste("r",1:10,"cancre",sep="")), # cancer
            c(paste("r",1:10,"lunge",sep="")),  # lung disease
            c(paste("r",1:10,"hearte",sep="")), # heart problems
            c(paste("r",1:10,"stroke",sep="")), # stroke
            c(paste("r",1:10,"psyche",sep="")), # psychological problems
            c(paste("r",1:10,"arthre",sep="")), # arthritis
            c(paste("r",1:10,"conde",sep="")),  # num chronic conditions
            c(paste("r",1:2,"ulcer",sep="")),   # ulcers
            c(paste("r",1:6,"vigact",sep="")),  # vig. phys. act: 3+ times per week
            c("r1vgactf",paste("r",7:10,"vgactx",sep="")), # vig. phys. act: frequency
            c(paste("r",1:2,"lhtact",sep="")),  # lt. phys. act: 3+ times per week
            c("r1ltactf",paste("r",7:10,"ltactx",sep="")), # lt. phys. act: frequency
            c(paste("r",7:10,"mdactx",sep="")), # mod. phys. act: frequency
            c(paste("r",1:10,"drink",sep="")),  # alcohol: ever
            c(paste("r",3:10,"drinkd",sep="")), # alcohol: days
            c(paste("r",3:10,"drinkn",sep="")), # alcohol: number
            c(paste("r",1:10,"smokev",sep="")), # smoking: ever
            c(paste("r",1:10,"smoken",sep="")), # smoking: current
            c("r1deprex",paste("r",2:10,"depres",sep="")), # cesd: depression
            c("r1efforx",paste("r",2:10,"effort",sep="")), # cesd: effort
            c("r1sleepx",paste("r",2:10,"sleepr",sep="")), # cesd: sleep
            c("r1whappx",paste("r",2:10,"whappy",sep="")), # cesd: happy
            c("r1flonex",paste("r",2:10,"flone",sep="")),  # cesd: lonely
            c("r1fsadx",paste("r",2:10,"fsad",sep="")),    # cesd: sad
            c("r1goingx",paste("r",2:10,"going",sep="")),  # cesd: get going
            c("r1enlifx",paste("r",2:10,"enlife",sep="")), # cesd: enjoy life
            
            c(paste("r",5:6,"wtr_nh",sep="")),   # nursing home weights 
            
            c(paste("r",1:10,"liv75",sep="")),   # prob. living to age 75+
            
            c(paste("r",4:9,"memry",sep="")),    # doc. diag.: mem. prob. at interview
            c(paste("r",4:9,"memrye",sep="")),   # doc. diag.: mem. prob. ever
            c(paste("r",10,"alzhe",sep="")),     # doc. diag.: alzheimer's disease
            c(paste("r",10,"demen",sep="")),     # doc. diag.: dementia
            
            c(paste("r",1:10,"slfmem",sep="")),  # cognition: self-rated memory
            c(paste("r",1:10,"pstmem",sep="")),  # cognition: mem. compared to past
            c(paste("r",3:10,"imrc",sep="")),    # cognition: imm. word recall - 10 words
            "r2aimr10",
            "r1imrc20","r2himr20",               # cognition: imm. word recall - 20 words
            c(paste("r",3:10,"dlrc",sep="")),    # cognition: del. word recall - 10 words
            "r2adlr10",
            "r1dlrc20","r2hdlr20",               # cognition: del. word recall - 20 words
            c(paste("r",2:10,"ser7",sep="")),    # cognition: serial 7s
            c(paste("r",2:10,"bwc20",sep="")),   # cognition: backwards counting from 20
            c(paste("r",3:6,"bwc86",sep="")),    # cognition: backwards counting from 86
            c(paste("r",2:10,"mo",sep="")),      # cognition: naming month
            c(paste("r",2:10,"dy",sep="")),      # cognition: naming day of month
            c(paste("r",2:10,"yr",sep="")),      # cognition: naming year
            c(paste("r",2:10,"dw",sep="")),      # cognition: naming day of week
            c(paste("r",2:10,"scis",sep="")),    # cognition: naming scissors
            c(paste("r",2:10,"cact",sep="")),    # cognition: naming cactus
            c(paste("r",2:10,"pres",sep="")),    # cognition: naming president
            c(paste("r",2:10,"vp",sep="")),      # cognition: naming vice-president
            c(paste("r",3:10,"vocab",sep="")),   # cognition: vocab score
            c(paste("r",3:10,"tr20",sep="")),    # cognition: total 20 word recall score
            "r2atr20",
            "r1tr40","r2htr40",                  # cognition: total 40 word recall score
            c(paste("r",3:10,"mstot",sep="")),   # cognition: total mental status score
            "r2amstot"
              )]

rm(temp) # free up some mems

dat$dead=ifelse(is.na(dat$raddate),0,1) # indicator for death; 0=not dead; 1=dead
dat$days_alive=dat$raddate-dat$rabdate  # number of days alive; NAs for those still alive

# rename variables for sanity
names(dat) = c("id","cohort","s_wt",c(paste("intv",1:10,sep="")),
               c(paste("intv_dt",1:10,sep="")),"b_mo","b_yr","b_dt",
               "d_mo","d_yr","d_dt",c(paste("age",1:10,sep="")),
               "sex",c(paste("adl3_",2:10,sep="")),
               c(paste("adl5_",2:10,sep="")),c(paste("iadl3_",2:10,sep="")),
               c(paste("iadl5_",2:10,sep="")),c(paste("cesd",2:10,sep="")),
               "race","hisp","edu_yrs","b_pl","m_edu","f_edu",
               c(paste("reg",1:10,sep="")),c(paste("div",1:10,sep="")),
               c(paste("mar",1:10,sep="")),c(paste("lim_work",1:10,sep="")),
               c(paste("srh",1:10,sep="")),c(paste("bmi",1:10,sep="")),
               c(paste("back",1:10,sep="")),c(paste("p_wt",1:10,sep="")),
               c(paste("hosp",1:10,sep="")),c(paste("hosp_stays",1:10,sep="")),
               c(paste("hosp_nights",1:10,sep="")),c(paste("nh",1:10,sep="")),
               c(paste("nh_stays",1:10,sep="")),c(paste("nh_nights",1:10,sep="")),
               c(paste("nh_now",3:10,sep="")),c(paste("nh_mo",3:10,sep="")),
               c(paste("nh_yr",3:10,sep="")),c(paste("nh_days",3:10,sep="")),
               c(paste("doc",1:10,sep="")),c(paste("doc_visits",1:10,sep="")),
               c(paste("hhc",1:10,sep="")),c(paste("meds",2:10,sep="")),
               c(paste("surg",2:10,sep="")),c(paste("dent",2:10,sep="")),
               c(paste("shf",2:10,sep="")),c(paste("med_exp",3:6,sep="")),
               c(paste("adl_walk",2:10,sep="")),c(paste("adl_dress",2:10,sep="")),
               c(paste("adl_bath",2:10,sep="")),c(paste("adl_eat",2:10,sep="")),
               c(paste("adl_bed",2:10,sep="")),c(paste("adl_toilet",2:10,sep="")),
               c(paste("iadl_map",2:10,sep="")),"iadl_calc2",
               c(paste("iadl_tel",2:10,sep="")),c(paste("iadl_money",2:10,sep="")),
               c(paste("iadl_meds",2:10,sep="")),c(paste("iadl_shop",2:10,sep="")),
               c(paste("iadl_meals",2:10,sep="")),c(paste("mob",2:10,sep="")),
               c(paste("lg_mus",2:10,sep="")),c(paste("gross_mot",2:10,sep="")),
               c(paste("fine_mot",2:10,sep="")),c(paste("bp",1:10,sep="")),
               c(paste("diab",1:10,sep="")),c(paste("cancer",1:10,sep="")),
               c(paste("lung",1:10,sep="")),c(paste("heart",1:10,sep="")),
               c(paste("stroke",1:10,sep="")),c(paste("psych",1:10,sep="")),
               c(paste("arth",1:10,sep="")),c(paste("cc",1:10,sep="")),
               c(paste("ulc",1:2,sep="")),c(paste("vig",1:6,sep="")),
               "vig_freq1",c(paste("vig_freq",7:10,sep="")),
               c(paste("lt",1:2,sep="")),"lt_freq1",
               c(paste("lt_freq",7:10,sep="")),c(paste("mod_freq",7:10,sep="")),
               c(paste("alc_ev",1:10,sep="")),c(paste("alc_days",3:10,sep="")),
               c(paste("alc_drinks",3:10,sep="")),c(paste("smoke_ev",1:10,sep="")),
               c(paste("smoke_cur",1:10,sep="")),c(paste("cesd_depr",1:10,sep="")),
               c(paste("cesd_eff",1:10,sep="")),c(paste("cesd_sleep",1:10,sep="")),
               c(paste("cesd_happy",1:10,sep="")),c(paste("cesd_lone",1:10,sep="")),
               c(paste("cesd_sad",1:10,sep="")),c(paste("cesd_going",1:10,sep="")),
               c(paste("cesd_enjoy",1:10,sep="")),c(paste("nh_wt",5:6,sep="")),
               c(paste("prob75yo",1:10,sep="")),c(paste("mprob",4:9,sep="")),
               c(paste("mprobev",4:9,sep="")),"alz10","dem10",
               c(paste("srm",1:10,sep="")),c(paste("pastmem",1:10,sep="")),
               c(paste("ir10w",3:10,sep="")),"ir10w2","ir20w1","ir20w2",
               c(paste("dr10w",3:10,sep="")),"dr10w2","dr20w1","dr20w2",
               c(paste("ss",2:10,sep="")),c(paste("c20b",2:10,sep="")),
               c(paste("c86b",3:6,sep="")),c(paste("name_mo",2:10,sep="")),
               c(paste("name_dmo",2:10,sep="")),c(paste("name_yr",2:10,sep="")),
               c(paste("name_dwk",2:10,sep="")),c(paste("name_sci",2:10,sep="")),
               c(paste("name_cac",2:10,sep="")),c(paste("name_pres",2:10,sep="")),
               c(paste("name_vp",2:10,sep="")),c(paste("vocab",3:10,sep="")),
               c(paste("tr20w",3:10,sep="")),"tr20w2","tr40w1","tr40w2",
               c(paste("tm",3:10,sep="")),"tm2",
               
               "dead","days_alive")

# convert factors to strings to prevent future possibility of terribad conversion to integer values
i <- sapply(dat, is.factor)
dat[i] <- lapply(dat[i], as.character) 

# switch over to research directory
setwd("~/Research")

# save Rdata in wide-format
save(dat,file="thanos_wide_v2_2")

# create .dta file to pass to stata
write.dta(dat,file="thanos_wide_v2_2.dta")

# read in long-format data from stata (product of thanos_long_v2_2.do)
dat.l=read.dta(file="thanos_long_v2_2.dta")

# save Rdata in long-format
save(dat.l,file="thanos_long_v2_2")
