require(dplyr)
require(purrr)
require(stringr)
require(rio)
require(ISOcodes)
ISO_3166_1 %>% select(from='Alpha_3',to='Alpha_2')  %>%
    rbind(.,tribble(~from,~to,"XKX","XK")) %>% export('/doc/wissenschaft/rpackages/reco/inst/data/cnt3_cnt2.csv',quote=TRUE)

ISO_3166_1 %>% select(from='Alpha_2',to='Alpha_3')  %>%
    rbind(.,tribble(~from,~to,"XK","XKX")) %>%
    export('/doc/wissenschaft/rpackages/reco/inst/data/cnt2_cnt3.csv',quote=TRUE)

ISO_3166_1 %>% select(from='Name',to='Alpha_3')  %>%
    rbind(.,tribble(~from,~to,"Kosovo","XKX")) %>% 
    export('/doc/wissenschaft/rpackages/reco/inst/data/cnt_cnt3.csv',quote=TRUE)

ISO_3166_1 %>% select(from='Alpha_3',to='Name')  %>%
    rbind(.,tribble(~from,~to,"XKX","Kosovo")) %>% 
    export('/doc/wissenschaft/rpackages/reco/inst/data/cnt3_cnt.csv',quote=TRUE)

ISO_3166_1 %>% select(from='Name',to='Alpha_2')  %>%
    rbind(.,tribble(~from,~to,"Kosovo","XK")) %>% 
    export('/doc/wissenschaft/rpackages/reco/inst/data/cnt_cnt2.csv',quote=TRUE)



ISO_3166_1 %>% select(from='Alpha_2',to='Name')  %>%
    rbind(.,tribble(~from,~to,"XK","Kosovo")) %>% 
    export('/doc/wissenschaft/rpackages/reco/inst/data/cnt2_cnt.csv',quote=TRUE)


'/doc/wissenschaft/rpackages/reco/inst/data' -> datapath
dir(datapath,full=1,pattern='csv$')  -> datafiles
#print(datafiles)
datafiles %>% walk(~{
#    print(.x)
    .x  %>% str_replace(paste0(datapath,'/'),'') %>% str_replace('.csv','') -> datasetname
                                        #    print(datasetname)
    cat('Installing dataset: `',datasetname,'`\n')
    assign(datasetname,    import(.x))
    paste0('save(',datasetname,',file="',datapath %>% str_replace('/inst',''),'/',datasetname,'.rda")') -> code
 #   print(code)
    eval(parse(text=code))
    })
