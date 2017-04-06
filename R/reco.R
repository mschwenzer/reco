##' get_a.df
##'
##' return a.df from reco env
##' @title .. content for \description{} (no empty lines) ..
##' @param string 
##' @return a.df  
##' @author Marc Schwenzer
get_a.df <- function(string)
{
get('a.df',inherits='TRUE')
}


##' set_a.df
##'
##' set a.df from reco env
##' @title .. content for \description{} (no empty lines) ..
##' @param string 
##' @return Null
##' @author Marc Schwenzer
set_a.df <- function(val)
{
    assign('a.df',val,inherits=TRUE)
}

##' validate_df_structure
##'
##' Check if the file has the correct format
##' @title validate_df_structure
##' @param df a data.frame
##' @return df 
##' @author Marc Schwenzer
validate_df_structure <- function(df)
    {df}


##' reco_filters
##'
##' filter conditions
##' @title reco_filters
##' @param x a vector
##' @return string modified by several possible filters
##' @author Marc Schwenzer
reco_filters <- function(x)
    {x}


##' load_file_or_create_it
##'
##' Load a data.frame or return NULL.
##' @title load_file_or_create_it
##' @param file 
##' @return 
##' @author Marc Schwenzer
load_df_or_create_it <- function(file)
{
    if(file %>% file.exists)
    {
        file  %>% import  %>% validate_df_structure  -> a.df
        file ->  attr(a.df,'file')
                }
    else
    {
        data.frame(from='from',to='to') %>% .[-1,] -> a.df
        a.df  %>% export(file=file)
        file ->  attr(x=a.df,which='file')
    }
    a.df
}

##' reco
##'
##' free and easy recode string based on a file
##' @title 
##' @param string 
##' @param file 
##' @return 
##' @author Marc Schwenzer
reco <- function(string,file)
{
    load_df_or_create_it(file)  %>% set_a.df
    string %>% reco_filters  %>% unique  ->  x
    # add a reference to this environment to the x string
                                        # were to modify x
    x%>% map(~reco_do(string=.))
    get_a.df()[,2] -> replacements
    get_a.df()[,1] -> names(replacements)
    replacements[string] -> string
    NULL -> names(string)
    string
}




##' reco_do
##'
##' decide if ask_string_write_to_file is necessary
##' @title reco_do
##' @param string 
##' @param a.df
##' @return 
##' @author Marc Schwenzer
reco_do <- function(string)
{
        if(nrow(get_a.df())<1)
    {
        ask_string_write_to_file(string)
    }
    else
    {
        ifelse(string%in%get_a.df()[,1],'ok',ask_string_write_to_file(string)
        )
    }
    }
                                        #                           
 


ask_string_write_to_file<- function(string){
    suggest_based_on_df(string) -> new.string
    get_a.df() %>% attr('file') -> file
    bind_rows(get_a.df(),
              data.frame(from=string,to=new.string)
              ) -> a.df.new
#    cat('---a.df.y@ask_string_write_to_file\n')
 #   print(a.df.new)
    file -> attr(a.df.new,'file')
    a.df.new %>% export(file=file)
    set_a.df(a.df.new)
}


suggest_based_on_df <- function(string)
{
    cat(paste0('Choose Alternative for `',string[1],'`\n'))
    get_a.df()[,2] -> alternatives
    alternatives %>% unique -> alternatives
    if(length(alternatives)>0)
        {
            data.frame(num=1:length(alternatives),alt=alternatives) -> alt.tab
            print(alt.tab)
        }
    '' -> input
    while(input==''|input==0)
        {
            scan(what=character(),n=1) -> input
            }
    if(input  %>% as.numeric %>% is.na  %>% `!`)
    {
        alt.tab$alt[which(alt.tab$num%in%(input %>% as.numeric) )] -> input
    }
    input
}
