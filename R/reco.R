# * get_a.df
##' get_a.df
##'
##' return a.df from reco env
##' @title .. content for \description{} (no empty lines) ..
##' @param string 
##' @return a.df  
##' @author Marc Schwenzer
##' @importFrom dplyr %>% 
get_a.df <- function(string)
{
get('a.df',inherits='TRUE')
}

# * set_a.df
##' set_a.df
##'
##' set a.df from reco env
##' @title .. content for \description{} (no empty lines) ..
##' @param string 
##' @return Null
##' @author Marc Schwenzer
##' @importFrom dplyr %>% 
set_a.df <- function(val)
{
    assign('a.df',val,inherits=TRUE)
}

# * validate_df_structure
##' validate_df_structure
##'
##' Check if the file has the correct format
##' @title validate_df_structure
##' @param df a data.frame
##' @return df 
##' @author Marc Schwenzer
##' @importFrom dplyr %>% 
validate_df_structure <- function(df)
    {df}

# * reco_filters
##' reco_filters
##'
##' filter conditions
##' @title reco_filters
##' @param x a vector
##' @return string modified by several possible filters
##' @author Marc Schwenzer
##' @importFrom dplyr %>%
reco_filters <- function(x)
{
    if(exists('reco_filter'))
    {
        x %>% reco_filter -> x
    }
    else
    {
        x
    }
    x
}

# * load_file_or_create_it
##' load_file_or_create_it
##'
##' Load a data.frame or return NULL.
##' @title load_file_or_create_it
##' @param file 
##' @return 
##' @author Marc Schwenzer
##' @importFrom rio export
##' @importFrom rio import
##' @importFrom dplyr %>%
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


# * reco
##' reco
##'
##' free and easy recode string based on a file
##' @title 
##' @param string The string to recode
##' @param file The file where the replacements are located. Columsn have to be named as from and to
##' @param interactive If true an interactive coding session to add recodigs written to file, otherwise the recoding is done just base on the file leaving it as it is.
##' @return 
##' @author Marc Schwenzer
##' @export
##' @importFrom purrr map
##' @importFrom dplyr %>% 
reco <- function(string,file,interactive=TRUE)
{
    load_df_or_create_it(file)  %>% set_a.df
    if (interactive)
        {
    string %>% reco_filters  %>% unique  ->  x
    x%in%get_a.df()[,1] %>% sum %>% `-`(length(x),.)  %>% cat(.,' values to recode...\n')
    # add a reference to this environment to the x string
                                        # were to modify x
    x%>% map(~reco_do(string=.))
    }
    get_a.df()[,2] -> replacements
    get_a.df()[,1] -> names(replacements)
    replacements[string] -> string
    NULL -> names(string)
    string
}

# * reco_do
##' reco_do
##'
##' decide if ask_string_write_to_file is necessary
##' @title reco_do
##' @param string 
##' @param a.df
##' @return 
##' @author Marc Schwenzer
##' @importFrom dplyr %>% 
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

# * ask_string_write_to_file
##' ask_string_write_to_file
##'
##' ask_string_write_to_file
##' @title ask_string_write_to_file
##' @param string 
##' @return 
##' @author Marc Schwenzer
##' @importFrom dplyr %>%
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

# * suggest_based_on_df
##' suggest_based_on_df
##'
##' suggest_based_on_df
##' @title suggest_based_on_df
##' @param string 
##' @return 
##' @author Marc Schwenzer
##' @importFrom dplyr %>%
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
# * Filevars
# Local Variables:
# orgstruct-heading-prefix-regexp: "# "
# eval: (orgstruct++-mode)
# eval: (orgstruct-hijacker-org-shifttab 1)
# End:
