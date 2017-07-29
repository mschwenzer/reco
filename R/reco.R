# * get_a.df
##' return a.df from reco env
##'
##' ##' get_a.df
##' @keywords internal
##' @param string 
##' @return a.df  
##' @author Marc Schwenzer
##' @importFrom dplyr %>% 
get_a.df <- function(string)
{
get('a.df',inherits='TRUE')
}

# * set_a.df
##' set a.df from reco env
##'
##' 
##' @title set_a.df
##' @param string 
##' @keywords internal
##' @return Null
##' @author Marc Schwenzer
##' @importFrom dplyr %>% 
set_a.df <- function(val)
{
    assign('a.df',val,inherits=TRUE)
}

# * validate_df_structure
##' Check if the file has the correct format
##'
##' @title validate_df_structure
##' @param df a data.frame
##' @keywords internal
##' @return df 
##' @author Marc Schwenzer
##' @importFrom dplyr %>% 
validate_df_structure <- function(df)
    {
        df
    }
# * reco_filters
##' filter conditions
##'
##' @title reco_filters
##' @param x a vector
##' @return string modified by several possible filters
##' @keywords internal
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
##' Load a data.frame or return NULL.
##'
##' @title load_file_or_create_it
##' @param file 
##' @return NULL
##' @author Marc Schwenzer
##' @keywords internal
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


# * reco_do
##' reco_do
##'
##' decide if ask_string_write_to_file is necessary
##' @title reco_do
##' @param string 
##' @param a.df
##' @keywords internal
##' @return 
##' @author Marc Schwenzer
##' @importFrom dplyr %>% 
reco_do <- function(string,tab=TRUE)
{
        if(nrow(get_a.df())<1)
    {
        ask_string_write_to_file(string)
    }
    else
    {
#        print(string)
                if(string%in%get_a.df()[,1] %>% `!`) {ask_string_write_to_file(string,tab=tab)}
    }
    }

# * ask_string_write_to_file
##' ask_string_write_to_file
##'
##' ask_string_write_to_file
##' @title ask_string_write_to_file
##' @param string
##' @keywords internal
##' @return 
##' @author Marc Schwenzer
##' @importFrom dplyr %>%
ask_string_write_to_file<- function(string,tab=TRUE){
    suggest_based_on_df(string,tab=tab) -> new.string
    get_a.df() %>% attr('file') -> file
    if (is.na(new.string))
        {
    bind_rows(get_a.df(),
              data.frame(from=string,to=new.string)
              ) -> a.df.new
    file -> attr(a.df.new,'file')
    a.df.new %>% export(file=file)
    set_a.df(a.df.new)
    return()
                }
    if (new.string!='')
        {
    bind_rows(get_a.df(),
              data.frame(from=string,to=new.string)
              ) -> a.df.new
    file -> attr(a.df.new,'file')
    a.df.new %>% export(file=file)
    set_a.df(a.df.new)
    }
}

# * suggest_based_on_df
##' suggest_based_on_df
##'
##' suggest_based_on_df
##' @title suggest_based_on_df
##' @param string
##' @keywords internal
##' @return 
##' @author Marc Schwenzer
##' @importFrom dplyr %>%
suggest_based_on_df <- function(string,tab=TRUE)
{
    if (tab)
        {
    cat(paste0('Choose Alternative for `',string[1],'`\n'))
    }
    else
        {
    cat(paste0('`',string[1],'`: '))
            }
    get_a.df()[,2] -> alternatives
    alternatives %>% unique -> alternatives
    if(length(alternatives)>0)
        {
            data.frame(num=1:length(alternatives),alt=alternatives) -> alt.tab
            if(tab){print(alt.tab)}
        }
    '' -> input
            scan(what=character(),nlines=1) -> input
    if(length(input)>1){paste0(input,collapse=' ') -> input}
    if(input %>% length %>% `>`(0))
        {
    if(input  %>% as.numeric %>% is.na  %>% `!`)
    {
        if(input==0)
            {
                string-> input
                }
        else
            {
        alt.tab$alt[which(alt.tab$num%in%(input %>% as.numeric) )] -> input
        }
    }
    }
    else
    {
        '' -> input
        }
    input
}
# * 🔴 reco
##' Free and easy replace elements of a vector based on simple replacement files. 
##' 
##' @title reco
##' @param string The string to recode
##' @param file The file where the replacements are located. Columsn have to be named as from and to
##' @param interactive If true an interactive coding session to add recodigs written to file, otherwise the recoding is done just base on the file leaving it as it is. The interactive coding session presents a table of alternatives. One can either enter a new string defining a new category to which the current value is assigned or enter a number that selects one of the previous categories. Entering 0 adds the current value as category. Entering '' (just return) ignores the current value.
##' @param tab Wheter to print the tab of alternatives/categories in every step.
##' @return a vector.
##' @author Marc Schwenzer
##' @export
##' @importFrom purrr map
##' @importFrom dplyr %>% 

reco <- function(string,file,interactive=TRUE,tab=TRUE)
{
    load_df_or_create_it(file)  %>% set_a.df
    if (interactive)
        {
    string %>% reco_filters  %>% unique  ->  x
    x%in%get_a.df()[,1] %>% sum %>% `-`(length(x),.)  %>% cat(.,' values to recode...\n')
    # add a reference to this environment to the x string
                                        # were to modify x
    x%>% map(~{reco_do(string=.x,tab=tab)})
    }
    get_a.df()[,2] -> replacements
    get_a.df()[,1] -> names(replacements)
    replacements[string] -> string
    NULL -> names(string)
    string
}
# * 🔴 str_replace_by_df
##' replace every part of a string based on a data.frame given by df. str_replace_by_df
##'
##' @title str_replace_by_df
##' @param q a vector that can be processed by stringr::str_replace_all
##' @param df An object of cl`ass data.frame or which can be converted to data.frame by as.data.frame.
##' @return a vector of the same length as q 
##' @author Marc Schwenzer
##' @importFrom stringr str_replace_all
##' @export
str_replace_by_df<- function(q,df)
    {
df %>% as.data.frame %>% {
    .[,2]-> y
    .[,1] -> names(y)
    return(y)
} -> rep.patterns
q %>% str_replace_all(rep.patterns)
}
# * Filevars
# Local Variables:
# orgstruct-heading-prefix-regexp: "# "
# eval: (orgstruct++-mode)
# eval: (orgstruct-hijacker-org-shifttab 1)
# End:
