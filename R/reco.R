# * validate_string_and_return_class
# ##' validate_string_and_return_class.. content for \description{} (no empty lines) ..
# ##'
# ##' validate_string_and_return_class
# ##' @title validate_string_and_return_class
# ##' @param string 
# ##' @return 
# ##' @author Marc Schwenzer
validate_string_and_return_class<- function(string)
    {
        if(!is.vector(string)){stop('string has to be a vector.')}
        class(string)
        }
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

# * validate_df_structure_and_return_class
##' Check if the replacements has the correct format
##'
##' @title validate_df_structure
##' @param df a data.frame
##' @keywords internal
##' @return df 
##' @author Marc Schwenzer
##' @importFrom dplyr %>% 
validate_df_structure <- function(df)
    {
        
        df %>% map_chr(~class(.)) -> df.class
        if(ncol(df)<2){stop("The replacement table specified by replacements has to have at least 2 columns")}
        names(df) -> dfnames
        dfnames %>% str_detect('[Ff]rom') %>% which  -> fromcol
        dfnames %>% str_detect('[Tt]o') %>% which  -> tocol
        
        if(length(fromcol)==0)
        {
            ifelse(length(tocol)==0,1,ifelse(tocol==1,2,1)) -> fromcol
            cat(paste0('from colum not specified,setting to `',dfnames[fromcol],'`. '))
        }
        if(length(tocol)==0)
        {
            ifelse(length(fromcol)==0,1,ifelse(fromcol==1,2,1)) -> tocol
            cat(paste0('to colum not specified,setting to `',dfnames[tocol],'`\n'))
        }
        else{
            cat('\n')}
        df %>% select(fromcol,tocol)  %>% unique-> df
        c('from','to') -> names(df)
        duplicated(df$from) %>% which  -> duplicates
        if(length(duplicates)>0){
        print(df[duplicates,])
        duplicates %>% map(~{
            (df$from%in%df$from[.x]) %>% which -> conflicting.rows
            df$from[conflicting.rows] %>% unique -> conflicting.froms

            conflicting.froms %>% map_chr(~{
            (df$from%in%.x) %>% which -> confl.rows
#            print(confl.rows)
            paste0(confl.rows %>% df$to[.] ,collapse=', ') -> rep.str
            paste0('\n',((df$from[confl.rows])[1]),' -> ',rep.str)})
            })         %>% paste0(collapse='')     -> error
        
            paste0('Duplicate original value (from column in replacement data.frame) with conflicting replacement values (to column):',error) -> error
        stop(error,call.=FALSE)
        }
        list(df,df.class)
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
        file  %>% import  -> a.df
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
# * reco
#### also a replacement method for factors that changes the level order based on the occurence in the data.frame
##' Free and easy replace elements of a vector based on simple replacement data.frames. 
##'
##' Reco will replace the input based on a data.frame either from a file or an data.frame given to the replacements argument. It is basically a wrapper to the dplyr::recode-function that does some previous conversions to guarantee that the format of the file/data.frame matches to the input string. If you specify interactive it can be used to recode interactively by appending values to the file.
##' @Title reco
##' @param input A input vector having class `character`, `numeric`, `integer`, `factor`.
##' @param replacements The file where the replacements are located. Colums have to be named as from and to. If replacements is a data.frame, then this is used as replacement structure.
##' @param interactive If true an interactive coding session to add recodigs written to file, otherwise the recoding is done just base on the file leaving it as it is. The interactive coding session presents a table of alternatives. One can either enter a new string defining a new category to which the current value is assigned or enter a number that selects one of the previous categories. Entering 0 adds the current value as category. Entering '' (just return) ignores the current value.
##' @param tab Wheter to print the tab of alternatives/categories in every step.
##' @param class The desired class of the output vector. 
##' @return a vector.
##' @author Marc Schwenzer
##' @export
##' @importFrom purrr map
##' @importFrom dplyr %>% 

reco <- function(input=NULL,replacements,interactive=TRUE,tab=FALSE,class=NULL,not.matching=NULL)
{
class -> desired_output_class
rm(class)
    validate_string_and_return_class(input) -> input_class
    if(is.atomic(replacements))
       {
           if(replacements%in%(data(package='reco')$results[,'Item'] %>% c))
               {
               data(list=replacements,package='reco')  %>%  paste0('set_a.df(',.,')')  -> code
               eval(parse(text=code))
               interactive=FALSE
               }
           else
           {
               load_df_or_create_it(replacements)  %>% set_a.df
           }
       }
           else
               {
    if(is.data.frame(replacements))
       {
           replacements %>% as.data.frame %>% set_a.df
           interactive=FALSE
           }
    else
        {
            stop('No readable soruce to find replacements')
            }
    }
    a.df %>% validate_df_structure -> a.df
    a.df[[2]] -> a.df.class
    a.df[[1]] -> a.df
    if(is.null(input)){return(a.df)}

                                        # ◼  1. Guarantee a complete replacement data.frame exists
    if (interactive)
        {
            input %>% reco_filters  %>% unique  ->  x
            x%in%get_a.df()[,1] %>% sum %>% `-`(length(x),.)  %>% cat(.,' values to recode...\n')
                                        # add a reference to this environment to the x input
                                        # were to modify x
            x%>% map(~{reco_do(string=.x,tab=tab)})
        }
    ## ◼  2. replace
                                        # determine all classes



    
    input %>% class -> input_class
    a.df.class[1] -> df_from_class
    a.df.class[2] -> df_to_class
#        ifelse(is.null(class),df_to_class,class) -> desired_output_class
if(!is.null(desired_output_class))
    {
        if((df_to_class!=desired_output_class))
    {
        convert_to_class(a.df$to,desired_output_class) -> a.df$to
        desired_output_class -> df_to_class
    }}

                                        # helper function
    
                                        # convert from df to input_class
    input %>% convert_to_class(df_to_class) -> input

    a.df$from %>% convert_to_class(df_to_class) -> a.df$from
    a.df$from %>% class -> df_from_class
                                        # generate the code to evaluate dplyr::recode
if(is.null(desired_output_class)){a.df$to %>% class -> desired_output_class}
    generate_recode_code(a.df,df_from_class,desired_output_class,not.matching) -> code

    eval(parse(text=code))
    ## ◼  3 convert to desired output class       
#
 #
 #   }
#   else
#       {
#   
#}
#            paste0('`',origvals,'`=',replacements,'L',collapse=',')                 %>% str_replace('=NA','=NA_integer_')  %>% str_replace('`NA`=','.missing=')  -> code
#   code %>%  paste('input %>%  recode(.,',.,')  -> input')  -> code
#            eval(parse(text=code))
#            }
    input
}
# * str_replace_by_df
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
;    .[,1] -> names(y)
    return(y)
} -> rep.patterns
q %>% str_replace_all(rep.patterns)
}
# * DEPRECIATED reco_replace_character
reco_replace_character<- function(string,a.df,a.df.class)
    {
        if(a.df.class[1]=='numeric')
            {
                cat('converting `from` column: `numeric`→`character` (to match `string` of class `character`).\n')
                a.df %>% mutate(from=from %>% as.character) -> a.df
                }

        if(a.df.class[1]=='factor')
            {
                cat('converting `from` column: `factor`→`character` (to match `string` of class `character`).\n')                
                a.df %>% mutate(from=from %>% as.character) -> a.df
                }
        a.df[,2] -> replacements
        a.df[,1] -> names(replacements)
        replacements[string] -> string
        NULL -> names(string)
        }

reco_replace_numeric<- function(string,a.df,a.df.class)
    {
        if(a.df.class[1]=='character')
            {
                cat('converting `from` column: `character`→`numeric` (to match `string` of class `numeric`).\n')
                a.df[,2] %>% as.numeric -> replacements
}
        if(a.df.class[1]=='factor')
            {
                cat('converting `from` column: `factor`→`numeric` (to match `string` of class `numeric`).\n')
                a.df[,2] %>% as.numeric -> replacements
}
        replacements[string] -> string
        }




# * generate_recode_code: generate code for dplyr recode.
generate_recode_code<- function(a.df,df_from_class,desired_output_class,not.matching)
{
    # missing replacement for character

    paste0('`',
           a.df$from,'`='
          ,if(desired_output_class=='character'){'"'},
           a.df$to,
           if(desired_output_class=='integer'){'L'},
           if(desired_output_class=='character'){'"'},
           collapse=',') -> code
    # correct NA style
    code %>% str_replace_all(regex('=NA[L]*'),paste0('=',switch(desired_output_class,
                                                              numeric='NA_real_',
                                                              double='NA_real_',                                                              
                                                              integer='NA_integer_',
                                                              character='NA_character_')))  -> code
#    print(desired_output_class)
 #       print(not.matching)
    code %>% {ifelse(is.null(not.matching),
                    .,
                    . %>% str_replace('`NA`=','.missing='))} -> code
    
    code %>%  paste('input %>%  dplyr::recode(.,',.,')  -> input')  -> code
print(code)
    }



    convert_to_class<- function(a.input,class){switch(class,
               numeric={a.input %>% as.numeric -> a.input},
               character={a.input %>% as.character -> a.input},
               factor={a.input %>% as.factor -> a.input},
               integer={a.input %>% as.integer -> a.input},
               real={a.input %>% as.double -> a.input},               
               {stop("the class specified by reco's class argument is not recognized. Use either 'numeric', 'character', 'integer' or 'real'.")})
               return(a.input)
    }
# * Filevars
# Local Variables:
# orgstruct-heading-prefix-regexp: "# "
# eval: (orgstruct++-mode)
# eval: (orgstruct-hijacker-org-shifttab 1)
# End:
