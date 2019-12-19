# * Logic Overview
## main functino: reco

# * validate_string_and_return_class
# ##' validate_string_and_return_class.. content for \description{} (no empty lines) ..
# ##'
# ##' validate_string_and_return_class
# ##' @title validate_string_and_return_class
# ##' @param string a vector.
# ##' @return 
# ##' @author Marc Schwenzer
validate_string_and_return_class<- function(string)
    {
        if(!is.atomic(string)){stop('string has to be a vector.')}
        class(string)
        }
# * get_a.df
##' return a.df from reco env
##'
##' ##' get_a.df
##' @keywords internal
##' @param string a vector
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
##' @param string a vector
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
##' @importFrom purrr map
##' @importFrom purrr map_chr
##' @importFrom dplyr select
##' @importFrom stringr str_detect
##' @importFrom dplyr %>%
validate_df_structure <- function(df)
    {
        '' -> from_warning
        '' -> to_warning
        if(ncol(df)<2){stop("The replacement table specified by replacements has to have at least 2 columns")}
        names(df) -> dfnames
        dfnames %>% str_detect('[Ff]rom|^f *$') %>% which  -> fromcol
        dfnames %>% str_detect('[Tt]o|^t *$') %>% which  -> tocol
        
        if(length(fromcol)==0)
        {
            ifelse(length(tocol)==0,1,ifelse(tocol==1,2,1)) -> fromcol
            paste0('`from` colum not specified,setting to `',dfnames[fromcol],'`. ') -> from_warning
        }
        if(length(tocol)==0)
        {
            ifelse(length(fromcol)==0,1,ifelse(fromcol==1,2,1)) -> tocol
            paste0('`to` colum not specified,setting to `',dfnames[tocol],'`\n') -> to_warning
        }
        else{
            cat('')}
        c('from','to') -> names(df)
        df %>% select(fromcol,tocol)  %>% unique-> df
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
        df %>% map_chr(~class(.)) -> df.class
        if(df.class[2]=='logical')
            {
                df[,2] %>% convert_to_class('numeric') -> df[,2]
             'numeric' -> df.class[2]
                }
        # to be changed to attributes instead of list:
        # a.df %>% {df.class -> attr(.,'class');.} %>% {paste0(from_warning,to_warning,collapse='',sep='') -> attr(.,'warningn');.}
        list(df,df.class,paste0(from_warning,to_warning,collapse='',sep=''))
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
##' @param file A file
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
#        browser()
        file ->  attr(a.df,'file')
                }
    else
    {
        data.frame(from='from',to='to') %>% .[-1,] -> a.df
        a.df  %>% export(file=file,quote=TRUE)
        file ->  attr(x=a.df,which='file')
    }
    a.df
}


# * reco_do
##' reco_do
##'
##' decide if ask_string_write_to_file is necessary
##' @title reco_do
##' @param string a vector
##' @param a.df a data.frame
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
##' @param string a vector
##' @keywords internal
##' @return 
##' @author Marc Schwenzer
##' @importFrom dplyr bind_rows
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
    a.df.new %>% export(file=file,quote=TRUE)
    set_a.df(a.df.new)
    return()
                }
    if (new.string!='')
        {
    bind_rows(get_a.df(),
              data.frame(from=string,to=new.string)
              ) -> a.df.new
    file -> attr(a.df.new,'file')
    a.df.new %>% export(file=file,quote=TRUE)
    set_a.df(a.df.new)
    }
}

# * suggest_based_on_df
##' suggest_based_on_df
##'
##' suggest_based_on_df
##' @title suggest_based_on_df
##' @param string a vector
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
    if(length(input)>0)
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
# * ◼ reco
# ** TODO: also a replacement method for factors that changes the level order based on the occurence in the data.frame
# ** Roxygen
##' Free and easy replace elements of a vector based on simple replacement data.frames. 
##'
##' Reco will replace the input based on a data.frame either from a file or an data.frame given to the replacements argument. It is basically a wrapper to the dplyr::recode-function that does some previous conversions to guarantee that the format of the file/data.frame matches to the input string. If you specify interactive it can be used to recode interactively by appending values to the file.\cr
##' If only replacements without input is given, reco will return the replacement data.frame for convenience.
##' @title reco
##' @param input A input vector having class `character`, `numeric`, `integer`, `factor`.
##' @param replacements This argument defines how to replace values. The basic structure is a data.frame expected to have the columns f(rom) and t(o).\cr\cr
##' If replacements is not a data.frame, there are several options:\cr
##' - If replacements is a string with a path to a file readable by rio::import where the data.frame replacements are located.\cr
##' - If replacements is the string 'labels' it will use the attributes() function to extract the labels-attribute, effectively replacing values by there corresponding labels (which e.g. are generated automatically when importing from Stata, SPSS, etc.).\cr
##' - If replacements is a string that describes a data.frame in the reco package this will be used as replacement data.frame. See data(packages='reco') for the default data.frames.\cr
##' - If replacements is a list (not of class data.frame) its elements will be evaluated from left to right as individual replacement by recursive calling reco. In effect you can wrap several recoding steps in a list, e.g replacements=list('labels','cnt_cnt3)') to first extract the labels from a country variable and then recode it to a 3 digit code.
##' \cr
##' Note that you can use reco in combination with e.g. giving data.frame(from=c(1,2,3,4),to=c(NA,2,4,5)) or tibble::tribble(~f,~t,   1,NA,   2,4,   5,8) as argument to replacements to instantly replace values without using a seperate R object in memory or in a file. If adequately spaced, the comma seperated variant of tribble is quite readable and very fast to enter.\cr
##' \cr
##' If the data.frame does not have columns named from and to it will use the first or second column but warn you.\cr
##' @param interactive If true an interactive coding session to add recodigs written to file, otherwise the recoding is done just base on the file leaving it as it is. The interactive coding session presents a table of alternatives. One can either enter a new string defining a new category to which the current value is assigned or just enter a number that selects one of the previous categories. Entering 0 adds the current value as category. Entering '' (just return) ignores the current value.
##' @param tab Wheter to print the tab of alternatives/categories in every step.
##' @param reptab Wheter to print an overview of replacement tabs. Default TRUE but since it depends on stargazer you might decide turn it off in a batched sequence of replacements to save time or keep the output shorteri.
##' @param class The desired class of the output vector. 
##' @return a vector of same class as replaments' `to` column or specified by the `class` argument.
##' @author Marc Schwenzer
##' @export
##' @importFrom purrr map
##' @importFrom dplyr mutate_if
##' @importFrom dplyr %>% 
reco <- function(input=NULL,replacements,interactive=FALSE,tab=FALSE,class=NULL,not.matching=NULL,reptab=TRUE)
{
# ** init variables
'' -> the_source 
   

# ** ◼ Argument replacement: Switch based on various type and 
# *** Replacements is `list`
# **** {
if(replacements %>% is.list)
    {
# **** Option 1: replacements is `data.frame`: Use this data.frame as replacement table                    
        if(replacements %>% is.data.frame)
        {

                                        # Option 1: A data.frame given directly            
            paste0('data.frame `',paste0(deparse(substitute(replacements)),collapse=''),'`')  -> the_source
            interactive=FALSE
            replacements %>% as.data.frame %>% set_a.df
        }
        else
# **** Option 2: replacements is `list`, but not `data.frame`: recursively call reco
# ***** {            
        {
# ***** Repeatedly execute the reco function on input                     
# TODO: report the name (orig calling condition) when first calling
            while(TRUE)
                {


                    input %>% reco(replacements=replacements[[1]],interactive=interactive,tab=tab,not.matching=not.matching,reptab=reptab) -> input
#            print(replacements);print(length(replacements))
# ***** if replacements `list` contains only one remaining element quit the process and return new vector
                        if(length(replacements)==1){
# ****** Change the class of the output if specified in the `class` argument before returning vector
                            if(!is.null(class)){
                                if(class!=(class(input))){
                                    input %>% convert_to_class(desired_output_class) -> input
                                }
                            }
# ****** break from while by returning input and                             
                            return(input)}
                    if(length(replacements)>1){replacements[-1] -> replacements}
                }
# ****** Exit from the whole reco function because all list replacements-objects have been processesd
            return(input)
# ***** }
        }
        
# **** }        
    }
# *** replacements is not `list`
else
# *** {
    {
# *** replacements is `character`: matching df stored in reco package?, 'labels'?, file?
# **** {        
        if(replacements %>% is.character)
        {
# **** length(replacements==1)            
        if(length(replacements)==1)
# **** Option 3: A replacement data.frame stored in the reco package
            if(replacements%in%(data(package='reco')$results[,'Item'] %>% c))
            {
                paste0('default replacement data.frame `',replacements,'` from reco package')  -> the_source                                  
                data(list=replacements,package='reco')  %>%  paste0('set_a.df(',.,')')  -> code
                eval(parse(text=code))
                interactive=FALSE
            }
            else
        {
# **** Option 4: recode labels from attribute
            if(replacements=='labels')
            {

                if(!(is.null(input %>% attr('labels'))))
                {
#                browser()                    
                    input %>% attr('labels') -> labs
                    data.frame(from=labs,to=names(labs))  -> replacements
                    if((replacements[,1] %>% is.na %>% sum)>0)
                        {
                    replacements[,1] %>% is.na %>% which %>% {. -> a; replacements[-a,]} -> replacements
                    }
#                    browser()
                    replacements %>% set_a.df
                    '`labels`-attribute of input'  -> the_source
                    interactive=FALSE
                    }
                else
                {
                    stop('\nInput vector has no `labels` attribute.\n')
                }

            }
            else
            {
# **** Option 5: path to existing file
                paste0('replacement file `',replacements,'`')  -> the_source                                  
                load_df_or_create_it(replacements)  %>% set_a.df
            }
         }
        }
else
    {
# *** none of options 3-5 → error        
        stop("Argument `replacements` is of class `character`, but it's length is > 1.\nYou can use the path to a file, a default replacement data.frame, or the 'labels' option. Whatever you wanted to do, replacements needs to be of length 1. If you want to call reco recursively give a list of elements to replacements.")
        }
# *** }        
     }
# ** replacements given
# ** ◼ Validation of data.frame and class settings
class -> desired_output_class
rm(class)
####
validate_string_and_return_class(input) -> input_class
a.df %>% validate_df_structure -> a.df
a.df[[2]] -> a.df.class
a.df[[3]] -> a.df.warning    
a.df[[1]] -> a.df
# ** ◼ Switch to just view the data.frame
#    stop('No readable source to find replacements')
    if(is.null(input)){return(a.df)}

# ** Print info
input %>% reco_filters  %>% unique  ->  x
get_a.df()[,1]%in%    x -> matched.values
get('lhs',envir=parent.frame(6))  -> varname
varname %>% as.character -> varname
if(length(varname)>1){'input' -> varname}
cat(paste0('- reco> replace `',varname,'` based on ',the_source,'',if(a.df.warning[1]!=''){paste0('       ',a.df.warning)},collapse=''))
if(reptab){cat(':\n');visualize_replacement_table(replacements,matched.values=matched.values)}
    else{cat('.\n')}
# ** ◼  Interactive Coding session
    x%in%get_a.df()[,1]  %>% sum ->n_matched
            n_matched    %>% `-`(length(x),.) -> n_torecode
    if ((!interactive)&(n_torecode>0))
        {
            if(n_torecode) {
                cat(paste0('  !!! ',n_torecode,' value',if(n_torecode>1){'s'},' of ',length(x),' unique vals could not be matched, keeping orig vals:\n'))
                x[((x%in%get_a.df()[,1]) %>% `!` %>% which)] %>% unique %>%
                    {
                        . -> x
                        if(length(x)<10)
                        {
                            x[1:length(x)] -> vals
                        }
                        else
                            {
                                x[1:10] -> vals
                                }
                        vals %>% sort %>% paste0('`',.,'`',collapse=', ') %>% paste0('      e.g. ',.,'\n') %>% cat
}
            }
            }
    if (interactive&(n_torecode>0))
        {

            cat(paste0('- ',n_matched,' unique elements matched.\n'))
            cat(paste0('- ',n_torecode,' unique elements to recode.\n'))            
                                        # add a reference to this environment to the x input
                                        # were to modify x
            x%>% map(~{reco_do(string=.x,tab=tab)})
        }
# ** Prepare replacement frame and input to match logic of dplyr::recode
a.df %>% mutate_if(is.logical,as.double) -> a.df
# *** Set all classes
    input %>% class -> input_class
    a.df.class[1] -> df_from_class
    a.df.class[2] -> df_to_class
# *** set desired outputclass either by argument or to
    if(is.null(desired_output_class)){a.df$to %>% class -> desired_output_class}

# *** save nas in from and to previous the conversion (1)
is.na(a.df$from) %>% which -> orig.nas.in.from
is.na(a.df$to) %>% which -> orig.nas.in.to
# *** ◼ Convert input and df according to type to match dplyr::recode
# **** input:char

if(is.character(input))
    {
# ***** 1 input:char, from:char, to:char
                                        # do nothing        
# ***** 2 input:char, from:numeric, to:numeric
if((df_from_class=='numeric')&(df_to_class=='numeric'))
    {
    input %>% convert_to_class(df_from_class) -> input

        }
# ***** 3 input:char, from:numeric, to:character
if((df_from_class=='numeric')&(df_to_class=='character'))
    {
    a.df$from %>% convert_to_class('character') -> a.df$from
        }
# ***** 4 input:char, from:character, to:numeric
if((is.numeric(a.df$from))&(is.character(a.df$to)))
    {
    a.df$to %>% convert_to_class('character') -> a.df$to
        }
        }


# **** input numeric
if(is.numeric(input))
    {
# ***** 5 input:numeric, from:numeric, to:character
if((is.numeric(a.df$from))&(is.character(a.df$to)))
    {
#            print('here')
    a.df$from %>% convert_to_class('character') -> a.df$from
    input %>% convert_to_class('character') -> input

        }
# ***** 6 input:numeric, from:character, to:character
if((is.character(a.df$from))&(is.character(a.df$to)))
    {
    input %>% convert_to_class('character') -> input
    input %>% convert_to_class('character') -> input    
        }
# ***** 7 input:numeric, from:numeric, to:numeric
if((is.numeric(a.df$from))&(is.numeric(a.df$to)))
    {
#print('here')
if(class(a.df$from)!=class(a.df$to))
    {
        a.df$from  %>% convert_to_class(class(a.df$to)) -> a.df$from
        input  %>% convert_to_class(class(a.df$from)) -> input
        }
    
                                      }  # do nothing
# ***** 8 input:numeric, from:character, to:numeric        
if((is.character(a.df$from))&(is.numeric(a.df$to)))
    {
    a.df$from %>% convert_to_class('character') -> a.df$from
        }
    }

# ***** guarantee_numeric_are_same_class
guarantee_numeric_are_same_class(input,a.df) -> a.df
a.df[[1]] -> input
a.df[[2]] -> a.df
    
# *** remove nas in from generated by conversion (2)
is.na(a.df$to) %>% which -> current.nas_to
is.na(a.df$from) %>% which -> current.nas_from
#        print(new.nas.in.to)
#((current.nas%in%orig.nas.in.to)&(current.nas%in%orig.nas.in.from)) %>% `!` %>% which %>% current.nas[.]  -> to.drop.from.a.df
current.nas_from%in%orig.nas.in.from    %>% `!` %>% which %>% current.nas_from[.]  -> to.drop.from.a.df
                                        #    browser()
if(length(to.drop.from.a.df)>0){
a.df[-to.drop.from.a.df,] -> a.df
}
# *** Reset all classes
    input %>% class -> input_class
    a.df.class[1] -> df_from_class
    a.df.class[2] -> df_to_class

# ** generate the code to evaluate dplyr::recode and replace the values
    generate_recode_code(a.df,df_from_class,df_to_class,not.matching) -> code
#        cat(paste0('\nin: ',(class(input)),'\n'))
#    print(code)

    eval(parse(text=code))
#        cat(paste0('\nout: ',(class(input)),'\n'))
# ****** Change the class of the output if specified in the `class` argument before returning vector
    if(desired_output_class!=(class(input))){
    input %>% convert_to_class(desired_output_class) -> input
    }
    return(input)
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
##' reco_replace_character
##'
##' reco_replace_character
##' @title reco_replace_character
##' @param string a vector.
##' @param a.df a data.frame
##' @param a.df.class class of data.frame
##' @importFrom dplyr mutate
##' @return 
##' @author Marc Schwenzer
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




# * generate_recode_code: generate code for dplyr::recode.
##' generate_recode_code
##'
##' generate code for dplyr::recode.
##' @title generate_recode_code
##' @param a.df a data.frame
##' @param df_from_class class of from column in data.frame 
##' @param df_to_class class of to column in data.frame
##' @param not.matching elements in replacements that was not in the orig vector
##' @importFrom stringr str_replace_all
##' @return 
##' @author Marc Schwenzer
generate_recode_code<- function(a.df,df_from_class,df_to_class,not.matching)
{
    # missing replacement for character
    paste0('`',
           a.df$from,'`='
          ,if(df_to_class=='character'){'"'},
           a.df$to,
           if(df_to_class=='integer'){'L'},
           if(df_to_class=='character'){'"'},
           collapse=',') -> code
    # correct NA style
    code %>% str_replace_all(regex('="*NA[L]*"*,'),paste0('=',switch(df_to_class,
                                                              numeric='NA_real_,',
                                                              double='NA_real_,',                                                              
                                                              integer='NA_integer_,',
                                                              character='NA_character_,'))) %>%
str_replace_all(regex('="*NA[L]*"*$'),paste0('=',switch(df_to_class,
                                                              numeric='NA_real_',
                                                              double='NA_real_',                                                              
                                                              integer='NA_integer_',
                                                              character='NA_character_'))) %>%

    str_replace_all('`NA`=','.missing=')-> code
#    print(code)
#    print(df_to_class)
 #       print(not.matching)
    code %>% {ifelse(is.null(not.matching),
                    .,
                    . )} -> code

    code %>%  paste('input %>%  dplyr::recode(.,',.,')  -> input')  -> code
code
    }


##' convert_to_class
##'
##' Convert a input vector to according class. (Internal helper function of reco.)
##' @title convert_to_class
##' @param a.input a vector
##' @param class a class
##' @return the vector transformed to class
##' @author Marc Schwenzer
convert_to_class<- function(a.input,class){
        switch(class,
               numeric={a.input %>% as.numeric -> a.input},
               character={a.input %>% as.character -> a.input},
               factor={a.input %>% as.factor -> a.input},
               integer={a.input %>% as.integer -> a.input},
               real={a.input %>% as.double -> a.input},               
               {stop("the class specified by reco's class argument is not recognized. Use either 'numeric', 'character', 'integer' or 'real'.")})
               return(a.input)
    }



guarantee_numeric_are_same_class <- function(input,a.df)
    {
#        browser()
        input %>% is.double -> inputdbl
        a.df$from %>% is.double -> adffromdbl
        a.df$to %>% is.double -> adftodbl
        if(any(inputdbl,adffromdbl,adftodbl))
            {
                if((!inputdbl)&is.numeric(input)){input %>% as.double -> input}
                if((!adffromdbl)&is.numeric(a.df$from)){a.df$from %>% as.double -> a.df$from}                                    
                if((!adftodbl)&is.numeric(a.df$to)){a.df$to %>% as.double -> a.df$to}
            }
    return(list(input,a.df))
    }
# * visualize_replacement_table
##' visualize_replacement_table
##'
##' Visualizer for replacemenent table based on stargazer
##' @title visualize_replacement_table
##' @param replacements the replacements data.frame
##' @param type horizontal or vertical based on tab argument of reco
##' @return 
##' @author Marc Schwenzer
##' @importFrom stringr str_trunc
##' @importFrom stargazer stargazer
##' @importFrom dplyr %>%
visualize_replacement_table<- function(replacements,type='vertical',matched.values=matched.values)
    {
options()$width -> n_dis_char

n_dis_char - 4
if(type=='horizontal'){
replacements %>% head(30) %>% transmute(f,` `='->',t) %>% as.matrix %>% stargazer(type='text')
}
# vertical
if(type=='vertical'){
capture.output(replacements %>% filter(matched.values) %>% transmute(f,`  `='|',` `='v',t) %>% t %>% stargazer(type='text',rownames=FALSE)  ) %>% .[-c(1:2,length(.))]  -> output
output %>% nchar %>% max -> n_char_max
# *** If number display chars are lower than the string path cut into two pieces and truncate middle values
if((n_char_max )>n_dis_char)
{
paste0('  ',output %>% stringr::str_trunc((n_dis_char/2)-3,'right'),output %>% stringr::str_trunc((n_dis_char/2)-3,'left'),sep='') %>% paste0(.,collapse='\n') %>% paste0('\n') %>% cat
             cat('\n')
}
else{
         paste0('  ',output) %>% cat(sep='\n')
         cat('\n')
        }
}
return()
}
# * Filevars
# Local Variables:
# orgstruct-heading-prefix-regexp: "# "
# eval: (orgstruct++-mode)
# eval: (orgstruct-hijacker-org-shifttab 1)
# End:
