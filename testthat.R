# * Library
library(
expect_equal(1:10,reco(1:10,data.frame(from=1:10,to=1:10)))
expect_equal(1:10 %>% class,reco(1:10,data.frame(from=1:10,to=1:10)) %>% class)

expect_equal(as.character(1:10),reco(1:10,data.frame(from=1:10,to=as.character(1:10))))
expect_equal(as.character(1:10),reco(1:10,data.frame(from=1:10,to=as.character(1:10))))
expect_equal(as.character(1:10) %>% class,reco(1:10,data.frame(from=1:10,to=as.character(1:10))) %>% class)

expect_equal(as.character(1:10) %>% class,reco(1:10 %>% as.character,data.frame(from=1:10,to=as.character(1:10))) %>% class)
# class determined by to, coercion when the input is character and the to ist integer
expect_equal(reco(1:10 %>% as.character,data.frame(from=1:10,to=as.character(1:10))) %>% class,'character')
# class determined by to, coercion when the input is integer and the to ist character
expect_equal(reco(1:10,data.frame(from=1:10 %>% as.character,to=as.character(1:10))) %>% class,'character')





# * New Test
# ** Define vectors of various type
# *** integer
c(1,2,3,4,5,6,7,8,9,10) -> an.integer.vector
# *** integer with NA
c(1,NA,3,4,5,6,7,8,9,10) -> an.integer.vector.with.NA
# *** real
c(1.4,2,3,4,5,6,7,8,9,10) -> an.real.vector
# *** real with NA
c(1.4,2,NA,4,5,6,7,8,9,10) -> an.real.vector.with.NA
list(an.integer.vector,an.real.vector,an.integer.vector.with.NA,an.real.vector.with.NA) -> from.variants
# ** Define vectors transformed plus one (for output)
from.variants %>% map(~{.x+1}) -> to.variants
#an.character.vector,an.character.vector.with.NA
# *** character
an.integer.vector -> an.character.vector
c('a','b') -> an.character.vector[3:4]
an.character.vector -> an.character.vector.with.NA
# *** character with NA
NA -> an.character.vector.with.NA[10]
an.character.vector
# add character vectors
from.variants %>% append(list(an.character.vector)) -> from.variants
an.integer.vector -> an.character.vector
c('a','b') -> an.character.vector[5:7]
an.character.vector -> an.character.vector.with.NA
# *** character with NA
NA -> an.character.vector.with.NA[3]
an.character.vector
to.variants %>% append(list(an.character.vector)) -> to.variants
# ** set variants



cross3(from=1:10 %>% as.character,to=as.character(1:10))
# ** Define to variants
c(1,2,3,4,5,6,7,8,9,10)+1 -> an.integer.vector
c(1.4,2,3,4,5,6,7,8,9,10)+1 -> an.real.vector
c(1.4,2,3,4,5,6,7,8,9,NA)+1 -> an.real.vector.with.NA
an.integer.vector -> an.character.vector
c(2,3,4,5,6,7,8,9,10,NA)+1 -> an.integer.vector.with.NA

c('a','b') -> an.character.vector[3:4]
an.character.vector[-10] %>% c(.,NA) ->     an.character.vector.with.na
list(an.integer.vector,an.real.vector,an.character.vector,an.integer.vector.with.NA,an.character.vector.with.NA,an.real.vector.with.NA) -> to.variants

# ** Define class variants
list('numeric','character','integer',NULL) -> class.variants

# ** 


# ** Tests for standard
input.variants %>% map(~{.x -> i
    data.frames %>% map(~{
        .x -> a.rep.frame
        reco(i,a.rep.frame) -> recoout
        expect_equal(class(a.rep.frame$to),class(recoout))
    })})

cross3(from.variants,to.variants,class.variants) -> variants
variants%>% map(~{1:10  %>% reco(replacements=tibble(from=.x[[1]],to=.x[[2]]),class==.x[[3]])}) -> outputs
variants %>% map(~{try(.x[[3]],silent=TRUE)}) %>% as.character %>% .[1:90] -> predefined.classes
predefined.classes
outputs %>% map_chr(~class(.)) %>% .[1:90]==predefined.classes

# ** Test all
from.variants %>% map(~{.x -> i
     to.variants %>% map(~{data.frame(from=i,to=.x)})}) -> data.frames
data.frames %>% flatten -> data.frames
class.variants %>% map(
         ~{              .x -> class.variant
                       input.variants %>% map(~{.x -> i
    data.frames %>% map(~{
        .x -> a.rep.frame
        cat('--------------------\ninput variant:',class(i),' rep.frame from:',class(a.rep.frame$from),' rep.frame to:',class(a.rep.frame$to),' class variant:',class.variant,'\n')
        reco(i,a.rep.frame,class=class.variant) -> recoout
        print(i)
                print(a.rep.frame)
        print(recoout)
        expect_equal(ifelse(!is.null(class.variant),class.variant,class(a.rep.frame$to)),class(recoout))
    })})})

# ** 
1:10 %>% reco(tribble(~from,~to,     1,2.4,  2,3, 4,9 , 5,9),class='character')
1:10 %>% reco(tribble(~from,~to,     1,NA,  2,'3', 4,'9' , 5,'9'),class='integer')

c(1:10,'heinz','gert',NA)  %>% reco(tribble(~from,~to,   3,NA,  4,'3', 2,'9' , 5,'9',   NA,'okay'),class='numeric')
c(1:10,'heinz','gert',NA)  %>% reco(tribble(~from,~to,   3,NA,  4,'3', 2,'9' , 5,'9',   NA,'okay'),class='character')

c(1:10,'heinz','gert',NA)  %>% reco(tribble(~from,~to,   3,NA,  4,'3', 2,'9' , 5,'9',   NA,'okay'),class='character')

c(1:10)  %>% reco(tribble(~from,~to, 'onkel',4 , 'heinz',3,  '4',3, 2,9 , 5,9,   NA,999),class='numeric') 
1:10 %>% reco(testfr)
tribble(~from,~to,   '3',3,  'heinz',3, 2,9 , 5,9,   NA,999) %>% map(~class(.))


testfr<- data.frame(from=from.variants[[3]],to=to.variants[[4]])
testfr
1:10 %>% reco(testfr)

testoutclass<- function(x,repl){expect_equal(class(repl$to),reco(x,repl) %>% class)}




# * Test: convert_to_class
c('character','integer','numeric','double') -> class.variants
class.variants %>% map(~{convert_to_class(1:10,.x) %>% class})
# * Tests for class equality
# ** Define testout: test that class is the same as class of to
testoutclass<- function(x,repl){expect_equal(class(repl$to),reco(x,repl) %>% class)}
# ** Default
# *** from is character, to is character, 
data.frame(from=1:10 %>% as.character,to=as.character(1:10)) -> repl
                                        # orig is integer
1:10  %>% testoutclass(repl)
1:10 %>% as.character %>% testoutclass(repl)
1:10 %>% as.factor %>% testoutclass(repl)
# !!!! ◼  factors not working by now

# *** from is integer, to is intger
data.frame(from=1:10,to=1:10) -> repl
                                        # orig is integer
1:10  %>% testoutclass(repl)
                                        # orig is character
1:10 %>% as.character %>% testoutclass(repl)
#1:10 %>% as.factor %>% testoutclass(repl)
# *** from is intger, to is character
data.frame(from=1:10,to=(1:10 %>% as.character)) -> repl
                                        # orig is integer
1:10  %>% testoutclass(repl) 
                                        # orig is character
1:10 %>% as.character %>% testoutclass(repl)
#1:10 %>% as.factor %>% testoutclass(repl)
# *** from is character, to is integer
data.frame(from=1:10 %>% as.character,to=(1:10)) -> repl
                                        # orig is integer
1:10  %>% testoutclass(repl) 
                                        # orig is character
1:10 %>% as.character %>% testoutclass(repl)
#1:10 %>% as.factor %>% testoutclass(repl)



# ** The same base on the class parameter
testout_withclass<- function(x,repl,toclass){expect_equal(toclass,reco(x,repl,class=toclass) %>% class)}
# *** from is character, to is character, 
data.frame(from=1:10 %>% as.character,to=as.character(1:10)) -> repl
                                        # orig is integer
1:10  %>% testout_withclass(repl,'character')
1:10  %>% testout_withclass(repl,'integer')
# ❗️ error
1:10 %>% as.character %>% testoutclass(repl)
1:10 %>% as.factor %>% testoutclass(repl)
# !!!! ◼  factors not working by now


# * Test for missingness
# ** Define test no: test that class is the same as class of to
testoutclass<- function(x,repl){expect_equal(class(repl$to),reco(x,repl) %>% class)}

# ** missingness in orig

c(1,2,3,NA,5,6,7,8,9,10) %>% reco(repl)
# * Emacs Filevariables (Irrelevant to Non-Emacs-Users)
# Local Variables:
# orgstruct-heading-prefix-regexp: "# "
# eval: (orgstruct++-mode)
# eval: (orgstruct-hijacker-org-shifttab 1)
# End:
