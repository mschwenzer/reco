library(testthat)
library(rec)
load('/dsk/torecode.rda')
data(isco08_de)
isco08_de[1:10,1]
torecode[400:800]
rec( torecode ,recodes=isco08_de,coder='benni')

"Medizinische Fachangestellte / \\Arzthelferin\\" %>% recoder.suggestion(isco08_de)
"Medizinische Fachangestellte / \\Arzthelferin\\" %>% rec(debug=1,recodes='isco08_de')
'Sachverständiger für KFZ Mechank (Dipl. Ing.)' %>% rec(debug=1,recodes='isco08_de')
'Lehrerin (Grund- und Hauptschule)' %>% rec(debug=1,recodes='isco08_de')
'' %>% rec(debug=1,recodes='isco08_de')
'' %>% recoder.suggestion(isco08_de,debug=1)

    c('Und','bla') %>% remove.seperator.words
grepl('[uU]nd',c('und','bla','und'))
str_detect(pattern='[uU]nd',c('und','bla','und'))
Grund-
recoder.suggestion( '\\'  ,isco08_de)
recoder.suggestion( 'Medizinische Fachangestellte /'  ,isco08_de)

recoder.suggestion
recoder.suggestion( 'Dach\\ Tisch'  ,isco08_de,debug=TRUE)
remove.brackets.and.colons('Dach \\ Tisch')
isco08_de[470,]

test_that('recoder.suggestion',
          {
          expect_false(try(recoder.suggestion( '\\'  ,isco08_de),silent=TRUE) %>% class=='try-error')
          expect_equal(recoder.suggestion( '\\'  ,isco08_de),NULL)
          expect_is(recoder.suggestion( 'Dach Tisch'  ,isco08_de),'data.frame')
          expect_error(recoder.suggestion(1:10  ,isco08_de))
          expect_error(recoder.suggestion(data.frame(as.character(1:10))  ,isco08_de))
          expect_error(recoder.suggestion(list('a','b')  ,isco08_de))
          expect_is(recoder.suggestion( 'ʛ'  ,isco08_de),'data.frame')          
      }
)

recoder.suggestion( 'ʛ'  ,isco08_de,debug=TRUE)
options(error=print)
recoder.suggestion( 'Dach \\ Tisch'  ,isco08_de,debug=1)

-> a
a <- TRUE
?expect_false(a)
?expect_true

!='try-error') %>% as.logical
expect_error(

