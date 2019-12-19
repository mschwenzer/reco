context("Output class of vector")


test_that("Output class conversion", {
    expect_equal(1:10 %>% reco(tibble(from=1:10,to=2:11),class='character') %>% typeof, 'character')
    expect_equal(1:10 %>% reco(tibble(from=1:10,to=2:11),class='numeric') %>% class, 'numeric')
    expect_equal(1:10 %>% reco(tibble(from=1:10,to=2:11),class='real') %>% class, 'numeric')
    expect_equal(1:10 %>% reco(tibble(from=1:10,to=2:11),class='integer') %>% class, 'integer')
    expect_equal(1:10 %>% as.integer %>% reco(tibble(from=1:10,to=2:11),class='character') %>% typeof, 'character')
    expect_equal(1:10 %>% as.integer %>% reco(tibble(from=1:10,to=2:11),class='numeric') %>% class, 'numeric')
    expect_equal(1:10 %>% as.integer %>% reco(tibble(from=1:10,to=2:11),class='real') %>% class, 'numeric')
    expect_equal(1:10 %>% as.integer %>% reco(tibble(from=1:10,to=2:11),class='integer') %>% class, 'integer')
    expect_equal(1:10 %>% as.character %>% reco(tibble(from=1:10,to=2:11),class='character') %>% typeof, 'character')
    expect_equal(1:10 %>% as.character %>% reco(tibble(from=1:10,to=2:11),class='numeric') %>% class, 'numeric')
    expect_equal(1:10 %>% as.character %>% reco(tibble(from=1:10,to=2:11),class='real') %>% class, 'numeric')
    expect_equal(1:10 %>% as.character %>% reco(tibble(from=1:10,to=2:11),class='integer') %>% class, 'integer')
})


context("Unicode replacements")

context("NA")

test_that("NA", {
    expect_equal(1:10 %>% reco(tibble(from=1,to=NA),class='numeric'), c(NA,2:10))
        expect_equal(c(NA,2:10) %>% reco(tibble(from=NA,to=99),class='numeric'), c(99,2:10))
})


context("Unicode replacements")
