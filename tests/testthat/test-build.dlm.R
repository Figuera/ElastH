library(testthat)
context("Build dlm")

dlm <- build.dlm()

test_that("Função build.dlm deve retornar valores no formato correto", {
            expect_equal(class(dlm), "dlm")
        })

test_that("Função build.dlm deve retornar valores corretos", {
            expect_equal(dlm$FF, t(c(1, 0, 1, 0, 1)))
            expect_equal(round(dlm$GG), matrix(c(1,1, 0,0, 0,
                                                 0,1, 0,0, 0,
                                                 0,0, 0,1, 0,
                                                 0,0,-1,0, 0,
                                                 0,0, 0,0,-1),5,5, byrow=TRUE))
            expect_equal(dlm$W, diag(rep(1,5)))
            expect_equal(dlm$V, matrix(1))
            expect_equal(dlm$m0, rep(0,5))
            expect_equal(dlm$C0, diag(rep(1,5)))
        })

dlm <- build.dlm(y=c(10:14))

test_that("Função deve responder corretamente às mudanças de variâncias", {
            expect_equal(dlm$V, matrix(exp(10)))
            expect_equal(dlm$W[1,1], exp(11))
            expect_equal(dlm$W[2,2], exp(12))
            expect_equal(dlm$W[3,3], dlm$W[4,4])
            expect_equal(dlm$W[4,4], dlm$W[5,5])
            expect_equal(dlm$W[5,5], exp(13))
            expect_equal(dlm$C0, diag(rep(exp(14), 5)))
        })

dlm <- build.dlm(y=c(10:14), f=list(nivel="F", inclinacao="F", sazon="F", irregular="F", freq=4))

test_that("Função deve fixar parametros quando solicitado", {
            expect_equal(dlm$W, diag(rep(0,5)))
            expect_equal(dlm$V, matrix(0))
        })

dlm <- build.dlm(y=c(10:14), f=list(nivel="F", inclinacao="F", sazon="F", irregular="F", freq=4), pre.interv=T)

test_that("Função deve fixar parametros quando solicitado", {
            expect_equal(dlm$W, diag(c(rep(exp(-32),2), rep(0,3))))
            expect_equal(dlm$V, matrix(0))
        })
