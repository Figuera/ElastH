context("modelar")

modelo <- modelar(Exemplo$y, Exemplo$dlm, estrutura=list(p=3, loglike=Exemplo$loglike))

test_that("Função deve retornar valores no formato correto", {
            expect_equal(class(modelo), "mee")
            expect_equal(length(modelo), 15)
        })

test_that("Função deve retornar modelo correto", {
            expect_equal(modelo$dlm,    Exemplo$dlm)
        })

test_that("Função deve retornar valores corretos", {
            expect_equal(modelo$y,      Exemplo$y)
        })

#test_that("Função deve retornar filtros corretos", {
            #expect_equal(modelo$f,      Exemplo$f)
        #})

test_that("Função deve retornar componentes corretos", {
            expect_equal(modelo$comp,   Exemplo$comp)
        })

test_that("Função deve retornar tests t corretos", {
            expect_equal(modelo$tt,     Exemplo$tt)
        })

test_that("Função deve retornar choques corretos", {
            expect_equal(modelo$choques, Exemplo$choques)
        })

test_that("Função deve retornar intervenções corretas", {
            expect_equal(modelo$interv, Exemplo$interv)
        })

test_that("Função deve retornar erros de predição corretos", {
            expect_equal(modelo$e,      Exemplo$e)
        })

test_that("Função deve retornar testes corretos", {
            expect_equal(modelo$q,      Exemplo$q)
            expect_equal(modelo$h,      Exemplo$h)
            expect_equal(modelo$nt,     Exemplo$nt)
            expect_equal(modelo$aic,    Exemplo$aic)
        })
