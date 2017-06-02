context("teste Normalidade")

nt <- teste.normalidade(Exemplo$e)

test_that("teste normalidade deve retornar valores no formato correto", {
            expect_equal(class(nt), "data.frame")
        })

test_that("teste normalidade deve retornar valores corretos", {
            expect_equal(nt, Exemplo$nt)
        })
