context("teste H")

h <- teste.h(Exemplo$e, 7)

test_that("teste h deve retornar valores no formato correto", {
            expect_equal(class(h), "data.frame")
        })

test_that("teste h deve retornar valores corretos", {
            expect_equal(h, Exemplo$h)
        })

h <- teste.h(Exemplo$e, 39)

test_that("teste h deve retornar erro quando for necessário", {
            expect_equal(h, "Nao existem graus de liberdade o suficientes para o teste h")
        })
