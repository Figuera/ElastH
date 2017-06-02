context("choques")

choques <- choques(Exemplo$f)

test_that("Função deve retornar valores no formato correto", {
            expect_equal(class(choques), "list")
            expect_equal(length(choques), 2)
        })

test_that("Função deve retornar valores corretos", {
            expect_equal(choques$ch, Exemplo$choques$ch)
            expect_equal(choques$sd, Exemplo$choques$sd)
        })
