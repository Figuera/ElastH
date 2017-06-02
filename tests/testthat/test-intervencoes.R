context("Intervencoes")

intx <- intervencoes(Exemplo$choques.pre, Exemplo$dlm.pre)

test_that("Função deve retornar no formato correto", {
            expect_equal(class(intx), "matrix")
            expect_equal(ncol(intx), 2)
        })

test_that("Função deve retornar valores corretos", {
            expect_equivalent(intx, Exemplo$X.pre)
          })
