context("posicoes")

norm.posc <- posicoes(Exemplo$dlm)$norm
int.posc <- posicoes(Exemplo$dlm)$int

test_that("Função deve retornar no formato correto", {
            expect_equal(class(norm.posc), "integer")
            expect_equal(class(int.posc) , "integer")
        })

test_that("Função deve retornar valores corretos", {
            expect_equivalent(norm.posc["Nivel"]     , 1)
            expect_equivalent(norm.posc["Inclinacao"], 2)
            expect_equivalent(norm.posc["Sazon"]     , 3)
            expect_equivalent(norm.posc["Coef1"]     , 6)
            expect_equivalent( int.posc["C.Nivel2"]  , 7)
          })
