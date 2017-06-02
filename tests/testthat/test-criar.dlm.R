context("decompor")

modelo <- decompor(y = Exemplo$y, X=Exemplo$Hpib,
                    irregular="S", nivel="F", inclinacao="S",
                   sazon="F", regres="F", comeco=2005, fim=c(2014,4)) 

test_that("Função deve retornar valores no formato correto", {
            expect_equal(class(modelo), "mee")
        })

test_that("Função deve retornar modelo correto", {
            expect_equal(modelo$dlm   , Exemplo$dlm)
        })

#modelo2 <- modelar(Exemplo$y, Exemplo$dlm, estrutura=list(p=3, loglike=Exemplo$loglike))

#test_that("Função deve retornar modelo correto", {
            #expect_equal(modelo , modelo2)
        #})

modelo <- decompor(y = Exemplo$y.interv.complexa, Exemplo$Hpib, nivel="F")

test_that("Funão deve retornar valores no formato correto", {
            expect_equal(class(modelo), "mee")
        })

test_that("Função deve retornar intervenções corretas mesmo após duas tentativas", {
            expect_true(all(modelo$interv$periodo < 2012))
        })
