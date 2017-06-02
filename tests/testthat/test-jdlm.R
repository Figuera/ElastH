context("jdlm")

n <- 50
dlm <- build.dlm()
jdlm <- jdlm(dlm, n)

test_that("Função deve retornar valores no formato correto", {
            expect_equal(class(jdlm), "list")
        })

test_that("Componente devem ser correto quando são constantes no tempo", {
            expect_equal(jdlm$FF, rep(list(dlm$FF), n))
            expect_equal(jdlm$GG, rep(list(dlm$GG), n))
            expect_equal(jdlm$V, rep(list(dlm$V), n))
            expect_equal(jdlm$W, rep(list(dlm$W), n))
        })

jdlm <- jdlm(Exemplo$dlm)

test_that("Componente devem ser correto quando são variantes no tempo", {
            expect_equal(lapply(jdlm$FF, "[", 1, 1:5, drop=F), rep(list(dlm$FF), nrow(Exemplo$dlm$X)))
            expect_equal(sapply(jdlm$FF, "[", 1, 6, drop=F), Exemplo$dlm$X[,1])
            expect_equal(lapply(jdlm$GG, "[", 1:5, 1:5), rep(list(dlm$GG), nrow(Exemplo$dlm$X)))
            expect_equal(sapply(jdlm$GG, "[", 1, 7, drop=F), Exemplo$dlm$X[,2])
            expect_equal(jdlm$V, rep(list(Exemplo$dlm$V), nrow(Exemplo$dlm$X)))
            expect_equal(jdlm$W, rep(list(Exemplo$dlm$W), nrow(Exemplo$dlm$X)))
        })
