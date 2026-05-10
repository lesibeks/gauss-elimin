A <- matrix(c(4, 2, -1, 
              5, -3, 4, 
              1, 2, -2), nrow = 3, byrow = TRUE)

b <- c(1, -2, 0)

RowRed <- function(A, b){
  #spojime A a b do jednej rozširenej matice
  Ab <- cbind(A, b)
  #zistime dim(A), za predpokladu že A je štvorcova
  n <- nrow(Ab)

  #posuvame na na predposledni diagonalny prvok
  for (i in 1:(n-1)){
    #---mechanizmus na vymenu ridkov v pripade ak trapim na 0 na diagonale

    #tu sa zisti max v stlpci a porovna sa s "počitačovou" nulou
    pivot_indx <- which.max(abs(Ab[(i:n), i]))+i-1

    if (abs(Ab[pivot_indx, i]) < 1e-15){
      next
    }

    #samotna vymena
    if (pivot_indx != i){
      tmp_row <- Ab[i, ]
      Ab[i, ] <- Ab[pivot_indx, ]
      Ab[pivot_indx, ] <- tmp_row
    }
    #---
    #tu potom prebieha eliminacia(dolny riadok - prislušny nasobok horneho)
    for (j in (i+1):n){
      factor <- Ab[j, i] / Ab[i, i]
      Ab[j, ] <- Ab[j, ] - factor * Ab[i, ]
    }
  }
  #zapsmall() vracia 0 namiesto nekonečne maleho zlomku
  return(list(
    A_Red = zapsmall(Ab[, 1:n]),
    b_Red = zapsmall(Ab[, (n+1)])
    )
  )
}
red <- RowRed(A, b)


solve(A,b)
solve(red$A_Red, red$b_Red)