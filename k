































rm(list = ls())
options(digits = 10)

# Задание 1: 
a <- c(2, -3, 4, 1)      
b <- c(-6, 9, -12, -3)   
p <- c(3, 2, -1, 4)      


# ========== ЗАДАНИЕ 1а ==========
len_a <- sqrt(sum(a^2))
len_b <- sqrt(sum(b^2))
dot_ab <- sum(a * b)
cos_ab <- dot_ab / (len_a * len_b)

if(cos_ab > 0) angle_type <- "острый"
if(cos_ab < 0) angle_type <- "тупой"
if(cos_ab == 0) angle_type <- "прямой"
if(cos_ab == -1) angle_type <- "развёрнутый"  # straight

cat("1a. cos =", cos_ab, "\n")
cat("    Угол:", angle_type, "\n\n")

# ========== ЗАДАНИЕ 1b ==========
dot_pb <- sum(p * b)
p2 <- sum(p^2)
len_b <- sqrt(sum(b^2))
dot_ap <- sum(a * p)
len_a <- sqrt(sum(a^2))
len_p <- sqrt(sum(p^2))
cos_ap <- dot_ap / (len_a * len_p)

result_1b <- 12 * dot_pb * p + 83 * b * p2 - exp(1) * len_b * b + pi * cos_ap * a

cat("1b. Результат (вектор):\n")
print(result_1b)
cat("\n")




































# Задание 2: 
Q <- matrix(c(-1,2,0, 3,-1,0, 1,4,1), nrow=3, byrow=TRUE) 
A <- matrix(c(1,-3,5, 2,-1,6, 1,0,0), nrow=3, byrow=TRUE)  
I <- diag(3)  

# Матрица P 
P <- matrix(c(0,1,2, 0,1,2, 0,1,2, 0,1,2, -3,1,2, 4,1,2), nrow=3, byrow=FALSE)  

# ========== ЗАДАНИЕ 2a ==========
cat("2a. dim(P) =", dim(P), "\n\n")

# ========== ЗАДАНИЕ 2b ==========
result_2b <- -4 * Q + pi * A
cat("2b. -4Q + πA =\n")
print(result_2b)
cat("\n")

# ========== ЗАДАНИЕ 2c ==========
result_2c <- t(Q) %*% A - Q
cat("2c. Q^T * A - Q =\n")
print(result_2c)
cat("\n")

# ========== ЗАДАНИЕ 2d ==========
# install.packages("expm") 
library(expm)
Q5 <- Q %^% 5
cat("2d. Q^5 =\n")
print(Q5)
cat("\n")

# ========== ЗАДАНИЕ 2e ==========
det_Q <- det(Q)
det_Q5 <- det(Q5)
cat("2e. det(Q) =", det_Q, "\n")
cat("    det(Q^5) =", det_Q5, "\n")
cat("    Связь: det(Q^5) = det(Q)^5 =", det_Q^5, "\n\n")

# ========== ЗАДАНИЕ 2f ==========
Q_inv <- solve(Q)
cat("2f. Q^{-1} =\n")
print(Q_inv)
cat("\n    Проверка Q * Q^{-1} =\n")
print(Q %*% Q_inv)
cat("\n")

# ========== ЗАДАНИЕ 2g ==========
rank_P <- qr(P)$rank
cat("2g. rank(P) =", rank_P, "\n\n")

# ========== ЗАДАНИЕ 2h ==========
X <- solve(A - I) %*% (solve(Q) %*% t(A)) %*% solve(Q)
cat("2h. X =\n")
print(X)
cat("\n")






























# Задание 3: матрица 
A3 <- matrix(c(
  1,0,4,-2,1,1,
  2,3,4,2,0,-3,
  3,1,3,2,9,5,
  4,0,3,2,0,-5,
  5,7,1,-2,8,3,
  6,4,1,2,0,2
), nrow=6, byrow=TRUE) 


# ========== ЗАДАНИЕ 3 ==========
cat("========== ЗАДАНИЕ 3 ==========\n")

# Поиск самого длинного и короткого столбца
n_cols <- ncol(A3)
max_len <- -Inf
min_len <- Inf
longest_idx <- NA
shortest_idx <- NA

for (j in 1:n_cols) {
  col_len <- sqrt(sum(A3[, j]^2))
  if (col_len > max_len) {
    max_len <- col_len
    longest_idx <- j
  }
  if (col_len < min_len) {
    min_len <- col_len
    shortest_idx <- j
  }
}

cat("Самый длинный столбец:", longest_idx, "длина =", max_len, "\n")
cat("Самый короткий столбец:", shortest_idx, "длина =", min_len, "\n")

# Ортогональные столбцы
cat("\nОртогональные столбцы:\n")
found <- FALSE
for (i in 1:(n_cols-1)) {
  for (j in (i+1):n_cols) {
    if (abs(sum(A3[, i] * A3[, j])) < 1e-10) {
      cat("Столбцы", i, "и", j, "\n")
      found <- TRUE
    }
  }
}
if(!found) cat("Нет\n")

# Ортогональные строки
cat("\nОртогональные строки:\n")
n_rows <- nrow(A3)
found <- FALSE
for (i in 1:(n_rows-1)) {
  for (j in (i+1):n_rows) {
    if (abs(sum(A3[i, ] * A3[j, ])) < 1e-10) {
      cat("Строки", i, "и", j, "\n")
      found <- TRUE
    }
  }
}
if(!found) cat("Нет\n")
