# ============================================
# 💙 ПРИЛОЖЕНИЕ С МАСКОТОМ УНИВЕРСИТЕТА 💙
# 30 задач · R-код решения · маскот · снежинки
# ============================================
library(shiny)
library(ggplot2)
library(bslib)

# --------------------------------------------
# 🩵 ТЕМА ПОД МАСКОТА
# --------------------------------------------
mascot_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary   = "#4A6FA5",
  secondary = "#8FA9C9",
  success   = "#7BA7BC",
  base_font = font_google("Nunito"),
  heading_font = font_google("Nunito"),
  font_scale = 1.0
)

# --------------------------------------------
# 📚 СПИСКИ ЗАДАЧ (10 в каждом разделе)
# --------------------------------------------
math_tasks <- c(
  "Квадратное уравнение", "Факториал", "Простые числа",
  "НОД и НОК", "Числа Фибоначчи", "Сумма арифметической прогрессии",
  "Возведение в степень", "Логарифм", "Тригонометрия",
  "Площадь треугольника (Герон)"
)

stat_tasks <- c(
  "Описательные статистики", "Гистограмма распределения", "Корреляция",
  "Линейная регрессия", "Нормальное распределение", "Одновыборочный t-тест",
  "Доверительный интервал", "Квартили и боксплот", "Мода и медиана",
  "Коэффициент вариации"
)

fin_tasks <- c(
  "Сложный процент", "Аннуитетный платёж (ипотека)", "Дисконтирование",
  "NPV (чистая приведённая стоимость)", "IRR (внутренняя норма доходности)",
  "Период окупаемости", "Простые проценты", "Депозит с капитализацией",
  "Реальная доходность (Фишер)", "Будущая стоимость аннуитета"
)

# --------------------------------------------
# 🧠 ФУНКЦИЯ РЕШЕНИЯ — возвращает list(text, code)
# --------------------------------------------
parse_nums <- function(txt) {
  nums <- suppressWarnings(as.numeric(trimws(unlist(strsplit(txt, ",")))))
  nums[!is.na(nums)]
}

solve_task <- function(sub, input) {
  # ---- МАТЕМАТИКА ----
  if (sub == "Квадратное уравнение") {
    a <- input$a; b <- input$b; c <- input$c
    if (a == 0) {
      text <- sprintf("Уравнение: %g x^2 + %g x + %g = 0\n\nОшибка: a = 0, уравнение не квадратное.", a, b, c)
      code <- sprintf("a <- %g; b <- %g; c <- %g\n# a = 0 → не квадратное", a, b, c)
    } else {
      D <- b^2 - 4*a*c
      if (D > 0) {
        x1 <- (-b + sqrt(D)) / (2*a); x2 <- (-b - sqrt(D)) / (2*a)
        text <- sprintf("Уравнение: %g x^2 + %g x + %g = 0\nD = %g\n\nКорни:\n  x1 = %.4f\n  x2 = %.4f", a, b, c, D, x1, x2)
      } else if (D == 0) {
        x <- -b / (2*a)
        text <- sprintf("Уравнение: %g x^2 + %g x + %g = 0\nD = 0\n\nКорень: x = %.4f", a, b, c, x)
      } else {
        text <- sprintf("Уравнение: %g x^2 + %g x + %g = 0\nD = %g < 0\n\nДействительных корней нет.", a, b, c, D)
      }
      code <- sprintf("a <- %g; b <- %g; c <- %g\nD <- b^2 - 4*a*c\nif (D >= 0) {\n  x1 <- (-b + sqrt(D)) / (2*a)\n  x2 <- (-b - sqrt(D)) / (2*a)\n}", a, b, c)
    }
    return(list(text = text, code = code))
  }
  
  if (sub == "Факториал") {
    n <- input$n_fact
    if (n < 0 || n != round(n)) {
      return(list(text = "Ошибка: требуется целое неотрицательное число.",
                  code = "# Проверка: n >= 0 и целое"))
    }
    return(list(
      text = sprintf("Факториал %d! = %s", n, format(factorial(n), scientific = FALSE)),
      code = sprintf("n <- %d\nresult <- factorial(n)\nprint(result)", n)
    ))
  }
  
  if (sub == "Простые числа") {
    n <- input$n_prime
    if (n < 2) return(list(text = "Ошибка: граница должна быть >= 2.", code = "# n >= 2"))
    primes <- c()
    for (i in 2:n) {
      ok <- TRUE
      if (i > 2) for (j in 2:floor(sqrt(i))) if (i %% j == 0) { ok <- FALSE; break }
      if (ok) primes <- c(primes, i)
    }
    return(list(
      text = sprintf("Простые числа в [2; %d]:\n%s\n\nВсего: %d", n, paste(primes, collapse = ", "), length(primes)),
      code = sprintf("n <- %d\nprimes <- c()\nfor (i in 2:n) {\n  if (all(i %% 2:max(2, floor(sqrt(i))) != 0) || i == 2)\n    primes <- c(primes, i)\n}\nprint(primes)", n)
    ))
  }
  
  if (sub == "НОД и НОК") {
    a <- input$gcd_a; b <- input$gcd_b
    gcd_val <- function(x, y) { while (y != 0) { t <- y; y <- x %% y; x <- t }; x }
    g <- gcd_val(a, b); l <- a * b / g
    return(list(
      text = sprintf("Числа: a = %g, b = %g\n\nНОД(a, b) = %g\nНОК(a, b) = %g", a, b, g, l),
      code = sprintf("a <- %g; b <- %g\ngcd <- function(x, y) { while (y != 0) { t <- y; y <- x %% y; x <- t }; x }\nG <- gcd(a, b)\nL <- a * b / G", a, b)
    ))
  }
  
  if (sub == "Числа Фибоначчи") {
    n <- input$fib_n
    fib <- numeric(n)
    if (n >= 1) fib[1] <- 0
    if (n >= 2) fib[2] <- 1
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    return(list(
      text = sprintf("Первые %d чисел Фибоначчи:\n%s", n, paste(fib[1:n], collapse = ", ")),
      code = sprintf("n <- %d\nfib <- numeric(n)\nfib[1] <- 0; fib[2] <- 1\nfor (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]\nprint(fib)", n)
    ))
  }
  
  if (sub == "Сумма арифметической прогрессии") {
    a1 <- input$ar_a1; d <- input$ar_d; n <- input$ar_n
    an <- a1 + (n - 1) * d
    S <- n * (a1 + an) / 2
    return(list(
      text = sprintf("a1 = %g, d = %g, n = %g\n\na%d = %g\nS%d = %g", a1, d, n, n, an, n, S),
      code = sprintf("a1 <- %g; d <- %g; n <- %g\nan <- a1 + (n - 1) * d\nS <- n * (a1 + an) / 2", a1, d, n)
    ))
  }
  
  if (sub == "Возведение в степень") {
    base <- input$pow_base; exp <- input$pow_exp
    res <- base^exp
    return(list(
      text = sprintf("%g ^ %g = %.4f", base, exp, res),
      code = sprintf("base <- %g\nexp <- %g\nresult <- base^exp", base, exp)
    ))
  }
  
  if (sub == "Логарифм") {
    x <- input$log_x; base <- input$log_base
    if (x <= 0 || base <= 0 || base == 1) {
      return(list(text = "Ошибка: x > 0, base > 0, base ≠ 1.", code = "# Проверка условий"))
    }
    res <- log(x, base = base)
    return(list(
      text = sprintf("log_%g(%g) = %.6f", base, x, res),
      code = sprintf("x <- %g\nbase <- %g\nresult <- log(x, base = base)", x, base)
    ))
  }
  
  if (sub == "Тригонометрия") {
    deg <- input$trig_deg
    rad <- deg * pi / 180
    return(list(
      text = sprintf("Угол: %g°\n\nsin = %.6f\ncos = %.6f\ntg  = %.6f", deg, sin(rad), cos(rad), tan(rad)),
      code = sprintf("deg <- %g\nrad <- deg * pi / 180\nsin(rad); cos(rad); tan(rad)", deg)
    ))
  }
  
  if (sub == "Площадь треугольника (Герон)") {
    a <- input$her_a; b <- input$her_b; c <- input$her_c
    p <- (a + b + c) / 2
    if (p <= 0 || p <= a || p <= b || p <= c) {
      return(list(text = "Ошибка: треугольник с такими сторонами не существует.",
                  code = "# Проверка неравенства треугольника"))
    }
    S <- sqrt(p * (p - a) * (p - b) * (p - c))
    return(list(
      text = sprintf("Стороны: a = %g, b = %g, c = %g\nПолупериметр: p = %g\n\nПлощадь: S = %.4f", a, b, c, p, S),
      code = sprintf("a <- %g; b <- %g; c <- %g\np <- (a + b + c) / 2\nS <- sqrt(p*(p-a)*(p-b)*(p-c))", a, b, c)
    ))
  }
  
  # ---- СТАТИСТИКА ----
  if (sub == "Описательные статистики") {
    x <- parse_nums(input$data_desc)
    if (length(x) < 2) return(list(text = "Ошибка: нужно >= 2 чисел.", code = "# Нужно >= 2 чисел"))
    return(list(
      text = sprintf("n = %d\nДанные: %s\n\nСреднее:       %.4f\nМедиана:       %.4f\nСт. отклонение: %.4f\nДисперсия:     %.4f\nМин / Макс:    %g / %g\nРазмах:        %g",
                     length(x), paste(x, collapse = ", "), mean(x), median(x), sd(x), var(x), min(x), max(x), max(x) - min(x)),
      code = sprintf("x <- c(%s)\nmean(x); median(x); sd(x); var(x)\nmin(x); max(x); max(x) - min(x)", paste(x, collapse = ", "))
    ))
  }
  
  if (sub == "Гистограмма распределения") {
    return(list(
      text = sprintf("Параметры распределения:\n  n = %d\n  μ = %g\n  σ = %g\n\nГрафик построен выше.",
                     input$n_hist, input$mean_hist, input$sd_hist),
      code = sprintf("set.seed(42)\ndata <- rnorm(%d, mean = %g, sd = %g)\nhist(data, col = '#8FA9C9', border = 'white')",
                     input$n_hist, input$mean_hist, input$sd_hist)
    ))
  }
  
  if (sub == "Корреляция") {
    x <- parse_nums(input$x_cor); y <- parse_nums(input$y_cor)
    if (length(x) != length(y) || length(x) < 2)
      return(list(text = "Ошибка: наборы X и Y должны быть одной длины (>= 2).", code = "# Проверка длин"))
    r <- cor(x, y)
    interp <- if (abs(r) > 0.7) "сильная" else if (abs(r) > 0.3) "умеренная" else "слабая"
    return(list(
      text = sprintf("X: %s\nY: %s\n\nr Пирсона = %.4f\nСвязь: %s", paste(x, collapse = ", "), paste(y, collapse = ", "), r, interp),
      code = sprintf("x <- c(%s)\ny <- c(%s)\nr <- cor(x, y)\nprint(r)", paste(x, collapse = ", "), paste(y, collapse = ", "))
    ))
  }
  
  if (sub == "Линейная регрессия") {
    x <- parse_nums(input$reg_x); y <- parse_nums(input$reg_y)
    if (length(x) != length(y) || length(x) < 2)
      return(list(text = "Ошибка: наборы X и Y должны быть одной длины (>= 2).", code = "# Проверка длин"))
    fit <- lm(y ~ x)
    b0 <- coef(fit)[1]; b1 <- coef(fit)[2]; r2 <- summary(fit)$r.squared
    return(list(
      text = sprintf("Линейная модель: y = %.4f + %.4f * x\n\nR² = %.4f\nУравнение объясняет %.2f%% дисперсии Y.",
                     b0, b1, r2, r2 * 100),
      code = sprintf("x <- c(%s)\ny <- c(%s)\nfit <- lm(y ~ x)\nsummary(fit)", paste(x, collapse = ", "), paste(y, collapse = ", "))
    ))
  }
  
  if (sub == "Нормальное распределение") {
    x <- input$norm_x; mu <- input$norm_mu; sig <- input$norm_sig
    d <- dnorm(x, mu, sig); p <- pnorm(x, mu, sig)
    return(list(
      text = sprintf("X ~ N(μ = %g, σ = %g)\n\nТочка: x = %g\nПлотность f(x) = %.6f\nP(X ≤ x) = %.6f", mu, sig, x, d, p),
      code = sprintf("mu <- %g; sigma <- %g; x <- %g\ndnorm(x, mu, sigma)\npnorm(x, mu, sigma)", mu, sig, x)
    ))
  }
  
  if (sub == "Одновыборочный t-тест") {
    x <- parse_nums(input$ttest_data); mu0 <- input$ttest_mu0
    if (length(x) < 2) return(list(text = "Ошибка: нужно >= 2 чисел.", code = "# >= 2 чисел"))
    t <- t.test(x, mu = mu0)
    return(list(
      text = sprintf("H0: μ = %g\nH1: μ ≠ %g\n\nt = %.4f, df = %d\np-value = %.4f\n\n%s",
                     mu0, mu0, t$statistic, t$parameter, t$p.value,
                     if (t$p.value < 0.05) "Отвергаем H0 (различия значимы)" else "Не отвергаем H0"),
      code = sprintf("x <- c(%s)\nt.test(x, mu = %g)", paste(x, collapse = ", "), mu0)
    ))
  }
  
  if (sub == "Доверительный интервал") {
    x <- parse_nums(input$ci_data); conf <- input$ci_level
    if (length(x) < 2) return(list(text = "Ошибка: нужно >= 2 чисел.", code = "# >= 2 чисел"))
    t <- t.test(x, conf.level = conf)
    return(list(
      text = sprintf("Уровень доверия: %.0f%%\nСреднее: %.4f\n\nДИ: [%.4f ; %.4f]",
                     conf * 100, mean(x), t$conf.int[1], t$conf.int[2]),
      code = sprintf("x <- c(%s)\nt.test(x, conf.level = %g)$conf.int", paste(x, collapse = ", "), conf)
    ))
  }
  
  if (sub == "Квартили и боксплот") {
    x <- parse_nums(input$quart_data)
    if (length(x) < 4) return(list(text = "Ошибка: нужно >= 4 чисел.", code = "# >= 4 чисел"))
    q <- quantile(x)
    return(list(
      text = sprintf("Квартили:\n  Q0 (min) = %g\n  Q1 = %g\n  Q2 (медиана) = %g\n  Q3 = %g\n  Q4 (max) = %g\n\nIQR = %g",
                     q[1], q[2], q[3], q[4], q[5], IQR(x)),
      code = sprintf("x <- c(%s)\nquantile(x)\nIQR(x)\nboxplot(x)", paste(x, collapse = ", "))
    ))
  }
  
  if (sub == "Мода и медиана") {
    x <- parse_nums(input$mode_data)
    if (length(x) < 2) return(list(text = "Ошибка: нужно >= 2 чисел.", code = "# >= 2 чисел"))
    ux <- unique(x); m <- ux[which.max(tabulate(match(x, ux)))]
    return(list(
      text = sprintf("Данные: %s\n\nМода:    %g\nМедиана: %.4f", paste(x, collapse = ", "), m, median(x)),
      code = sprintf("x <- c(%s)\nux <- unique(x)\nmode_val <- ux[which.max(tabulate(match(x, ux)))]\nmedian(x)", paste(x, collapse = ", "))
    ))
  }
  
  if (sub == "Коэффициент вариации") {
    x <- parse_nums(input$cv_data)
    if (length(x) < 2) return(list(text = "Ошибка: нужно >= 2 чисел.", code = "# >= 2 чисел"))
    cv <- sd(x) / mean(x) * 100
    return(list(
      text = sprintf("Среднее: %.4f\nСт. отклонение: %.4f\n\nCV = %.2f%%\n%s",
                     mean(x), sd(x), cv,
                     if (cv < 10) "Однородная совокупность" else if (cv < 25) "Умеренная вариация" else "Высокая вариация"),
      code = sprintf("x <- c(%s)\ncv <- sd(x) / mean(x) * 100\nprint(cv)", paste(x, collapse = ", "))
    ))
  }
  
  # ---- ФИНАНСЫ ----
  if (sub == "Сложный процент") {
    P <- input$P; r <- input$r_sp / 100; n <- input$n_sp
    S <- P * (1 + r)^n
    return(list(
      text = sprintf("Начальная сумма: %s руб.\nСтавка: %g%% годовых\nСрок: %g лет\n\nНаращенная сумма: %s руб.\nДоход: %s руб.",
                     format(P, big.mark = " "), input$r_sp, n,
                     format(round(S, 2), big.mark = " "), format(round(S - P, 2), big.mark = " ")),
      code = sprintf("P <- %g; r <- %g; n <- %g\nS <- P * (1 + r)^n\nS - P", P, r, n)
    ))
  }
  
  if (sub == "Аннуитетный платёж (ипотека)") {
    S <- input$S_mort; r <- input$r_mort / 100 / 12; n <- input$n_mort * 12
    pay <- if (r == 0) S / n else S * r / (1 - (1 + r)^(-n))
    total <- pay * n
    return(list(
      text = sprintf("Сумма кредита: %s руб.\nСтавка: %g%% годовых\nСрок: %g лет (%d мес.)\n\nЕжемесячный платёж: %s руб.\nВсего выплат: %s руб.\nПереплата: %s руб.",
                     format(S, big.mark = " "), input$r_mort, input$n_mort, n,
                     format(round(pay, 2), big.mark = " "),
                     format(round(total, 2), big.mark = " "),
                     format(round(total - S, 2), big.mark = " ")),
      code = sprintf("S <- %g; r <- %g/12; n <- %g*12\npay <- S * r / (1 - (1 + r)^(-n))\npay * n - S", S, input$r_mort / 100, input$n_mort)
    ))
  }
  
  if (sub == "Дисконтирование") {
    FV <- input$FV; r <- input$r_disc / 100; n <- input$n_disc
    PV <- FV / (1 + r)^n
    return(list(
      text = sprintf("Будущая стоимость: %s руб.\nСтавка: %g%%\nСрок: %g лет\n\nТекущая стоимость: %s руб.\nДисконт: %s руб.",
                     format(FV, big.mark = " "), input$r_disc, n,
                     format(round(PV, 2), big.mark = " "),
                     format(round(FV - PV, 2), big.mark = " ")),
      code = sprintf("FV <- %g; r <- %g; n <- %g\nPV <- FV / (1 + r)^n\nFV - PV", FV, r, n)
    ))
  }
  
  if (sub == "NPV (чистая приведённая стоимость)") {
    cf <- parse_nums(input$npv_cf); r <- input$npv_r / 100
    if (length(cf) < 1) return(list(text = "Ошибка: нужны денежные потоки.", code = "# Денежные потоки"))
    npv <- sum(cf / (1 + r)^(0:(length(cf) - 1)))
    return(list(
      text = sprintf("Денежные потоки: %s\nСтавка: %g%%\n\nNPV = %s руб.\n%s",
                     paste(cf, collapse = ", "), input$npv_r,
                     format(round(npv, 2), big.mark = " "),
                     if (npv > 0) "Проект эффективен (NPV > 0)" else "Проект неэффективен (NPV < 0)"),
      code = sprintf("cf <- c(%s)\nr <- %g\nnpv <- sum(cf / (1 + r)^(0:(length(cf)-1)))\nprint(npv)", paste(cf, collapse = ", "), r)
    ))
  }
  
  if (sub == "IRR (внутренняя норма доходности)") {
    cf <- parse_nums(input$irr_cf)
    if (length(cf) < 2) return(list(text = "Ошибка: нужно >= 2 потоков.", code = "# >= 2 потоков"))
    npv_f <- function(r) sum(cf / (1 + r)^(0:(length(cf) - 1)))
    tryCatch({
      irr <- uniroot(npv_f, c(-0.99, 10))$root
      return(list(
        text = sprintf("Денежные потоки: %s\n\nIRR = %.4f%%", paste(cf, collapse = ", "), irr * 100),
        code = sprintf("cf <- c(%s)\nnpv_f <- function(r) sum(cf / (1 + r)^(0:(length(cf)-1)))\nuniroot(npv_f, c(-0.99, 10))$root", paste(cf, collapse = ", "))
      ))
    }, error = function(e) {
      return(list(text = "Ошибка: IRR не найден.", code = "# Решение не найдено"))
    })
  }
  
  if (sub == "Период окупаемости") {
    cf <- parse_nums(input$pb_cf)
    if (length(cf) < 2 || cf[1] >= 0)
      return(list(text = "Ошибка: первый поток должен быть отрицательным (инвестиция).", code = "# cf[1] < 0"))
    cum <- cumsum(cf)
    if (all(cum < 0)) return(list(text = "Проект не окупается.", code = "# Не окупается"))
    pb <- which(cum >= 0)[1] - 1
    return(list(
      text = sprintf("Денежные потоки: %s\nНакопленный: %s\n\nСрок окупаемости: %d лет",
                     paste(cf, collapse = ", "), paste(cum, collapse = ", "), pb),
      code = sprintf("cf <- c(%s)\ncum <- cumsum(cf)\nwhich(cum >= 0)[1] - 1", paste(cf, collapse = ", "))
    ))
  }
  
  if (sub == "Простые проценты") {
    P <- input$sP; r <- input$sR / 100; n <- input$sN
    S <- P * (1 + r * n)
    return(list(
      text = sprintf("P = %s руб., r = %g%%, n = %g лет\n\nНачислено: %s руб.\nИтоговая сумма: %s руб.",
                     format(P, big.mark = " "), input$sR, n,
                     format(round(S - P, 2), big.mark = " "),
                     format(round(S, 2), big.mark = " ")),
      code = sprintf("P <- %g; r <- %g; n <- %g\nS <- P * (1 + r * n)", P, r, n)
    ))
  }
  
  if (sub == "Депозит с капитализацией") {
    P <- input$dep_P; r <- input$dep_r / 100; n <- input$dep_n; m <- input$dep_m
    S <- P * (1 + r/m)^(m * n)
    return(list(
      text = sprintf("Вклад: %s руб.\nСтавка: %g%%\nСрок: %g лет\nКапитализация: %d раз в год\n\nИтог: %s руб.\nДоход: %s руб.",
                     format(P, big.mark = " "), input$dep_r, n, m,
                     format(round(S, 2), big.mark = " "),
                     format(round(S - P, 2), big.mark = " ")),
      code = sprintf("P <- %g; r <- %g; n <- %g; m <- %g\nS <- P * (1 + r/m)^(m*n)", P, r, n, m)
    ))
  }
  
  if (sub == "Реальная доходность (Фишер)") {
    nom <- input$fish_nom / 100; inf <- input$fish_inf / 100
    real <- (1 + nom) / (1 + inf) - 1
    return(list(
      text = sprintf("Номинальная ставка: %g%%\nИнфляция: %g%%\n\nРеальная доходность: %.4f%%", input$fish_nom, input$fish_inf, real * 100),
      code = sprintf("nom <- %g; inf <- %g\nreal <- (1 + nom) / (1 + inf) - 1", nom, inf)
    ))
  }
  
  if (sub == "Будущая стоимость аннуитета") {
    PMT <- input$ann_PMT; r <- input$ann_r / 100; n <- input$ann_n
    FV <- PMT * ((1 + r)^n - 1) / r
    return(list(
      text = sprintf("Платёж: %s руб.\nСтавка: %g%%\nПериодов: %g\n\nБудущая стоимость: %s руб.",
                     format(PMT, big.mark = " "), input$ann_r, n,
                     format(round(FV, 2), big.mark = " ")),
      code = sprintf("PMT <- %g; r <- %g; n <- %g\nFV <- PMT * ((1 + r)^n - 1) / r", PMT, r, n)
    ))
  }
  
  list(text = "Задача не найдена.", code = "# Ошибка")
}

# --------------------------------------------
# UI
# --------------------------------------------
ui <- fluidPage(
  theme = mascot_theme,
  
  tags$head(
    tags$style(HTML("
      html { min-height: 100%; }
      body {
        background: linear-gradient(135deg, #A8C8E4 0%, #D4E6F5 40%, #C5D8EF 70%, #9FB8DC 100%);
        background-size: 200% 200%;
        background-attachment: fixed;
        animation: morningShift 35s ease-in-out infinite;
        color: #2C3E50;
        position: relative;
        overflow-x: hidden;
        min-height: 100vh;
      }
      @keyframes morningShift {
        0%   { background-position: 0% 0%; }
        50%  { background-position: 100% 100%; }
        100% { background-position: 0% 0%; }
      }
      body::before {
        content: '❄        ❅        ❆        ✻        ❄        ❅        ❆        ✻';
        position: fixed; top: -20%; left: -5%; width: 110%; height: 160%;
        font-size: 24px; color: rgba(74, 111, 165, 0.35);
        letter-spacing: 180px; line-height: 6.5em; word-spacing: 40px;
        pointer-events: none; z-index: 0;
        animation: floatDown1 45s linear infinite; text-align: justify;
      }
      @keyframes floatDown1 {
        0% { transform: translateY(0) translateX(0) rotate(0deg); }
        50% { transform: translateY(110px) translateX(30px) rotate(180deg); }
        100% { transform: translateY(220px) translateX(0) rotate(360deg); }
      }
      body::after {
        content: '💙      💙      💙      💙      💙      💙      💙      💙      💙';
        position: fixed; top: -10%; left: 0; width: 100%; height: 150%;
        font-size: 20px; opacity: 0.55;
        letter-spacing: 240px; line-height: 9em; word-spacing: 100px;
        pointer-events: none; z-index: 0;
        animation: floatDown2 55s linear infinite; text-align: justify;
      }
      @keyframes floatDown2 {
        0% { transform: translateY(0) translateX(0) rotate(-10deg); }
        50% { transform: translateY(140px) translateX(-25px) rotate(10deg); }
        100% { transform: translateY(280px) translateX(0) rotate(-10deg); }
      }
      html::before {
        content: '❄  💙  ❆  💙  ❅  💙  ✻  💙  ❄  💙  ❆  💙';
        position: fixed; top: -15%; left: 2%; width: 100%; height: 155%;
        font-size: 18px; opacity: 0.4;
        letter-spacing: 300px; line-height: 12em; word-spacing: 150px;
        pointer-events: none; z-index: 0;
        animation: floatDown3 70s linear infinite; text-align: justify;
      }
      @keyframes floatDown3 {
        0% { transform: translateY(0) translateX(0); }
        33% { transform: translateY(90px) translateX(20px); }
        66% { transform: translateY(180px) translateX(-20px); }
        100% { transform: translateY(270px) translateX(0); }
      }
      .container-fluid, .well, .main-panel, .app-header { position: relative; z-index: 1; }

      .mascot-header {
        background: rgba(255, 255, 255, 0.92);
        backdrop-filter: blur(6px);
        border-radius: 16px; padding: 20px 24px; margin-bottom: 20px;
        box-shadow: 0 4px 20px rgba(74,111,165,0.18);
        border: 1px solid rgba(212, 226, 239, 0.8);
        display: flex; align-items: center; gap: 20px;
        position: relative; overflow: hidden;
      }
      .mascot-header::before {
        content: '💙'; position: absolute; top: 8px; right: 14px;
        font-size: 22px; animation: pulse 2.5s ease-in-out infinite;
      }
      .mascot-header::after {
        content: '❄'; position: absolute; bottom: 8px; left: 14px;
        font-size: 22px; color: #4A6FA5; animation: spin 15s linear infinite;
      }
      @keyframes pulse { 0%,100% { transform: scale(1); } 50% { transform: scale(1.25); } }
      @keyframes spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }
      .mascot-header-text h1 { color: #2F4A6E; margin: 0; font-size: 1.6em; font-weight: 700; }
      .mascot-header-text h1::before { content: '❄  '; color: #6495ED; margin-right: 4px; }
      .mascot-header-text h1::after { content: '  💙'; }
      .mascot-header-text p { color: #5C7A99; margin: 6px 0 0 0; font-size: 0.95em; }
      .mascot-video { border-radius: 12px; box-shadow: 0 4px 12px rgba(74,111,165,0.2); flex-shrink: 0; }

      .badge-tasks {
        display: inline-block; padding: 4px 12px; border-radius: 12px;
        background: linear-gradient(135deg, #4A6FA5, #6B8FC4);
        color: #fff; font-size: 0.75em; font-weight: 600;
        margin-left: 10px; vertical-align: middle;
        box-shadow: 0 2px 6px rgba(74,111,165,0.3);
      }

      .well {
        background: rgba(255, 255, 255, 0.95) !important;
        border: 1px solid #D4E2EF !important;
        border-radius: 14px !important;
        box-shadow: 0 4px 14px rgba(74,111,165,0.10) !important;
        padding: 20px !important; position: relative; backdrop-filter: blur(4px);
      }
      .well::before {
        content: '💙'; position: absolute; top: 10px; right: 14px;
        font-size: 16px; opacity: 0.65;
      }
      .well h4 {
        color: #4A6FA5; font-weight: 700; font-size: 0.95em;
        text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 14px;
      }
      .well h4::before { content: '❄ '; font-size: 0.95em; color: #6495ED; }

      .btn-primary {
        background: linear-gradient(135deg, #4A6FA5, #6B8FC4) !important;
        border: none !important; border-radius: 10px !important;
        font-weight: 600; padding: 10px 22px !important;
        letter-spacing: 0.3px; transition: all 0.25s ease;
        box-shadow: 0 3px 10px rgba(74,111,165,0.3);
      }
      .btn-primary::before { content: '❄ '; }
      .btn-primary:hover {
        transform: translateY(-1px);
        box-shadow: 0 6px 18px rgba(74,111,165,0.5), 0 0 22px rgba(100, 149, 237, 0.55);
      }

      .form-control, .form-select {
        border-radius: 10px !important; border: 1.5px solid #D4E2EF !important;
        background: #FFFFFF !important; color: #2C3E50 !important;
      }
      .form-control:focus, .form-select:focus {
        border-color: #4A6FA5 !important;
        box-shadow: 0 0 0 3px rgba(74,111,165,0.15) !important;
      }
      label { color: #5C7A99 !important; font-weight: 600; font-size: 0.9em; }
      label::before { content: '❆ '; color: #6495ED; font-size: 0.85em; }

      #result {
        background: rgba(255, 255, 255, 0.97);
        padding: 18px 20px; border-radius: 12px;
        border: 1px solid #D4E2EF; border-left: 4px solid #4A6FA5;
        font-family: 'Consolas', 'Monaco', monospace;
        font-size: 14px; color: #2C3E50; min-height: 100px;
        box-shadow: 0 4px 14px rgba(74,111,165,0.08);
      }

      h3.section-title {
        color: #2F4A6E; font-weight: 700;
        border-bottom: 2px solid rgba(74,111,165,0.25);
        padding-bottom: 10px; margin-bottom: 20px; position: relative;
      }
      h3.section-title::before { content: '❄  '; color: #6495ED; }

      /* 📋 БЛОК R-КОДА — светлая тема */
      .code-block {
        background: rgba(255, 255, 255, 0.97);
        border-radius: 12px;
        padding: 0;
        margin-top: 16px;
        overflow: hidden;
        box-shadow: 0 4px 14px rgba(74, 111, 165, 0.15);
        border: 1px solid #D4E2EF;
      }
      .code-header {
        background: linear-gradient(90deg, #E8EFF5, #D4E2EF);
        color: #2F4A6E;
        padding: 10px 16px;
        font-size: 0.85em;
        font-weight: 700;
        letter-spacing: 0.5px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        border-bottom: 1px solid #D4E2EF;
      }
      .code-header .dots { display: inline-flex; gap: 6px; }
      .code-header .dot { width: 11px; height: 11px; border-radius: 50%; }
      .code-header .dot.r { background: #FF8B85; }
      .code-header .dot.y { background: #FFD580; }
      .code-header .dot.g { background: #8FE0A8; }
      .code-header .title {
        font-family: 'Consolas', 'Monaco', monospace;
        color: #2F4A6E; font-weight: 600; font-size: 0.95em;
      }
      .code-body {
        background: #FAFCFE;
        color: #2C3E50;
        padding: 16px 20px;
        font-family: 'Consolas', 'Monaco', monospace;
        font-size: 13.5px;
        line-height: 1.6;
        white-space: pre;
        overflow-x: auto;
        margin: 0;
        border-left: 3px solid #6B8FC4;
      }
      .code-body .kw { color: #7B3FA0; font-weight: 600; }
      .code-body .fn { color: #1F5FA0; font-weight: 600; }
      .code-body .str { color: #A0522D; }
      .code-body .num { color: #2E7D32; font-weight: 600; }
      .code-body .cmt { color: #8A97A6; font-style: italic; }

      .mascot-footer {
        text-align: center; margin-top: 40px; padding: 20px;
        color: #4A6FA5; font-size: 0.85em;
        border-top: 1px solid rgba(74,111,165,0.2);
      }
      .mascot-footer .paw { font-size: 1.3em; margin: 0 6px; color: #4A6FA5; }
      .mascot-footer .snow { color: #4A6FA5; margin: 0 8px; font-size: 1.15em; }
      .mascot-footer .heart { margin: 0 8px; font-size: 1.1em; }
      hr { border-color: rgba(74,111,165,0.2); }
    "))
  ),
  
  # 💙 ШАПКА С МАСКОТОМ
  div(class = "mascot-header",
      tags$video(
        src = "elk_cat.webm",
        width = "130", autoplay = NA, loop = NA, muted = NA,
        class = "mascot-video"
      ),
      div(class = "mascot-header-text",
          h1("Аналитическое приложение"),
          p("Математические расчёты · Статистический анализ · Финансовые модели")
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4(HTML("Раздел <span class='badge-tasks'>30 задач</span>")),
      selectInput("main_menu", NULL,
                  choices = c("Математические задачи (10)",
                              "Анализ данных и статистика (10)",
                              "Финансовые расчёты (10)")),
      
      uiOutput("sub_menu"),
      hr(),
      uiOutput("inputs"),
      
      br(),
      actionButton("run", "Выполнить расчёт",
                   class = "btn-primary", width = "100%",
                   icon = icon("play"))
    ),
    
    mainPanel(
      h3(textOutput("header"), class = "section-title"),
      verbatimTextOutput("result"),
      
      # 📊 Сначала график (если есть)
      conditionalPanel(
        condition = "input.sub == 'Гистограмма распределения'",
        plotOutput("plot")
      ),
      
      # 📋 Потом R-код решения
      uiOutput("code_block")
    )
  ),
  
  div(class = "mascot-footer",
      span(class = "snow", "❄"),
      span(class = "paw", "🐾"),
      span(class = "heart", "💙"),
      "Учебный проект · Разработано на R + Shiny",
      span(class = "heart", "💙"),
      span(class = "paw", "🐾"),
      span(class = "snow", "❄")
  )
)

# --------------------------------------------
# SERVER
# --------------------------------------------
server <- function(input, output) {
  
  # Подменю
  output$sub_menu <- renderUI({
    if (grepl("Матем", input$main_menu)) {
      selectInput("sub", "Задача:", choices = math_tasks)
    } else if (grepl("Анализ", input$main_menu)) {
      selectInput("sub", "Метод:", choices = stat_tasks)
    } else {
      selectInput("sub", "Расчёт:", choices = fin_tasks)
    }
  })
  
  # Динамические поля ввода
  output$inputs <- renderUI({
    req(input$sub)
    s <- input$sub
    
    if (s == "Квадратное уравнение") tagList(
      numericInput("a", "Коэффициент a", 1),
      numericInput("b", "Коэффициент b", -3),
      numericInput("c", "Коэффициент c", 2)
    )
    else if (s == "Факториал") numericInput("n_fact", "Число n (0–170)", 5, 0, 170, 1)
    else if (s == "Простые числа") numericInput("n_prime", "Граница поиска", 30, 2, step = 1)
    else if (s == "НОД и НОК") tagList(
      numericInput("gcd_a", "Число a", 24, step = 1),
      numericInput("gcd_b", "Число b", 36, step = 1)
    )
    else if (s == "Числа Фибоначчи") numericInput("fib_n", "Сколько чисел", 10, 2, 50, 1)
    else if (s == "Сумма арифметической прогрессии") tagList(
      numericInput("ar_a1", "Первый член a1", 2),
      numericInput("ar_d", "Разность d", 3),
      numericInput("ar_n", "Количество n", 10, 1, step = 1)
    )
    else if (s == "Возведение в степень") tagList(
      numericInput("pow_base", "Основание", 2),
      numericInput("pow_exp", "Показатель", 10)
    )
    else if (s == "Логарифм") tagList(
      numericInput("log_x", "Число x", 100, min = 0.001),
      numericInput("log_base", "Основание", 10, min = 0.001)
    )
    else if (s == "Тригонометрия") numericInput("trig_deg", "Угол (градусы)", 30)
    else if (s == "Площадь треугольника (Герон)") tagList(
      numericInput("her_a", "Сторона a", 3),
      numericInput("her_b", "Сторона b", 4),
      numericInput("her_c", "Сторона c", 5)
    )
    else if (s == "Описательные статистики") textAreaInput("data_desc", "Числа через запятую:", "12, 15, 18, 22, 25, 30, 35, 40, 45, 50", rows = 3)
    else if (s == "Гистограмма распределения") tagList(
      numericInput("n_hist", "Объём выборки", 200, 10),
      numericInput("mean_hist", "Среднее (μ)", 50),
      numericInput("sd_hist", "Ст. отклонение (σ)", 10, 0.1)
    )
    else if (s == "Корреляция") tagList(
      textAreaInput("x_cor", "Набор X:", "1, 2, 3, 4, 5, 6, 7, 8", rows = 2),
      textAreaInput("y_cor", "Набор Y:", "2, 4, 5, 4, 5, 7, 8, 9", rows = 2)
    )
    else if (s == "Линейная регрессия") tagList(
      textAreaInput("reg_x", "Набор X:", "1, 2, 3, 4, 5, 6, 7, 8", rows = 2),
      textAreaInput("reg_y", "Набор Y:", "2, 4, 5, 4, 5, 7, 8, 9", rows = 2)
    )
    else if (s == "Нормальное распределение") tagList(
      numericInput("norm_x", "Точка x", 50),
      numericInput("norm_mu", "Среднее μ", 50),
      numericInput("norm_sig", "σ", 10, 0.1)
    )
    else if (s == "Одновыборочный t-тест") tagList(
      textAreaInput("ttest_data", "Выборка:", "12, 15, 18, 22, 25, 30, 35, 40, 45, 50", rows = 3),
      numericInput("ttest_mu0", "Гипотетическое μ0", 30)
    )
    else if (s == "Доверительный интервал") tagList(
      textAreaInput("ci_data", "Выборка:", "12, 15, 18, 22, 25, 30, 35, 40, 45, 50", rows = 3),
      numericInput("ci_level", "Уровень (0.9 / 0.95 / 0.99)", 0.95, 0.5, 0.999, 0.01)
    )
    else if (s == "Квартили и боксплот") textAreaInput("quart_data", "Выборка (>= 4 чисел):", "12, 15, 18, 22, 25, 30, 35, 40, 45, 50", rows = 3)
    else if (s == "Мода и медиана") textAreaInput("mode_data", "Выборка:", "2, 3, 3, 5, 7, 7, 7, 9, 10, 10", rows = 3)
    else if (s == "Коэффициент вариации") textAreaInput("cv_data", "Выборка:", "12, 15, 18, 22, 25, 30, 35, 40, 45, 50", rows = 3)
    else if (s == "Сложный процент") tagList(
      numericInput("P", "Начальная сумма (руб.)", 100000, 0),
      numericInput("r_sp", "Ставка (% годовых)", 10, 0),
      numericInput("n_sp", "Срок (лет)", 5, 1)
    )
    else if (s == "Аннуитетный платёж (ипотека)") tagList(
      numericInput("S_mort", "Сумма кредита (руб.)", 3000000, 0),
      numericInput("r_mort", "Годовая ставка (%)", 12, 0),
      numericInput("n_mort", "Срок (лет)", 20, 1)
    )
    else if (s == "Дисконтирование") tagList(
      numericInput("FV", "Будущая стоимость (руб.)", 150000, 0),
      numericInput("r_disc", "Ставка (%)", 10, 0),
      numericInput("n_disc", "Период (лет)", 3, 1)
    )
    else if (s == "NPV (чистая приведённая стоимость)") tagList(
      textAreaInput("npv_cf", "Денежные потоки (через запятую):", "-100000, 30000, 40000, 50000, 30000", rows = 2),
      numericInput("npv_r", "Ставка дисконт. (%)", 10)
    )
    else if (s == "IRR (внутренняя норма доходности)") textAreaInput("irr_cf", "Денежные потоки:", "-100000, 30000, 40000, 50000, 30000", rows = 2)
    else if (s == "Период окупаемости") textAreaInput("pb_cf", "Денежные потоки (первый — инвестиция):", "-100000, 30000, 40000, 50000, 30000", rows = 2)
    else if (s == "Простые проценты") tagList(
      numericInput("sP", "Сумма (руб.)", 100000, 0),
      numericInput("sR", "Ставка (%)", 10, 0),
      numericInput("sN", "Срок (лет)", 3, 1)
    )
    else if (s == "Депозит с капитализацией") tagList(
      numericInput("dep_P", "Сумма (руб.)", 100000, 0),
      numericInput("dep_r", "Ставка (%)", 10, 0),
      numericInput("dep_n", "Срок (лет)", 5, 1),
      numericInput("dep_m", "Капитализаций в год", 12, 1, 365, 1)
    )
    else if (s == "Реальная доходность (Фишер)") tagList(
      numericInput("fish_nom", "Номинальная ставка (%)", 12),
      numericInput("fish_inf", "Инфляция (%)", 6)
    )
    else if (s == "Будущая стоимость аннуитета") tagList(
      numericInput("ann_PMT", "Платёж (руб.)", 10000, 0),
      numericInput("ann_r", "Ставка (%)", 8, 0),
      numericInput("ann_n", "Периодов", 10, 1, step = 1)
    )
  })
  
  # Заголовок раздела
  output$header <- renderText({
    paste(input$main_menu, "—", input$sub)
  })
  
  # Результат + код
  task_result <- eventReactive(input$run, {
    solve_task(input$sub, input)
  })
  
  output$result <- renderPrint({
    req(task_result())
    cat(task_result()$text)
  })
  
  # 📋 Светлый блок с R-кодом
  output$code_block <- renderUI({
    req(task_result())
    code <- task_result()$code
    if (is.null(code) || code == "") return(NULL)
    
    code_html <- code
    code_html <- gsub("&", "&amp;", code_html, fixed = TRUE)
    code_html <- gsub("<", "&lt;", code_html, fixed = TRUE)
    code_html <- gsub(">", "&gt;", code_html, fixed = TRUE)
    # Комментарии
    code_html <- gsub("(#[^\n]*)", "<span class='cmt'>\\1</span>", code_html)
    # Строки
    code_html <- gsub("('[^']*')", "<span class='str'>\\1</span>", code_html)
    # Числа
    code_html <- gsub("\\b([0-9]+\\.?[0-9]*)\\b", "<span class='num'>\\1</span>", code_html)
    # Ключевые слова R
    for (kw in c("if", "else", "for", "while", "function", "return", "print", "c", "numeric")) {
      code_html <- gsub(paste0("\\b", kw, "\\b"), paste0("<span class='kw'>", kw, "</span>"), code_html)
    }
    
    div(class = "code-block",
        div(class = "code-header",
            div(class = "dots",
                span(class = "dot r"), span(class = "dot y"), span(class = "dot g")
            ),
            span(class = "title", "📋  R-код решения"),
            span(style = "font-size: 0.8em; opacity: 0.8;", "RStudio · Shiny")
        ),
        tags$pre(class = "code-body", HTML(code_html))
    )
  })
  
  # График
  output$plot <- renderPlot({
    if (input$sub == "Гистограмма распределения") {
      data <- rnorm(input$n_hist, mean = input$mean_hist, sd = input$sd_hist)
      par(bg = "#FFFFFF", fg = "#2C3E50", col.axis = "#5C7A99",
          col.lab = "#2F4A6E", col.main = "#4A6FA5", family = "sans")
      hist(data, col = "#8FA9C9", border = "white",
           main = "Гистограмма распределения выборки",
           xlab = "Значения", ylab = "Частота")
      abline(v = mean(data), col = "#4A6FA5", lwd = 2, lty = 2)
      legend("topright", legend = c("Выборочное среднее"),
             col = "#4A6FA5", lty = 2, lwd = 2,
             bg = "white", box.col = "#D4E2EF")
    }
  })
}

# --------------------------------------------
# ЗАПУСК
# --------------------------------------------
shinyApp(ui, server)