# ============================================
# АНАЛИТИЧЕСКОЕ ПРИЛОЖЕНИЕ
# Математика | Статистика | Финансы
# ============================================
library(shiny)
library(ggplot2)
library(bslib)
library(bsicons)

# --------------------------------------------
# ТЕМА ОФОРМЛЕНИЯ
# --------------------------------------------
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary   = "#1F3A5F",   # тёмно-синий
  secondary = "#5A6B7C",   # серо-синий
  success   = "#2E7D32",   # зелёный
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  font_scale = 1.0
)

# --------------------------------------------
# UI
# --------------------------------------------
ui <- fluidPage(
  theme = app_theme,
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #F4F6F8;
        color: #2C3E50;
      }
      /* Шапка */
      .app-header {
        background: linear-gradient(90deg, #1F3A5F 0%, #2E5077 100%);
        color: #FFFFFF;
        padding: 22px 30px;
        border-radius: 6px;
        margin-bottom: 24px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
      .app-header h1 {
        margin: 0;
        font-size: 1.6em;
        font-weight: 600;
        letter-spacing: 0.3px;
      }
      .app-header p {
        margin: 6px 0 0 0;
        opacity: 0.85;
        font-size: 0.95em;
      }
      /* Боковая панель */
      .well {
        background: #FFFFFF !important;
        border: 1px solid #DDE3EA !important;
        border-radius: 6px !important;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04) !important;
        padding: 20px !important;
      }
      .well h4 {
        color: #1F3A5F;
        font-weight: 600;
        font-size: 1em;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 14px;
      }
      /* Кнопка */
      .btn-primary {
        background: #1F3A5F !important;
        border: none !important;
        border-radius: 4px !important;
        font-weight: 500;
        padding: 10px 20px !important;
        letter-spacing: 0.3px;
      }
      .btn-primary:hover {
        background: #2E5077 !important;
      }
      /* Поля ввода */
      .form-control, .form-select {
        border-radius: 4px !important;
        border: 1px solid #DDE3EA !important;
        background: #FFFFFF !important;
        color: #2C3E50 !important;
      }
      .form-control:focus, .form-select:focus {
        border-color: #1F3A5F !important;
        box-shadow: 0 0 0 3px rgba(31,58,95,0.1) !important;
      }
      label {
        color: #5A6B7C !important;
        font-weight: 500;
        font-size: 0.9em;
      }
      /* Блок результата */
      #result {
        background: #FFFFFF;
        padding: 20px;
        border-radius: 6px;
        border: 1px solid #DDE3EA;
        border-left: 4px solid #1F3A5F;
        font-family: 'Consolas', 'Monaco', monospace;
        font-size: 14px;
        color: #2C3E50;
        min-height: 100px;
      }
      /* Заголовок раздела */
      h3.section-title {
        color: #1F3A5F;
        font-weight: 600;
        border-bottom: 2px solid #DDE3EA;
        padding-bottom: 10px;
        margin-bottom: 20px;
      }
      /* Футер */
      .app-footer {
        text-align: center;
        margin-top: 40px;
        padding: 20px;
        color: #8A97A6;
        font-size: 0.85em;
        border-top: 1px solid #DDE3EA;
      }
      hr { border-color: #DDE3EA; }
    "))
  ),
  
  # ШАПКА
  div(class = "app-header",
      h1(HTML("&nbsp; Аналитическое приложение")),
      p("Математические расчёты · Статистический анализ · Финансовое моделирование")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Раздел"),
      selectInput("main_menu", NULL,
                  choices = c("Математические задачи",
                              "Анализ данных и статистика",
                              "Финансовые расчёты")),
      
      uiOutput("sub_menu"),
      hr(),
      
      # Поля ввода
      uiOutput("inputs"),
      
      br(),
      actionButton("run", "Выполнить расчёт",
                   class = "btn-primary",
                   width = "100%",
                   icon = icon("play"))
    ),
    
    mainPanel(
      h3(textOutput("header"), class = "section-title"),
      verbatimTextOutput("result"),
      plotOutput("plot")
    )
  ),
  
  # ФУТЕР
  div(class = "app-footer",
      "Учебный проект · Разработано на R + Shiny"
  )
)

# --------------------------------------------
# SERVER
# --------------------------------------------
server <- function(input, output) {
  
  # Подменю
  output$sub_menu <- renderUI({
    if (grepl("Матем", input$main_menu)) {
      selectInput("sub", "Задача:",
                  choices = c("Квадратное уравнение",
                              "Факториал",
                              "Простые числа"))
    } else if (grepl("Анализ", input$main_menu)) {
      selectInput("sub", "Метод:",
                  choices = c("Описательные статистики",
                              "Гистограмма распределения",
                              "Корреляционный анализ"))
    } else {
      selectInput("sub", "Расчёт:",
                  choices = c("Сложный процент",
                              "Аннуитетный платёж (ипотека)",
                              "Дисконтирование"))
    }
  })
  
  # Поля ввода
  output$inputs <- renderUI({
    req(input$sub)
    
    if (input$sub == "Квадратное уравнение") {
      tagList(
        numericInput("a", "Коэффициент a", value = 1),
        numericInput("b", "Коэффициент b", value = -3),
        numericInput("c", "Коэффициент c", value = 2)
      )
    }
    else if (input$sub == "Факториал") {
      numericInput("n_fact", "Число n (целое, ≤ 170)",
                   value = 5, min = 0, max = 170, step = 1)
    }
    else if (input$sub == "Простые числа") {
      numericInput("n_prime", "Верхняя граница поиска",
                   value = 30, min = 2, step = 1)
    }
    else if (input$sub == "Описательные статистики") {
      textAreaInput("data_desc",
                    "Набор данных (через запятую):",
                    value = "12, 15, 18, 22, 25, 30, 35, 40, 45, 50",
                    rows = 3)
    }
    else if (input$sub == "Гистограмма распределения") {
      tagList(
        numericInput("n_hist", "Объём выборки", value = 200, min = 10),
        numericInput("mean_hist", "Среднее (μ)", value = 50),
        numericInput("sd_hist", "Стандартное отклонение (σ)", value = 10, min = 0.1)
      )
    }
    else if (input$sub == "Корреляционный анализ") {
      tagList(
        textAreaInput("x_cor", "Набор X (через запятую):",
                      value = "1, 2, 3, 4, 5, 6, 7, 8", rows = 2),
        textAreaInput("y_cor", "Набор Y (через запятую):",
                      value = "2, 4, 5, 4, 5, 7, 8, 9", rows = 2)
      )
    }
    else if (input$sub == "Сложный процент") {
      tagList(
        numericInput("P", "Начальная сумма (руб.)", value = 100000, min = 0),
        numericInput("r_sp", "Ставка (% годовых)", value = 10, min = 0),
        numericInput("n_sp", "Срок (лет)", value = 5, min = 1)
      )
    }
    else if (input$sub == "Аннуитетный платёж (ипотека)") {
      tagList(
        numericInput("S_mort", "Сумма кредита (руб.)", value = 3000000, min = 0),
        numericInput("r_mort", "Годовая ставка (%)", value = 12, min = 0),
        numericInput("n_mort", "Срок (лет)", value = 20, min = 1)
      )
    }
    else if (input$sub == "Дисконтирование") {
      tagList(
        numericInput("FV", "Будущая стоимость (руб.)", value = 150000, min = 0),
        numericInput("r_disc", "Ставка дисконтирования (%)", value = 10, min = 0),
        numericInput("n_disc", "Период (лет)", value = 3, min = 1)
      )
    }
  })
  
  # Заголовок
  output$header <- renderText({
    paste(input$main_menu, "—", input$sub)
  })
  
  # Парсер чисел
  parse_numbers <- function(txt) {
    nums <- suppressWarnings(as.numeric(trimws(unlist(strsplit(txt, ",")))))
    nums[!is.na(nums)]
  }
  
  # Основная логика
  output$result <- renderPrint({
    req(input$run > 0)
    isolate({
      
      # МАТЕМАТИКА
      if (input$sub == "Квадратное уравнение") {
        a <- input$a; b <- input$b; c <- input$c
        cat("Уравнение:", a, "x^2 +", b, "x +", c, "= 0\n\n")
        if (a == 0) {
          cat("Ошибка: коэффициент a = 0, уравнение не является квадратным.\n")
        } else {
          D <- b^2 - 4*a*c
          cat("Дискриминант D =", D, "\n\n")
          if (D > 0) {
            x1 <- (-b + sqrt(D)) / (2*a)
            x2 <- (-b - sqrt(D)) / (2*a)
            cat("Два действительных корня:\n")
            cat("  x1 =", round(x1, 4), "\n")
            cat("  x2 =", round(x2, 4), "\n")
          } else if (D == 0) {
            x <- -b / (2*a)
            cat("Один корень (D = 0): x =", round(x, 4), "\n")
          } else {
            cat("Действительных корней нет (D < 0).\n")
          }
        }
      }
      
      else if (input$sub == "Факториал") {
        n <- input$n_fact
        if (n < 0 || n != round(n)) {
          cat("Ошибка: требуется целое неотрицательное число.\n")
        } else {
          cat("Факториал", n, "! =", factorial(n), "\n")
        }
      }
      
      else if (input$sub == "Простые числа") {
        n <- input$n_prime
        if (n < 2) {
          cat("Ошибка: граница должна быть ≥ 2.\n")
        } else {
          primes <- c()
          for (i in 2:n) {
            is_prime <- TRUE
            if (i > 2) {
              for (j in 2:floor(sqrt(i))) {
                if (i %% j == 0) { is_prime <- FALSE; break }
              }
            }
            if (is_prime) primes <- c(primes, i)
          }
          cat("Простые числа в диапазоне [2;", n, "]:\n\n")
          cat(primes, "\n\n")
          cat("Всего найдено:", length(primes), "\n")
        }
      }
      
      # СТАТИСТИКА
      else if (input$sub == "Описательные статистики") {
        x <- parse_numbers(input$data_desc)
        if (length(x) < 2) {
          cat("Ошибка: требуется минимум 2 числа.\n")
        } else {
          cat("Объём выборки: n =", length(x), "\n")
          cat("Данные:", x, "\n\n")
          cat("Среднее значение:        ", round(mean(x), 4), "\n")
          cat("Медиана:                 ", round(median(x), 4), "\n")
          cat("Стандартное отклонение:  ", round(sd(x), 4), "\n")
          cat("Дисперсия:               ", round(var(x), 4), "\n")
          cat("Минимум:                 ", min(x), "\n")
          cat("Максимум:                ", max(x), "\n")
          cat("Размах:                  ", max(x) - min(x), "\n")
        }
      }
      
      else if (input$sub == "Гистограмма распределения") {
        cat("Параметры распределения:\n\n")
        cat("  Объём выборки: n =", input$n_hist, "\n")
        cat("  Среднее (μ):   ", input$mean_hist, "\n")
        cat("  Ст. откл. (σ): ", input$sd_hist, "\n\n")
        cat("График построен на панели справа.\n")
      }
      
      else if (input$sub == "Корреляционный анализ") {
        x <- parse_numbers(input$x_cor)
        y <- parse_numbers(input$y_cor)
        if (length(x) != length(y) || length(x) < 2) {
          cat("Ошибка: наборы X и Y должны быть одинаковой длины (≥ 2).\n")
          cat("Длина X:", length(x), "| Длина Y:", length(y), "\n")
        } else {
          r <- cor(x, y)
          cat("Набор X:", x, "\n")
          cat("Набор Y:", y, "\n\n")
          cat("Коэффициент корреляции Пирсона: r =", round(r, 4), "\n\n")
          if (abs(r) > 0.7) cat("Интерпретация: сильная линейная связь.\n")
          else if (abs(r) > 0.3) cat("Интерпретация: умеренная линейная связь.\n")
          else cat("Интерпретация: слабая линейная связь.\n")
        }
      }
      
      # ФИНАНСЫ
      else if (input$sub == "Сложный процент") {
        P <- input$P; r <- input$r_sp / 100; n <- input$n_sp
        S <- P * (1 + r)^n
        cat("Исходные данные:\n")
        cat("  Начальная сумма:  ", format(P, big.mark = " "), "руб.\n")
        cat("  Ставка:            ", input$r_sp, "% годовых\n")
        cat("  Срок:              ", n, "лет\n\n")
        cat("Результат:\n")
        cat("  Наращенная сумма:  ", format(round(S, 2), big.mark = " "), "руб.\n")
        cat("  Доход:             ", format(round(S - P, 2), big.mark = " "), "руб.\n")
      }
      
      else if (input$sub == "Аннуитетный платёж (ипотека)") {
        S <- input$S_mort
        r <- input$r_mort / 100 / 12
        n <- input$n_mort * 12
        payment <- if (r == 0) S / n else S * r / (1 - (1 + r)^(-n))
        total <- payment * n
        cat("Параметры кредита:\n")
        cat("  Сумма:             ", format(S, big.mark = " "), "руб.\n")
        cat("  Годовая ставка:    ", input$r_mort, "%\n")
        cat("  Срок:              ", input$n_mort, "лет (", n, "месяцев)\n\n")
        cat("Результат:\n")
        cat("  Ежемесячный платёж:", format(round(payment, 2), big.mark = " "), "руб.\n")
        cat("  Общая сумма выплат:", format(round(total, 2), big.mark = " "), "руб.\n")
        cat("  Переплата:         ", format(round(total - S, 2), big.mark = " "), "руб.\n")
      }
      
      else if (input$sub == "Дисконтирование") {
        FV <- input$FV
        r <- input$r_disc / 100
        n <- input$n_disc
        PV <- FV / (1 + r)^n
        cat("Исходные данные:\n")
        cat("  Будущая стоимость: ", format(FV, big.mark = " "), "руб.\n")
        cat("  Ставка:             ", input$r_disc, "%\n")
        cat("  Период:             ", n, "лет\n\n")
        cat("Результат:\n")
        cat("  Текущая стоимость: ", format(round(PV, 2), big.mark = " "), "руб.\n")
        cat("  Дисконт:           ", format(round(FV - PV, 2), big.mark = " "), "руб.\n")
      }
    })
  })
  
  # График
  output$plot <- renderPlot({
    req(input$run > 0)
    if (input$sub == "Гистограмма распределения") {
      data <- rnorm(input$n_hist, mean = input$mean_hist, sd = input$sd_hist)
      par(bg = "#FFFFFF", fg = "#2C3E50", col.axis = "#5A6B7C",
          col.lab = "#2C3E50", col.main = "#1F3A5F", family = "sans")
      hist(data,
           col = "#5A8FCA",
           border = "white",
           main = "Гистограмма распределения выборки",
           xlab = "Значения",
           ylab = "Частота")
      abline(v = mean(data), col = "#C0392B", lwd = 2, lty = 2)
      legend("topright",
             legend = c("Выборочное среднее"),
             col = "#C0392B", lty = 2, lwd = 2,
             bg = "white", box.col = "#DDE3EA")
    }
  })
}

# --------------------------------------------
# ЗАПУСК
# --------------------------------------------
shinyApp(ui, server)