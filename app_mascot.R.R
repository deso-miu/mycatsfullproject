# ============================================
# 💙 ПРИЛОЖЕНИЕ С МАСКОТОМ УНИВЕРСИТЕТА 💙
# Снежный барс · сердечки · снежинки · яркое горное утро
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
# UI
# --------------------------------------------
ui <- fluidPage(
  theme = mascot_theme,
  
  tags$head(
    tags$style(HTML("
      /* === 🌄 ЯРКИЙ ГРАДИЕНТ «ГОРНОЕ УТРО» === */
      html {
        min-height: 100%;
      }
      body {
        background: linear-gradient(135deg,
                    #A8C8E4 0%,
                    #D4E6F5 40%,
                    #C5D8EF 70%,
                    #9FB8DC 100%);
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
      
      /* === ❄ СЛОЙ 1: СНЕЖИНКИ === */
      body::before {
        content: '❄        ❅        ❆        ✻        ❄        ❅        ❆        ✻';
        position: fixed;
        top: -20%;
        left: -5%;
        width: 110%;
        height: 160%;
        font-size: 24px;
        color: rgba(74, 111, 165, 0.35);
        letter-spacing: 180px;
        line-height: 6.5em;
        word-spacing: 40px;
        pointer-events: none;
        z-index: 0;
        animation: floatDown1 45s linear infinite;
        text-align: justify;
      }
      @keyframes floatDown1 {
        0%   { transform: translateY(0) translateX(0) rotate(0deg); }
        50%  { transform: translateY(110px) translateX(30px) rotate(180deg); }
        100% { transform: translateY(220px) translateX(0) rotate(360deg); }
      }
      
      /* === 💙 СЛОЙ 2: СЕРДЕЧКИ === */
      body::after {
        content: '💙      💙      💙      💙      💙      💙      💙      💙      💙';
        position: fixed;
        top: -10%;
        left: 0;
        width: 100%;
        height: 150%;
        font-size: 20px;
        opacity: 0.55;
        letter-spacing: 240px;
        line-height: 9em;
        word-spacing: 100px;
        pointer-events: none;
        z-index: 0;
        animation: floatDown2 55s linear infinite;
        text-align: justify;
      }
      @keyframes floatDown2 {
        0%   { transform: translateY(0) translateX(0) rotate(-10deg); }
        50%  { transform: translateY(140px) translateX(-25px) rotate(10deg); }
        100% { transform: translateY(280px) translateX(0) rotate(-10deg); }
      }
      
      /* === ❄💙 СЛОЙ 3: СМЕШАННЫЙ === */
      html::before {
        content: '❄  💙  ❆  💙  ❅  💙  ✻  💙  ❄  💙  ❆  💙';
        position: fixed;
        top: -15%;
        left: 2%;
        width: 100%;
        height: 155%;
        font-size: 18px;
        opacity: 0.4;
        letter-spacing: 300px;
        line-height: 12em;
        word-spacing: 150px;
        pointer-events: none;
        z-index: 0;
        animation: floatDown3 70s linear infinite;
        text-align: justify;
      }
      @keyframes floatDown3 {
        0%   { transform: translateY(0) translateX(0); }
        33%  { transform: translateY(90px) translateX(20px); }
        66%  { transform: translateY(180px) translateX(-20px); }
        100% { transform: translateY(270px) translateX(0); }
      }
      
      /* Контент поверх фона */
      .container-fluid, .well, .main-panel, .app-header {
        position: relative;
        z-index: 1;
      }
      
      /* === 💙 ШАПКА === */
      .mascot-header {
        background: rgba(255, 255, 255, 0.92);
        backdrop-filter: blur(6px);
        border-radius: 16px;
        padding: 20px 24px;
        margin-bottom: 20px;
        box-shadow: 0 4px 20px rgba(74,111,165,0.18);
        border: 1px solid rgba(212, 226, 239, 0.8);
        display: flex;
        align-items: center;
        gap: 20px;
        position: relative;
        overflow: hidden;
      }
      .mascot-header::before {
        content: '💙';
        position: absolute;
        top: 8px;
        right: 14px;
        font-size: 22px;
        animation: pulse 2.5s ease-in-out infinite;
      }
      .mascot-header::after {
        content: '❄';
        position: absolute;
        bottom: 8px;
        left: 14px;
        font-size: 22px;
        color: #4A6FA5;
        animation: spin 15s linear infinite;
      }
      @keyframes pulse {
        0%, 100% { transform: scale(1); }
        50%      { transform: scale(1.25); }
      }
      @keyframes spin {
        from { transform: rotate(0deg); }
        to   { transform: rotate(360deg); }
      }
      .mascot-header-text h1 {
        color: #2F4A6E;
        margin: 0;
        font-size: 1.6em;
        font-weight: 700;
      }
      .mascot-header-text h1::before {
        content: '❄  ';
        color: #6495ED;
        margin-right: 4px;
      }
      .mascot-header-text h1::after {
        content: '  💙';
      }
      .mascot-header-text p {
        color: #5C7A99;
        margin: 6px 0 0 0;
        font-size: 0.95em;
      }
      .mascot-video {
        border-radius: 12px;
        box-shadow: 0 4px 12px rgba(74,111,165,0.2);
        flex-shrink: 0;
      }
      
      /* === 📋 БОКОВАЯ ПАНЕЛЬ === */
      .well {
        background: rgba(255, 255, 255, 0.95) !important;
        border: 1px solid #D4E2EF !important;
        border-radius: 14px !important;
        box-shadow: 0 4px 14px rgba(74,111,165,0.10) !important;
        padding: 20px !important;
        position: relative;
        backdrop-filter: blur(4px);
      }
      .well::before {
        content: '💙';
        position: absolute;
        top: 10px;
        right: 14px;
        font-size: 16px;
        opacity: 0.65;
      }
      .well h4 {
        color: #4A6FA5;
        font-weight: 700;
        font-size: 0.95em;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 14px;
      }
      .well h4::before {
        content: '❄ ';
        font-size: 0.95em;
        color: #6495ED;
      }
      
      /* === 🔘 КНОПКА === */
      .btn-primary {
        background: linear-gradient(135deg, #4A6FA5, #6B8FC4) !important;
        border: none !important;
        border-radius: 10px !important;
        font-weight: 600;
        padding: 10px 22px !important;
        letter-spacing: 0.3px;
        transition: all 0.25s ease;
        box-shadow: 0 3px 10px rgba(74,111,165,0.3);
      }
      .btn-primary::before {
        content: '❄ ';
      }
      .btn-primary:hover {
        transform: translateY(-1px);
        box-shadow: 0 6px 18px rgba(74,111,165,0.5),
                    0 0 22px rgba(100, 149, 237, 0.55);
      }
      
      /* === ✏️ ПОЛЯ ВВОДА === */
      .form-control, .form-select {
        border-radius: 10px !important;
        border: 1.5px solid #D4E2EF !important;
        background: #FFFFFF !important;
        color: #2C3E50 !important;
      }
      .form-control:focus, .form-select:focus {
        border-color: #4A6FA5 !important;
        box-shadow: 0 0 0 3px rgba(74,111,165,0.15) !important;
      }
      label {
        color: #5C7A99 !important;
        font-weight: 600;
        font-size: 0.9em;
      }
      label::before {
        content: '❆ ';
        color: #6495ED;
        font-size: 0.85em;
      }
      
      /* === 📊 БЛОК РЕЗУЛЬТАТА === */
      #result {
        background: rgba(255, 255, 255, 0.97);
        padding: 18px 20px;
        border-radius: 12px;
        border: 1px solid #D4E2EF;
        border-left: 4px solid #4A6FA5;
        font-family: 'Consolas', 'Monaco', monospace;
        font-size: 14px;
        color: #2C3E50;
        min-height: 100px;
        box-shadow: 0 4px 14px rgba(74,111,165,0.08);
      }
      
      /* === 🏷 ЗАГОЛОВОК РАЗДЕЛА === */
      h3.section-title {
        color: #2F4A6E;
        font-weight: 700;
        border-bottom: 2px solid rgba(74,111,165,0.25);
        padding-bottom: 10px;
        margin-bottom: 20px;
        position: relative;
      }
      h3.section-title::before {
        content: '❄  ';
        color: #6495ED;
      }
      
      /* === 🐾 ФУТЕР === */
      .mascot-footer {
        text-align: center;
        margin-top: 40px;
        padding: 20px;
        color: #4A6FA5;
        font-size: 0.85em;
        border-top: 1px solid rgba(74,111,165,0.2);
      }
      .mascot-footer .paw {
        font-size: 1.3em;
        margin: 0 6px;
        color: #4A6FA5;
      }
      .mascot-footer .snow {
        color: #4A6FA5;
        margin: 0 8px;
        font-size: 1.15em;
      }
      .mascot-footer .heart {
        margin: 0 8px;
        font-size: 1.1em;
      }
      hr { border-color: rgba(74,111,165,0.2); }
    "))
  ),
  
  # 💙 ШАПКА С МАСКОТОМ
  div(class = "mascot-header",
      tags$video(
        src = "elk_cat.webm",
        width = "130",
        autoplay = NA,
        loop = NA,
        muted = NA,
        class = "mascot-video"
      ),
      div(class = "mascot-header-text",
          h1("Аналитическое приложение"),
          p("Математические расчёты · Статистический анализ · Финансовые модели")
      )
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
      # 📈 График появляется ТОЛЬКО при гистограмме
      conditionalPanel(
        condition = "input.sub == 'Гистограмма распределения'",
        plotOutput("plot")
      )
    )
  ),
  
  # 🐾 ФУТЕР
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
  
  # График (показывается только при гистограмме, благодаря conditionalPanel)
  output$plot <- renderPlot({
    if (input$sub == "Гистограмма распределения") {
      data <- rnorm(input$n_hist, mean = input$mean_hist, sd = input$sd_hist)
      par(bg = "#FFFFFF", fg = "#2C3E50", col.axis = "#5C7A99",
          col.lab = "#2F4A6E", col.main = "#4A6FA5", family = "sans")
      hist(data,
           col = "#8FA9C9",
           border = "white",
           main = "Гистограмма распределения выборки",
           xlab = "Значения",
           ylab = "Частота")
      abline(v = mean(data), col = "#4A6FA5", lwd = 2, lty = 2)
      legend("topright",
             legend = c("Выборочное среднее"),
             col = "#4A6FA5", lty = 2, lwd = 2,
             bg = "white", box.col = "#D4E2EF")
    }
  })
}

# --------------------------------------------
# ЗАПУСК
# --------------------------------------------
shinyApp(ui, server)