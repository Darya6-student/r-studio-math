library(shiny)
library(ggplot2)
library(bslib)
library(bsicons)

# Определение пользовательского интерфейса
ui <- page_sidebar(
  title = "📚 Обучение математике в R Studio",
  
  sidebar = sidebar(
    navset_card_tab(
      id = "tabs",
      
      # Вкладка 1: Производные
      nav_panel("📐 Производные", icon = bs_icon("calculator"),
                selectInput("deriv_func", "Выберите функцию:",
                            choices = c(
                              "f(x) = x² + 3x - 5" = "poly",
                              "f(x) = sin(x)" = "sin",
                              "f(x) = e^x" = "exp",
                              "f(x) = ln(x)" = "log"
                            )
                ),
                numericInput("deriv_x", "Точка вычисления (x):", value = 2, min = -10, max = 10, step = 0.1),
                actionButton("calc_deriv", "Вычислить производную", class = "btn-primary")
      ),
      
      # Вкладка 2: Статистика
      nav_panel("📊 Статистика", icon = bs_icon("graph-up"),
                textAreaInput("stat_data", "Введите данные через запятую:", 
                              value = "12, 15, 18, 20, 22, 25, 28",
                              height = "100px"),
                actionButton("calc_stats", "Рассчитать статистику", class = "btn-success"),
                selectInput("plot_type", "Тип графика:",
                            choices = c("Гистограмма" = "hist", "Boxplot" = "boxplot")
                )
      ),
      
      # Вкладка 3: Финансовая математика
      nav_panel("💰 Финансы", icon = bs_icon("currency-dollar"),
                radioButtons("fin_calc", "Тип расчёта:",
                             choices = c(
                               "Сложный процент" = "compound",
                               "Аннуитетный платёж" = "annuity",
                               "NPV проекта" = "npv"
                             )
                ),
                
                # Поля для сложного процента
                conditionalPanel(
                  condition = "input.fin_calc == 'compound'",
                  numericInput("initial_amount", "Начальная сумма (руб.):", value = 10000),
                  numericInput("interest_rate", "Годовая ставка (%):", value = 8, min = 0, max = 100),
                  numericInput("years_compound", "Срок (лет):", value = 5, min = 1, max = 50)
                ),
                
                # Поля для аннуитета
                conditionalPanel(
                  condition = "input.fin_calc == 'annuity'",
                  numericInput("loan_amount", "Сумма кредита (руб.):", value = 500000),
                  numericInput("annual_rate", "Годовая ставка (%):", value = 12, min = 0, max = 100),
                  numericInput("loan_years", "Срок (лет):", value = 3, min = 1, max = 30)
                ),
                
                # Поля для NPV
                conditionalPanel(
                  condition = "input.fin_calc == 'npv'",
                  textAreaInput("cash_flows", "Денежные потоки через запятую\n(первый - отрицательный):", 
                                value = "-100000, 30000, 35000, 40000, 45000",
                                height = "100px"),
                  numericInput("discount_rate", "Ставка дисконтирования (%):", value = 10, min = 0, max = 100)
                ),
                
                actionButton("calc_finance", "Рассчитать", class = "btn-warning")
      )
    )
  ),
  
  # Основная область вывода
  card(
    card_header(h3("📋 Результаты")),
    uiOutput("result_output")
  )
)

# Серверная логика
server <- function(input, output, session) {
  
  # Обработка производных
  observeEvent(input$calc_deriv, {
    x_val <- input$deriv_x
    
    result_text <- switch(input$deriv_func,
                          "poly" = {
                            derivative <- 2 * x_val + 3
                            paste0("<strong>Функция:</strong> f(x) = x² + 3x - 5<br>",
                                   "<strong>Производная:</strong> f'(x) = 2x + 3<br>",
                                   "<strong>Значение при x =", x_val, ":</strong> f'(", x_val, ") = ", derivative)
                          },
                          "sin" = {
                            derivative <- cos(x_val)
                            paste0("<strong>Функция:</strong> f(x) = sin(x)<br>",
                                   "<strong>Производная:</strong> f'(x) = cos(x)<br>",
                                   "<strong>Значение при x =", round(x_val, 2), ":</strong> f'(", round(x_val, 2), ") = ", round(derivative, 4))
                          },
                          "exp" = {
                            derivative <- exp(x_val)
                            paste0("<strong>Функция:</strong> f(x) = e^x<br>",
                                   "<strong>Производная:</strong> f'(x) = e^x<br>",
                                   "<strong>Значение при x =", round(x_val, 2), ":</strong> f'(", round(x_val, 2), ") = ", round(derivative, 4))
                          },
                          "log" = {
                            if (x_val <= 0) {
                              return("<span style='color:red;'>❌ Ошибка: x должен быть больше 0 для ln(x)</span>")
                            }
