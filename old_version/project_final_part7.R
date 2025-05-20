# Завантажуємо наш датасет
my_data <- read.csv("/Users/artem/Documents/R/Odata2024File.csv", encoding = "UTF-8", header = TRUE, sep = ";")

# Перевірка структури даних, чи датасет нормально завантажився
head(my_data, 10)
View(my_data[1:10, ])

#----- ОПИС ДАТАСЕТУ -----
#Кількість рядків в нашому датасеті 
cat("В нашому датасеті", nrow(my_data), "рядків\n")

# кількість стовпців в нашому датасеті 
cat("В нашому датасеті", ncol(my_data), "стовпців\n")

# базові статистичні показники для кожної змінної
summary(my_data)

# структура даних - типи змінних і приклади значень
str(my_data)

# альтернативний перегляд структури з dplyr
library(dplyr)
glimpse(my_data)

#----- ОБРОБКА ДАНИХ -----
#під час перегляду структури даних було помічено що в датасеті замість NA 
# використовується null, що заважає коректно працювати з аналітичними функціями, 
# тому що null буде сприйматися як звичайні текстові дані, а не як пропущені 
# значення, що призведе до помилок у розрахунках та аналізі 

# перетворення null на NA
my_data[my_data == "null"] <- NA

# повторний аналіз даних після зміни null на NA
# базові статистичні показники для кожної змінної
summary(my_data)

# структура даних - типи змінних і приклади значень
str(my_data)

# альтернативний перегляд структури з dplyr
glimpse(my_data)

# заміна пройшла успішно, тепер можемо разувати кількість пропущених значень в датасеті

# перевірка пропущених значень
colSums(is.na(my_data))

# також під час перевірки структури датасету було помічено, що стовпець TestDate 
# має Class та Mode character, що не зовсім правильно, тому переведемо його в Date

str(my_data$TestDate)

my_data$TestDate <- as.Date(my_data$TestDate, format = "%d.%m.%Y")

str(my_data$TestDate)

# також під час перевірки структури датасету було помічено, що стовпці 
# UkrBlockBall100, UkrBlockBall, HistBlockBall100, HistBlockBall, MathBlockBall100, 
# MathBlockBall, PhysBlockBall100, PhysBlockBall, ChemBlockBall100, ChemBlockBall, 
# BioBlockBall100, BioBlockBall, GeoBlockBall100, GeoBlockBall, EngBlockBall100, 
# EngBlockBall, FraBlockBall100, FraBlockBall, DeuBlockBall100, DeuBlockBall, 
# SpaBlockBall100, SpaBlockBall, UkrLitBlockBall100, UkrLitBlockBall мають Class 
# та Mode character, що не зовсім правильно, тому переведемо його в integer

# Створюємо список стовпців, які потрібно конвертувати
cols_to_convert <- c("UkrBlockBall100", "UkrBlockBall", "HistBlockBall100", "HistBlockBall", 
                     "MathBlockBall100", "MathBlockBall", "PhysBlockBall100", "PhysBlockBall", 
                     "ChemBlockBall100", "ChemBlockBall", "BioBlockBall100", "BioBlockBall", 
                     "GeoBlockBall100", "GeoBlockBall", "EngBlockBall100", "EngBlockBall", 
                     "FraBlockBall100", "FraBlockBall", "DeuBlockBall100", "DeuBlockBall", 
                     "SpaBlockBall100", "SpaBlockBall", "UkrLitBlockBall100", "UkrLitBlockBall")

for (col in cols_to_convert) {
  print(str(my_data[[col]]))
}

# Застосовуємо конвертацію
my_data[, cols_to_convert] <- lapply(my_data[, cols_to_convert], function(x) {
  # оскільки деякі цифри в нас з комою, то замінюємо її на крапку
  x_fixed <- gsub(",", ".", x)
  # тепер можемо пеертворити character спочатку в numeric, а потім в integer
  as.integer(as.numeric(x_fixed))
})

for (col in cols_to_convert) {
  print(str(my_data[[col]]))
}

# Перевіряємо зміни
str(my_data)
summary(my_data)

#----- ВІЗУАЛІЗАЦІЯ РОЗПОДІЛУ ЗМІННИХ -----

library(ggplot2)

# Ми побудуємо два варіанти графіків — з урахуванням нулів і без них, щоб краще 
# зрозуміти наші дані з різних боків. Коли ми включаємо нульові значення, бачимо 
# повну картину, зокрема тих, хто не набрав жодного бала (не подолав поріг). Це 
# допомагає оцінити, який відсоток таких учасників і наскільки вони відрізняються 
# від решти.
# Якщо ж ми прибираємо всі нулі, то можемо детальніше побачити, як розподіляються 
# результати лише серед тих, хто подолав поріг. У такому випадку не буде сильного 
# «зсуву» чи окремого «шипа» на графіку, і ми чіткіше побачимо, де саме 
# зосереджені бали (скажімо, переважно в діапазоні від 140 до 150).

# Гістограма без нулів (тільки ті, хто подолав поріг)
# Створюємо підмножину даних без нульових значень
data_filtered <- my_data[my_data$UkrBlockBall100 > 0, ]

not_passed <- sum(my_data$UkrBlockBall100 == 0, na.rm = TRUE)
total <- sum(!is.na(my_data$UkrBlockBall100))
percent_not_passed <- round(not_passed / total * 100, 1)

ggplot(data_filtered, aes(x = UkrBlockBall100)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "Розподіл балів UkrBlockBall100 (без нулів)",
       subtitle = paste(not_passed, "учасників (", percent_not_passed, "%) не подолали поріг"),
       x = "Бали", y = "Частота") +
  theme_minimal()

# На цій гістограмі ми бачимо розподіл балів тих учасників, які подолали поріг, 
# тобто отримали більше 0. Найвищий стовпчик розташований у районі 140–150 
# балів, і можна сказати, що найбільша частка учасників отримала оцінки саме в 
# цьому діапазоні. Загалом результати досить щільно згруповані приблизно від 110 
# до 180 балів, що вказує на відносно «центрований» розподіл без яскраво 
# вираженого зміщення
# При цьому зверху на графіку зазначено, що лише 0,4% (тобто 1218 учасників) 
# від загальної кількості отримали 0. Таким чином, більшість учасників 
# успішно подолали мінімальний пороговий бал, а розподіл результатів переважно 
# зосереджений у середньому та високому діапазоні.

# Коробкова діаграма без нулів
ggplot(data_filtered, aes(y = UkrBlockBall100)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Коробкова діаграма UkrBlockBall100 (без нулів)",
       subtitle = paste(not_passed, "учасників (", percent_not_passed, "%) не подолали поріг"),
       y = "Бали") +
  theme_minimal()

# На цій коробковій діаграмі ми бачимо розподіл балів серед тих, хто подолав поріг, 
# тобто хто має бали,  вищі за 0 . Коробка розташована приблизно в межах від  
# 130 до 150 , а медіана знаходиться близько 140–145  балів.  
# Важливо, що на цій діаграмі вже немає нульових балів, оскільки ми відсіяли 
# учасників, які не подолали поріг (за легендою, їх було 0,4% від загальної кількості).  
# Таким чином, цей графік дозволяє побачити, як розподілилися бали в учасників, 
# які набрали принаймні 1 бал, і наочно показує, де знаходяться типовий діапазон 
# оцінок (коробка) та невелика кількість екстремальних значень.

# Графік розсіювання

# Для побудови графіка розсіювання візьмемо змінні MathBlockBall100 та UkrBlockBall100

ggplot(my_data, aes(x = UkrBlockBall100, y = MathBlockBall100)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Графік розсіювання: UkrBlockBall100 vs MathBlockBall100 (включаючи нулі)",
       x = "UkrBlockBall100",
       y = "MathBlockBall100") +
  theme_minimal()

# На осі X відкладені бали з української, а на осі Y — бали з математики.
# Бачимо, що частина учасників отримала 0 (не подолали поріг) з обох предметів 
# (точки, які примикають до осей).
# Основна «хмара» точок зосереджена в діапазоні 100–200 як по UkrBlockBall100, 
# так і по MathBlockBall100. Тут можна розгледіти, що чим вищі бали з одного 
# предмета, тим більша схильність до вищих балів з іншого (є певна позитивна 
# залежність).
# Є також невелика кількість точок, що сильно виділяються (наприклад, хтось 
# має 0 з одного предмета, але понад 150 чи 200 з іншого). Такі точки свідчать, 
# що учасник міг добре скласти один предмет, але повністю «провалити» інший.
# Загалом ми бачимо скупчення балів у верхньому правому куті (близько 150–200), 
# що вказує на чималу кількість учасників з високими балами з обох предметів.

#----- ПОБУДОВА КОРЕЛЯЦІЙНОЇ МАТРИЦІ -----
library(corrplot)

# Перетворення змінних стовпця 'TerTypeName' у числові
my_data <- my_data %>%
  mutate(city = ifelse(TerTypeName == "місто", 1, 0),
         village_or_town = ifelse(TerTypeName == "селище, село", 1, 0),
         other_country = ifelse(TerTypeName == "інша країна", 1, 0))

# Перетворення змінних стовпця 'SexTypeName' у числові
my_data <- my_data %>%
  mutate(male = ifelse(SexTypeName == "чоловіча", 1, 0),
         female = ifelse(SexTypeName == "жіноча", 1, 0))

# Фільтрування та обрання лише числових стовпців.
numeric_data <- my_data %>%
  select(where(is.numeric))

# Прибирання стовпців із великою кількістю пропущених значень для збільшення
#точності кореляції.
threshold <- 0.7  # залишаємо лише ті, де понад 70% значень не NA
valid_cols <- names(numeric_data)[colMeans(!is.na(numeric_data)) >= threshold]
cor_data <- numeric_data[, valid_cols]

# Видалення рядків із NA
cor_data <- na.omit(cor_data)

# Розрахунок кореляційної матриці
cor_matrix <- cor(cor_data, use = "complete.obs")

# Побудова кореляційної матриці
corrplot(cor_matrix, 
         method = "color",        
         type = "full",          
         addCoef.col = "black",   
         tl.col = "grey",        
         number.cex = 0.7,        
         col = colorRampPalette(c("purple", "white", "orange"))(200)  
)

#Побудова кореляційної матриці дозволила виявити певний звʼязок між ознаками.
#Прослідкована позитивна кореляція між місцевістю, в якій відбувалося складання
#НМТ учасником, та балами за складені предмети. Так можна припустити, що ті 
#учасники, що проходили НМТ в місті, отримували більшу кількість балів. 
#Також можна прослідкувати позитивну кореляцію між статтю учасника НМТ та результатами
#зі складених предметів. Можливим є припущення, що учасники жіночого роду в середньому
#мають трохи вищі бали за українську мові, порівняно з учасниками чоловічого роду.
#Також можна прослідкувати позитивну кореляцію між датою народження та результатами
#проходження НМТ, але цей звʼязок є досить слабким. Серед прикладів негативної кореляції
#можна виділити звʼязок між селищем як територією складання НМТ та результатами з математики,
#проте цей звʼязок є настільки слабким, що навряд чи впливає на результат.

#----- ЧИСТКА ТА ПІДГОТОВКА ДАНИХ -----

# ВИЯВЛЯЄМО ПРОПУЩЕНІ ЗНАЧЕННЯ

# Спочатку подивимось, скільки пропущених значень у нашому датасеті
sum(is.na(my_data))

# Аналіз відсотку пропущених значень
na_percentage <- round(colMeans(is.na(my_data)) * 100, 2)
na_df <- data.frame(Column = names(na_percentage), NA_Percentage = na_percentage)
na_df <- na_df[order(-na_df$NA_Percentage), ]
print(na_df[na_df$NA_Percentage > 0, ])

# Ми вирішили не підставляти жодних вигаданих оцінок замість пропусків, бо в нашому 
# випадку NA означає реальну відсутність учня на іспиті або вибір предмету, і 
# будь-яка спроба «заповнити» ці порожні клітинки середнім чи медіаною лише 
# спотворить справжню картину результатів. Якщо ми підсунемо штучні бали, то 
# втратимо інформацію про тих, хто не з’явився, і ризикуємо отримати хибні 
# висновки про успішність учнів. Краще залишити пропуски як вони є, щоб у 
# подальшому аналізі чітко розуміти, що ці дані відсутні з об’єктивних причин, 
# а не через технічну помилку. Це дозволяє зберегти достовірність дослідження 
# та уникнути непотрібних перекручувань у статистичних показниках.

#----- АНАЛІЗ ТА ОБРОБКА ВИКИДІВ -----

# Підготуємо лише числові дані
numeric_data <- my_data[, sapply(my_data, is.numeric)]

# Список максимальних балів для кожного BlockBall-стовпця
max_scores <- c(
  UkrBlockBall      = 45,
  UkrLitBlockBall   = 45,
  MathBlockBall     = 32,
  HistBlockBall     = 54,
  EngBlockBall      = 32,
  FraBlockBall      = 32,
  DeuBlockBall      = 32,
  SpaBlockBall      = 32,
  BioBlockBall      = 46,
  ChemBlockBall     = 40,
  PhysBlockBall     = 32,
  GeoBlockBall      = 46
)

# Шукаємо всі стовпці з “BlockBall” у назві
score_cols <- grep("BlockBall", names(numeric_data), value = TRUE)

# Функція виявлення викидів за доменними межами
detect_outliers <- function(x, col_name) {
  n <- length(x)
  outliers <- rep(FALSE, n)
  valid    <- !is.na(x)
  
  # BlockBall100: 0 – легітимно, ненульові мають бути в [100,200]
  if (grepl("BlockBall100$", col_name)) {
    outliers[ valid & x != 0 & (x < 100 | x > 200) ] <- TRUE
    return(list(outliers = outliers, lower = 100, upper = 200))
  }
  
  # Інші *BlockBall: доменні межі [0, max_scores[col_name]]
  if (col_name %in% names(max_scores)) {
    upper <- max_scores[col_name]
    # всі значення <0 або >upper вважаємо викидами
    outliers[ valid & (x < 0 | x > upper) ] <- TRUE
    return(list(outliers = outliers, lower = 0, upper = upper))
  }
  
  # всі інші – без аналізу
  list(outliers = outliers, lower = NA, upper = NA)
}

# Підготуємо таблицю для статистики нульових
zero_scores <- data.frame(
  Subject      = character(),
  Zero_Count   = integer(),
  Zero_Percent = numeric(),
  stringsAsFactors = FALSE
)

# Основний цикл по кожному предмету
for (col in score_cols) {
  vec <- numeric_data[[col]]
  
  # Підрахунок нулів
  zcount <- sum(vec == 0, na.rm = TRUE)
  total  <- sum(!is.na(vec))
  zpct   <- if (total > 0) round(zcount/total*100, 2) else NA
  zero_scores <- rbind(zero_scores, data.frame(
    Subject      = col,
    Zero_Count   = zcount,
    Zero_Percent = zpct,
    stringsAsFactors = FALSE
  ))
  
  # Виявлення викидів
  info      <- detect_outliers(vec, col)
  valid_cnt <- sum(!is.na(vec))
  out_cnt   <- sum(info$outliers, na.rm = TRUE)
  
  # Вивід результату
  if (grepl("BlockBall100$", col)) {
    cat(sprintf("%s: %d викидів (ненульові поза [100,200])\n",
                col, out_cnt))
  } else if (col %in% names(max_scores)) {
    cat(sprintf("%s: %d викидів (поза [0,%d])\n",
                col, out_cnt, max_scores[col]))
  } else {
    cat(sprintf("%s: аналіз доменних меж не застосовано\n", col))
  }
  
  # Winsorizing (якщо викидів <5% від валідних)
  if (col %in% names(max_scores) && valid_cnt > 0 &&
      out_cnt > 0 && out_cnt/valid_cnt < 0.05) {
    tmp <- my_data[[col]]
    idx <- which(info$outliers)
    for (i in idx) {
      if (!is.na(tmp[i])) {
        tmp[i] <- min(max(tmp[i], info$lower), info$upper)
      }
    }
    my_data[[col]] <- tmp
    cat(sprintf("  -> значення в %s обмежені до [%.1f, %.1f]\n",
                col, info$lower, info$upper))
  }
  
  cat("\n")
}

# Виводимо таблицю нульових балів
print(zero_scores)

# Ми провели перевірку всіх оцінок за двома групами предметів - ті, що мають шкалу 
# від 100 до 200, і ті, де максимальною межею є різні значення від 32 до 54. Для 
# предметів із «100-бальною» шкалою нулі вважаються звичайним результатом 
# (не подолав поріг), а всі оцінки поза інтервалом [100;200] розглядаються як 
# викиди. Як видно, жодних викидів немає, тому можна сказати, що всі записи в межах 
# шкали 100-200 вірні. Аналогічно, для предметів із меншою шкалою (UkrBlockBall – до 45, 
# MathBlockBall – до 32, HistBlockBall – до 54 тощо) усі бали перевіряються в діапазоні 
# від 0 до відповідного максимуму, і теж викидів не виявилося. При цьому нульові 
# бали (тобто учні, які не набрали жодного балу) трапляються нечасто – максимальна 
# частка таких спостережень становить близько 12,8 % у MathBlockBall100 та 10,2 % 
# у PhysBlockBall100, а в інших предметах цей показник не перевищує кількох відсотків. 

#----- ПЕРЕКОДУВАННЯ ЗМІННИХ: SexTypeName, TestDate, TerTypeName -----

# SexTypeName: character → factor → integer
my_data$SexTypeName <- as.factor(my_data$SexTypeName)
my_data$SexTypeName <- as.integer(my_data$SexTypeName)

# TestDate: character → Date
my_data$TestDate <- as.Date(my_data$TestDate)

# TerTypeName: character → factor
unique_tertypes <- unique(my_data$TerTypeName)
cat("Унікальні типи територій (TerTypeName):\n")
print(unique_tertypes)

# Перетворення TerTypeName у фактор і перекодування в числа
my_data$TerTypeName <- as.factor(my_data$TerTypeName)
my_data$TerTypeName <- as.integer(my_data$TerTypeName)

View(my_data[1:10, ])

# Ми перекодували деякі зі змінних в integer для зручної роботи з ними під час досліджень та 
# наступного аналізу. 
# SexTypeName має значення:
# 1 - "жіноча",
# 2 - "чоловіча".
# TerTypeName має значення:
# 1 - "інша країна",
# 2 - "місто",
# 3 - "селище, село".
# Для TestDate змінено формат з dd.mm.yyyy на yyyy-mm-dd для зручності роботи з датами
# Додаткова трансформація змінних не потрібна, оскільки більшість числових змінних — це бали за шкалою,
# вже інтерпретуються однозначно і не потребують додаткового масштабування чи перетворення. 
# Вони мають чітке смислове значення, яке втратиться після логарифмування або нормалізації.
# Логарифмічна трансформація — не підходить через нулі. 
# У змінних типу BlockBall або BlockBall100 можуть бути нулячі значення, що означають:
# або учень не подолав поріг,
# або не з’явився на іспит (пропущені значення).
# Якщо планується описова статистика або побудова частот/розподілів, то стандартизація 
# (переведення в Z-оцінки) або нормалізація (від 0 до 1) спотворює сенс шкал, що також нам не підходить.

#----- ПЕРЕВІРКА ГІПОТЕЗ -----

# Гіпотеза 1: «Якщо у людини високі оцінки з одного філологічного предмету 
# (наприклад, з англійської мови), то з іншого філологічного предмету 
# (наприклад, з української мови) в неї, скоріш за все, також високі оцінки.»

# Видаляємо пропущені значення
data_lang <- na.omit(my_data[, c("EngBlockBall100", "UkrBlockBall100")])

# --- 1. T-тест ---
# Створимо групу: висока оцінка з англійської > 180
data_lang$high_eng <- ifelse(data_lang$EngBlockBall100 >= 180, "high", "low")
t_test_1 <- t.test(UkrBlockBall100 ~ high_eng, data = data_lang)
print(t_test_1)

# p-value < 2.2e-16 - різниця між середніми оцінками значуща.

# mean high_eng ≈ 172.6, mean low_eng ≈ 152.5 - отже, в тих, хто добре знає англійську, 
# в середньому на 20 балів вище з української.

# --- 2. ANOVA ---
anova_1 <- aov(UkrBlockBall100 ~ high_eng, data = data_lang)
summary(anova_1)

# Аналогічно: значуща різниця між групами (p < 2e-16).
# Підтверджує T-тест: фактор "high_eng" впливає на оцінку з української.

# --- 3. x-квадрат тест ---
# Категоризація обох предметів
data_lang$eng_cat <- ifelse(data_lang$EngBlockBall100 >= 180, "high", "low")
data_lang$ukr_cat <- ifelse(data_lang$UkrBlockBall100 >= 180, "high", "low")
table_lang <- table(data_lang$eng_cat, data_lang$ukr_cat)
chisq_test_1 <- chisq.test(table_lang)
print(chisq_test_1)

# Обидва предмети поділено на "високі/низькі".
# X^2 = 9446, p < 2.2e-16 - є залежність між високими оцінками з англійської та української.

# --- 4. Кореляція Пірсона ---
cor_test_1 <- cor.test(data_lang$EngBlockBall100, data_lang$UkrBlockBall100, method = "pearson")
print(cor_test_1)

# Кореляція: r = 0.475 - помірний прямий зв'язок.
# Значущість підтверджена (p < 2.2e-16).

# Висновок: Успішність в англійській пов’язана з успішністю в українській. Усі методи це підтвердили.

# Гіпотеза 2: «Якщо у людини високі оцінки з одного технічного предмету (наприклад, з фізики), 
# то з іншого технічного предмету (наприклад, з математики) в неї, скоріш за все, також високі оцінки.»

# Видаляємо пропущення
data_tech <- na.omit(my_data[, c("PhysBlockBall100", "MathBlockBall100")])

# --- 1. T-тест ---
data_tech$high_phys <- ifelse(data_tech$PhysBlockBall100 >= 180, "high", "low")
t_test_2 <- t.test(MathBlockBall100 ~ high_phys, data = data_tech)
print(t_test_2)

# p-value < 2.2e-16
# Середні:
# high_phys ≈ 189.0
# low_phys ≈ 123.6
# Різниця в середньому балів: ~65 балів!

# --- 2. ANOVA ---
anova_2 <- aov(MathBlockBall100 ~ high_phys, data = data_tech)
summary(anova_2)

# F = 495.8, p < 2e-16 - значуща різниця.
# Потужний вплив "high_phys" на оцінку з математики.

# --- 3. x-квадрат тест ---
data_tech$phys_cat <- ifelse(data_tech$PhysBlockBall100 >= 180, "high", "low")
data_tech$math_cat <- ifelse(data_tech$MathBlockBall100 >= 180, "high", "low")
table_tech <- table(data_tech$phys_cat, data_tech$math_cat)
chisq_test_2 <- chisq.test(table_tech)
print(chisq_test_2)

# X^2 = 1783.6, p < 2.2e-16 → існує залежність між категоріями "високо/низько".

# --- 4. Кореляція Спірмена ---
cor_test_2 <- cor.test(data_tech$PhysBlockBall100, data_tech$MathBlockBall100, method = "spearman")
print(cor_test_2)

# rho = 0.709 - сильна позитивна кореляція.
# Це ще кращий результат, ніж у філологічній гіпотезі.

# Висновок: Дуже сильна залежність — ті, хто сильні у фізиці, зазвичай сильні й у математиці.

# Гіпотеза 3: «Через більш доступну освіту люди з міст мають у середньому вищі оцінки з 
# української мови, ніж люди з сільської місцевості.»

# Вибираємо лише потрібні дані
data_geo <- subset(my_data, TerTypeName %in% c(2, 3))
data_geo <- na.omit(data_geo[, c("UkrBlockBall100", "TerTypeName")])

# --- 1. T-тест ---
t_test_3 <- t.test(UkrBlockBall100 ~ TerTypeName, data = data_geo)
print(t_test_3)

# Середні бали:
# місто ≈ 147.0
# село ≈ 143.4
# Різниця: ~3.6 бала, p < 2.2e-16 — статистично значуща, але невелика.

# --- 2. ANOVA ---
anova_3 <- aov(UkrBlockBall100 ~ TerTypeName, data = data_geo)
summary(anova_3)

# F = 1719, p < 2e-16 — підтверджує, що тип місцевості впливає на оцінку.

# --- 3. x-квадрат тест ---
# Категоризуємо бали
data_geo$ukr_cat <- ifelse(data_geo$UkrBlockBall100 >= 180, "high", "low")
table_geo <- table(data_geo$TerTypeName, data_geo$ukr_cat)
chisq_test_3 <- chisq.test(table_geo)
print(chisq_test_3)

# X^2 = 317.2, p < 2.2e-16 — наявна асоціація між типом території та шансом мати високий бал.

# --- 4. Лінійна регресія ---
lm_model <- lm(UkrBlockBall100 ~ TerTypeName, data = data_geo)
summary(lm_model)

# Коефіцієнт при TerTypeName = -3.62, тобто село ≈ -3.6 бала нижче за місто.
# R^2 = 0.006 - вплив на результат є, але не дуже сильний (тип місцевості пояснює лише 0.6% варіації).

# Висновок: Міські учні мають трохи вищі бали, але ефект незначний по силі, хоч і статистично значущий.

# У ході дослідження було перевірено три гіпотези, пов’язані з результатами учасників НМТ 2024 року:
# 1. Філологічна гіпотеза: Високі бали з англійської мови асоціюються з високими балами з української. 
# # Усі методи (t-тест, ANOVA, χ²-тест, кореляція Пірсона) показали статистично значущий зв’язок 
# (p-value < 0.001). Кореляція Пірсона = 0.47 вказує на помірний позитивний лінійний зв’язок.
# 2. Технічна гіпотеза: Високі бали з фізики пов’язані з високими балами з математики. 
# Результати усіх методів підтвердили гіпотезу (p-value < 0.001). 
# Спірманова кореляція = 0.71 вказує на сильний позитивний монотонний зв’язок.
# 3. Географічна гіпотеза: Учні з міських територій мають дещо вищі бали з української мови, 
# ніж учні з сіл. Хоча різниця у середніх балах невелика (~3.6 бала), вона статистично значуща 
# (усі p-value < 0.001). Проте коефіцієнт детермінації (R² ≈ 0.006) вказує, 
# що територіальний чинник слабко пояснює варіацію оцінок.
# Таким чином, перші дві гіпотези підтверджено з помірним/сильним ефектом, третю — лише частково, 
# з дуже слабким ефектом, хоч і статистично значущим.

# Обґрунтування використаних методів
# 1. T-тест
# Застосовано для порівняння середніх значень між двома незалежними групами 
# (наприклад, "високі" та "низькі" бали).
# Підходить для числових змінних з приблизно нормальним розподілом або великим розміром вибірки.
# 2. ANOVA
# Використовується для перевірки впливу факторної змінної (групи) на змінну-відповідь.
# Дає подібну інформацію, що й t-тест, але дозволяє розширення на більшу кількість груп.
# Дублює t-тест при 2 групах, але дає змогу оцінити загальну дисперсію та F-статистику.
# 3. X-квадрат тест
# Застосовано для категоріальних змінних (наприклад, "високий"/"низький" бал).
# Визначає, чи існує статистично значуща асоціація між двома категоріальними змінними.
# Не вимагає нормального розподілу, але потребує достатнього обсягу спостережень.
# 4. Кореляційний аналіз
# Пірсон — для вимірювання лінійного зв’язку між двома числовими змінними (як у гіпотезі 1).
# Спірман — (як аналог Пірсона) для оцінки монотонного зв’язку, стійкий до викидів і 
# не вимагає нормальності (як у гіпотезі 2, де є багато однакових балів).
# 5. Лінійна регресія
# Дозволяє оцінити вплив однієї змінної (тип населеного пункту) на іншу (бал з української).
# Дає змогу інтерпретувати ступінь зміни залежної змінної при зміні незалежної.
# Показує, наскільки добре незалежна змінна пояснює варіацію залежної (через R^2).

#----- РЕГРЕСІЙНИЙ АНАЛІЗ -----

# Визначаємо залежну змінну: результат з української мови (UkrBlockBall100)
# Незалежні змінні: тип території, стать, результати з інших предметів (математика, історія, англійська)
# Виключаємо учасників, які не подолали поріг (значення 0)

cat("1. У цій регресійній моделі ми розглядаємо лише учасників, які подолали пороговий бал з української мови.\n")
cat("2. Ми також враховуємо, що значення 0 в інших предметах (математика, історія, англійська) також означає непроходження порогового бала.\n")
cat("3. Тому ця модель відображає взаємозв'язки між балами для тих учасників, які успішно склали тести.\n")

library(car)  # для аналізу мультиколінеарності
library(dplyr)

# Вибираємо дані для регресійної моделі 
# Виключаємо учасників, які не подолали поріг з української мови
# Також враховуємо, що 0 у всіх BlockBall100 означає непроходження порога
reg_data <- my_data %>%
  filter(UkrBlockBall100 > 0) %>%  # Виключаємо тих, хто не подолав поріг з української
  # Замінюємо нульові значення в інших предметах на NA, оскільки вони також означають непроходження порогу
  mutate(
    MathBlockBall100 = ifelse(MathBlockBall100 == 0, NA, MathBlockBall100),
    HistBlockBall100 = ifelse(HistBlockBall100 == 0, NA, HistBlockBall100),
    EngBlockBall100 = ifelse(EngBlockBall100 == 0, NA, EngBlockBall100)
  ) %>%
  select(UkrBlockBall100, TerTypeName, SexTypeName, 
         MathBlockBall100, HistBlockBall100, EngBlockBall100) %>%
  na.omit()  # Видаляємо рядки з NA

# Перевіряємо розмір підготовленого набору даних
cat("Розмір датасету для регресійного аналізу:", nrow(reg_data), "рядків\n")

# Будуємо початкову модель з усіма змінними
model1 <- lm(UkrBlockBall100 ~ TerTypeName + SexTypeName + 
                       MathBlockBall100 + HistBlockBall100 + EngBlockBall100, 
                     data = reg_data)

# Виводимо результати початкової моделі
summary(model1)

# Аналіз проблеми мультиколінеарності
vif_values <- vif(model1)
print("Фактори інфляції дисперсії (VIF):")
print(vif_values)

# Побудова кореляційної матриці для перевірки можливої мультиколінеарності
cor_matrix <- cor(reg_data[,c("UkrBlockBall100", "MathBlockBall100", 
                                             "HistBlockBall100", "EngBlockBall100")], 
                        use = "complete.obs")
print("Кореляційна матриця предикторів:")
print(cor_matrix)

# Оптимізація моделі: видалення змінних з високою мультиколінеарністю
# або незначущих предикторів
# На основі результатів VIF та кореляції, створюємо оптимізовану модель
# (припускаючи, що HistBlockBall100 має найбільшу кореляцію з іншими предикторами)
model2 <- lm(UkrBlockBall100 ~ TerTypeName + SexTypeName + 
                       MathBlockBall100 + EngBlockBall100, 
                     data = reg_data)

# Порівняння моделей за допомогою ANOVA
anova(model2, model1)

# Перевірка залишків оптимізованої моделі
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

# Перевірка VIF для оптимізованої моделі
vif_values2 <- vif(model2)
print("VIF для оптимізованої моделі:")
print(vif_values2)

# Перевірка нормальності залишків за допомогою тесту Шапіро-Вілка на підвибірці
set.seed(123)  # для відтворюваності
resid_sample <- sample(residuals(model2), min(5000, length(residuals(model2))))
shapiro_test <- shapiro.test(resid_sample)
print("Тест Шапіро-Вілка на нормальність залишків (підвибірка):")
print(shapiro_test)

# Графічні методи перевірки нормальності
# Q-Q plot
qqnorm(residuals(model2), main = "Q-Q графік залишків")
qqline(residuals(model2), col = "red", lwd = 2)

# Гістограма залишків
hist(residuals(model2), breaks = 50, 
     main = "Гістограма залишків",
     xlab = "Залишки", 
     col = "lightblue", 
     border = "white",
     probability = TRUE)
curve(dnorm(x, mean = mean(residuals(model2)), sd = sd(residuals(model2))), 
      add = TRUE, col = "darkred", lwd = 2)

# Використання альтернативних тестів на нормальність
if (!requireNamespace("nortest", quietly = TRUE)) {
  install.packages("nortest")
}
library(nortest)

# Тест Андерсена-Дарлінга
ad_test <- ad.test(residuals(model2))
print("Тест Андерсена-Дарлінга на нормальність залишків:")
print(ad_test)

# Тест Колмогорова-Смирнова
ks_test <- ks.test(residuals(model2), "pnorm", 
                         mean = mean(residuals(model2)), 
                         sd = sd(residuals(model2)))
print("Тест Колмогорова-Смирнова на нормальність залишків:")
print(ks_test)

# Перевірка моделі з взаємодією між статтю та балами з математики
model3 <- lm(UkrBlockBall100 ~ TerTypeName + SexTypeName + 
               MathBlockBall100 + HistBlockBall100 + EngBlockBall100 + 
               SexTypeName:MathBlockBall100,
             data = reg_data
)

# Порівнюємо моделі з та без взаємодії
anova(model1, model3)
summary(model3)

# Визначаємо фінальну модель на основі результатів
final_model <- model2

# Якщо взаємодія виявилась значущою, використовуємо модель з взаємодією
if (anova(model1, model3)$"Pr(>F)"[2] < 0.05) {
  final_model <- model3
  cat("Використовуємо модель з взаємодією як фінальну\n")
} else {
  cat("Використовуємо модель без взаємодії як фінальну\n")
}

# Отримуємо коефіцієнти та метрики для фінальної моделі
summary_final <- summary(final_model)
print("Підсумок фінальної моделі:")
print(summary_final)

# Обчислюємо стандартизовані коефіцієнти для порівняння важливості предикторів
# Стандартизація змінних
reg_data_scaled <- reg_data
reg_data_scaled[, c("MathBlockBall100", "EngBlockBall100")] <- 
  scale(reg_data_scaled[, c("MathBlockBall100", "EngBlockBall100")])

# Модель зі стандартизованими предикторами
model_scaled <- lm(UkrBlockBall100 ~ TerTypeName + SexTypeName + 
                           MathBlockBall100 + EngBlockBall100, 
                         data = reg_data_scaled)

# Виводимо коефіцієнти стандартизованої моделі
print("Коефіцієнти стандартизованої моделі:")
print(summary(model_scaled)$coefficients)

# Розрахунок прогнозованих значень та довірчих інтервалів
predicted <- predict(final_model, interval = "confidence")
head(predicted)

# Графік фактичних проти прогнозованих значень
plot(reg_data$UkrBlockBall100, predicted[,"fit"], 
     xlab = "Фактичні значення UkrBlockBall100", 
     ylab = "Прогнозовані значення",
     main = "Порівняння фактичних і прогнозованих значень", 
     asp = 1)
abline(0, 1, col = "red", lwd = 2)  # лінія y=x

# Оцінка важливості змінних через крос-валідацію
set.seed(123)
train_indices <- sample(nrow(reg_data), size = 0.7*nrow(reg_data))
train_data <- reg_data[train_indices, ]
test_data <- reg_data[-train_indices, ]

# Навчання моделі на навчальній вибірці
train_model <- lm(formula(final_model), data = train_data)

# Оцінка на тестовій вибірці
predictions <- predict(train_model, newdata = test_data)
test_mse <- mean((test_data$UkrBlockBall100 - predictions)^2)
test_rmse <- sqrt(test_mse)
test_r2 <- 1 - sum((test_data$UkrBlockBall100 - predictions)^2) / 
                   sum((test_data$UkrBlockBall100 - mean(test_data$UkrBlockBall100))^2)

print("Результати оцінки на тестовій вибірці:")
cat("MSE:", test_mse, "\n")
cat("RMSE:", test_rmse, "\n")
cat("R²:", test_r2, "\n")

# Підсумок регресійного аналізу
cat("\n--- ПІДСУМОК РЕГРЕСІЙНОГО АНАЛІЗУ ---\n")
cat("R-квадрат фінальної моделі:", round(summary_final$r.squared, 4), "\n")
cat("Скоригований R-квадрат:", round(summary_final$adj.r.squared, 4), "\n")
cat("F-статистика:", round(summary_final$fstatistic[1], 2), 
    "з p-value <", format.pval(pf(summary_final$fstatistic[1], 
                                summary_final$fstatistic[2], 
                                summary_final$fstatistic[3], 
                                lower.tail = FALSE), digits = 3), "\n")

# Інтерпретація результатів регресійного аналізу
cat("\n--- ІНТЕРПРЕТАЦІЯ РЕЗУЛЬТАТІВ ---\n")
cat("1. Модель пояснює приблизно", 
    round(summary_final$r.squared * 100, 1), 
    "% варіації результатів з української мови для учасників.\n")

cat("2. Найбільш впливові фактори (за абсолютною величиною коефіцієнтів):\n")
coef_abs <- abs(coef(final_model))
coef_abs <- coef_abs[-1]  # видаляємо перехват
coef_names <- names(coef_abs)
sorted_indices <- order(coef_abs, decreasing = TRUE)
for (i in sorted_indices) {
  cat("   - ", coef_names[i], ": ", 
      ifelse(coef(final_model)[coef_names[i]] > 0, "позитивний", "негативний"), 
      " вплив\n", sep = "")
}

cat("3. Взаємодія між предикторами:", 
    ifelse("SexTypeName:MathBlockBall100" %in% names(coef(final_model)), 
           "присутня і значуща", "не значуща або відсутня"), "\n")

cat("4. Перевірка припущень регресійного аналізу:", 
    ifelse(shapiro_test$p.value > 0.05, 
           "розподіл залишків близький до нормального", 
           "розподіл залишків відхиляється від нормального"), "\n")

cat("5. Мультиколінеарність:", 
    ifelse(max(vif_values2) < 5, 
           "відсутня проблема мультиколінеарності", 
           "наявна проблема мультиколінеарності"), "\n")

#----- ЛОГІСТИЧНИЙ АНАЛІЗ -----
# - Визначення бінарної залежної змінної та предикоторів незалежних змінних -
# Залежна змінна:
# 1. Результат проходження НМТ з української мови: якщо результат >= 180, то "1", інакше - "0".
# Незалежні змінні:
# 1. Результат проходження НМТ з математики: якщо результат >= 180, то "1", інакше - "0".
# 2. Результат проходження НМТ з історії України: якщо результат >= 180, то "1", інакше - "0".
# 3. Стать учасинка НМТ: 0 - "жіноча", 1 - "чоловіча".
# 4. Місце проходження НМТ: 0 - "місто", 1 - "селище, село"

my_data$UkrBallBinary <- ifelse(my_data$UkrBlockBall100 >= 180, 1, 0)

my_data$MathBallBinary <- ifelse(my_data$MathBlockBall100 >= 180, 1, 0)

my_data$HistBallBinary <- ifelse(my_data$HistBlockBall100 >= 180, 1, 0)

# Раніше значення SexTypeName були перекодовані наступним чином:
# 1 - "жіноча",
# 2 - "чоловіча".
# Для створення бінарної змінної було прийнято рішення виконати змінення значень:
# 0 - "жіноча" (замість 1)
# 1 - "чоловіча" (замість 2)

# Також раніше значення TerTypeName були перекодовані наступним чином:
# 1 - "інша країна",
# 2 - "місто",
# 3 - "селище, село".
# Для створення бінарної змінної було прийнято рішення виконати змінення значень:
# 2 -> 0 ("місто")
# 3 -> 1 ("селище, село")
need_data <- subset(my_data, SexTypeName %in% c(1, 2) & TerTypeName %in% c(2, 3))
need_data$SexTypeBinary <- ifelse(need_data$SexTypeName == 2, 1, 0)
need_data$TerTypeBinary <- ifelse(need_data$TerTypeName == 2, 0, 1)

# - Використовуємо логістичну регресію для моделювання бінарної залежності - 

# Обрання ствопців, що будуть приймати участь у моделюванні
logistic_data <- need_data[, c("UkrBallBinary", "MathBallBinary", "HistBallBinary", "TerTypeBinary", "SexTypeBinary")]

# Видалення рядків із NA, оскільки значення NA присутнє лише в тих учасників, що
# не зʼявилися на НМТ. Врахування цих даних призведе до погіршення результатів
# логістичного аналізу.
logistic_data <- na.omit(logistic_data)

View(logistic_data[1:10, ])

# Логістична регресія
log_model <- glm(UkrBallBinary ~ MathBallBinary + HistBallBinary + TerTypeBinary + SexTypeBinary, data = logistic_data, family = "binomial")

# - Оцінка моделі -
summary(log_model)

# Результати:
#Call:
#  glm(formula = UkrBallBinary ~ MathBallBinary + HistBallBinary + 
#        TerTypeBinary + SexTypeBinary, family = "binomial", data = logistic_data)
#
#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)    -3.08202    0.01422 -216.69   <2e-16 ***
#  MathBallBinary  2.81293    0.03471   81.05   <2e-16 ***
#  HistBallBinary  3.18957    0.02713  117.59   <2e-16 ***
#  TerTypeBinary  -0.35551    0.02768  -12.85   <2e-16 ***
#  SexTypeBinary  -1.20536    0.02351  -51.26   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 106437  on 280729  degrees of freedom
#Residual deviance:  77667  on 280725  degrees of freedom
#AIC: 77677
#
#Number of Fisher Scoring iterations: 7

#Інтерпретація отриманих результатів:
#Значення у стовпці Pr(>|z|) демонструють статистичну значущість змінних, що 
#були використані в ході моделювання бінарної залежності. 
#Також можна побачити, що отримання високих балів з математики та історії України
#підвищують шанси отримання високих балів з НМТ з української мови. 
#Місце складання НМТ має вплив на отримання високих результатів з предметів:
#учасники, що складали НМТ у селищі чи селі, мають менші шанси отримати високі результати,
#ніж ті, хто складав НМТ у містах.
#Також помітно, що учасники НМТ чоловічої статі мають менші шанси отримати високі бали
#з української мови, ніж учасники жіночої статі.

#Використання відношення шансів (Odds Ratio) дл демонстрації звʼязку між впливом
#факторів та результатом.
exp(log_model$coefficients)

#Результати:
#(Intercept) MathBallBinary HistBallBinary  TerTypeBinary  SexTypeBinary 
#0.04586639    16.65871830    24.27791843     0.70081434     0.29958294 

#Інтерпретація отриманих результатів:
#Отримані результати відношення шансів підтверджують результати вже отриманих
#коефіцієнтів. Можна помітити, що успішне складання НМТ з математики (>=180 балів) 
#збільшує шанси отримання високих результатів з української мови у ~16.65 разів.
#Успішне складання НМТ з історії України (>=180 балів) збільшує шанси отримання 
#високих результатів з української мови у ~24.27 рази.
#Шанси отримання високих балів з української мови зменшуються на 30% в тих учасників,
#що складали НМТ у селищі чи селі.
#У чоловіків шанси отримати >=180 балів на НМТ з української мови на 70% нижчі,
#ніж в жінок.

#- Побудова графіка для візуалізації результатів -
odds_ratios <- c(
  MathBallBinary = 16.66, HistBallBinary = 24.28, 
  TerTypeBinary = 0.70, SexTypeBinary = 0.30
)

odds_df <- data.frame(
  Predictor = names(odds_ratios),
  OddsRatio = odds_ratios
)

ggplot(odds_df, aes(x = Predictor, y = OddsRatio, fill = Predictor)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(OddsRatio, 2)), vjust = -0.5) +
  xlab("Predictors") + ylab("Odds Ratio") +
  ggtitle("Шанси отримання >= 180 балів з української мови") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# - Побудова ROC-кривої та отримання значення AUC -
#Встановлення необхідної бібліотеки
library(ROCR)

predictions <- predict(log_model, type = "response")
ROCRpred <- prediction(predictions, logistic_data$UkrBallBin)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

auc_perf <- performance(ROCRpred, "auc")
auc_value <- auc_perf@y.values[[1]]
text(x = 0.5, y = 0.3, labels = paste0("AUC = ", round(auc_value, 7)), cex = 1.5, col = "blue")

#Результати:
#0.8148797

#Інтерпретація отриманих результатів:
#На отриманому графіку можна побачити, що крива вигинається до лівого верхнього
#кута. Це свідчить про високу чутливість моделі та її здатність до надання коректних
#результатів. Значення AUC на рівні 0.8148 вказує на високу здатність моделі 
#розрізняти позитивні та негативні класи. Тобто, модель із високою ймовірністю зможе
#передбачити, які учасники отримають >= 180 балів з НМТ з української мови.

# 6. МЕТОДИ МАШИННОГО НАВЧАННЯ
# ----- ЗАДАЧА РЕГРЕСІЇ ----- 

# Завантажуємо необхідні бібліотеки
if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart") # для дерева рішень
}
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest") # для випадкового лісу
}
if (!requireNamespace("xgboost", quietly = TRUE)) {
  install.packages("xgboost") # для глибокого навчання
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret") # для крос-валідації та оптимізації параметрів
}
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071") # для SVM
}

library(rpart)
library(randomForest)
library(xgboost)
library(caret)
library(e1071)
library(Metrics) # для оцінки метрик моделей

# Функція для оцінки регресійних моделей
evaluate_regression_model <- function(predictions, actuals, model_name) {
  mae_value <- mae(actuals, predictions) # середня абсолютна помилка (показує, наскільки відхиляються прогнози від фактичних значень)
  rmse_value <- rmse(actuals, predictions) # середньоквадратична помилка (корінь із середнього квадратів похибки)
  r2_value <- 1 - sum((actuals - predictions)^2) / sum((actuals - mean(actuals))^2) # коефіцієнт детермінації (показує, яку частку дисперсії пояснює модель)
  
  cat("\n", model_name, "Evaluation:\n") # виводимо назву моделі
  cat("MAE:", round(mae_value, 4), "\n") # середня абсолютна помилка
  cat("RMSE:", round(rmse_value, 4), "\n") # середньоквадратична помилка
  cat("R²:", round(r2_value, 4), "\n") # коефіцієнт детермінації 
  
  return(data.frame( # повертаємо результати у вигляді таблиці
    Model = model_name, 
    MAE = round(mae_value, 4), 
    RMSE = round(rmse_value, 4),
    R_squared = round(r2_value, 4)
  ))
}

# Підготовка даних для регресії
# Виключаємо рядки з NA, готуємо дані для навчання
set.seed(123)
reg_data <- my_data %>%
  filter(UkrBlockBall100 > 0) %>% # відфільтровуємо рядки з 0 балів (тільки ті, хто подолав поріг з української)
  mutate(
    MathBlockBall100 = ifelse(MathBlockBall100 == 0, NA, MathBlockBall100), # виключаємо 0 балів з математики, замінюючи їх на NA
    HistBlockBall100 = ifelse(HistBlockBall100 == 0, NA, HistBlockBall100), # виключаємо 0 балів з історії, замінюючи їх на NA
    EngBlockBall100 = ifelse(EngBlockBall100 == 0, NA, EngBlockBall100) # виключаємо 0 балів з англійської, замінюючи їх на NA
  ) %>%
  select(UkrBlockBall100, TerTypeName, SexTypeName, 
         MathBlockBall100, HistBlockBall100, EngBlockBall100) %>% # вибираємо необхідні стовпці
  na.omit() # видаляємо рядки з NA

# Розділення даних на тренувальну та тестову вибірки (70/30)
set.seed(123) # встановлюємо seed для повторюваності результатів
train_indices <- createDataPartition(reg_data$UkrBlockBall100, p = 0.7, list = FALSE) # розділяємо дані на тренувальну та тестову вибірки
train_data <- reg_data[train_indices, ] # тренувальна вибірка
test_data <- reg_data[-train_indices, ] # тестова вибірка

# Зберігаємо фактичні значення для тестової вибірки
y_test <- test_data$UkrBlockBall100

# Створюємо таблицю результатів для порівняння всіх моделей
regression_results <- data.frame(
  Model = character(),
  MAE = numeric(),
  RMSE = numeric(),
  R_squared = numeric(),
  stringsAsFactors = FALSE
)

# 1. ЛІНІЙНА РЕГРЕСІЯ
linear_model <- lm(UkrBlockBall100 ~ TerTypeName + SexTypeName + 
                    MathBlockBall100 + HistBlockBall100 + EngBlockBall100, 
                  data = train_data) # тренуємо модель

linear_predictions <- predict(linear_model, newdata = test_data) # прогнозуємо значення для тестової вибірки
linear_results <- evaluate_regression_model(linear_predictions, y_test, "Linear Regression") # оцінюємо модель
regression_results <- rbind(regression_results, linear_results) # додаємо результати до таблиці

# MAE склав 7.9864 бали, тобто в середньому прогнози моделі відхиляються від реальних 
# значень приблизно на 8 балів, що досить суттєво, враховуючи 200-бальну шкалу. 
# RMSE дорівнює 10.3697, що показує середньоквадратичне відхилення прогнозів – цей 
# показник більш чутливий до великих помилок, тому його значення вище за MAE.
# Коефіцієнт детермінації R² становить 0.5876, що означає, що модель пояснює лише 
# близько 59% варіації балів з української мови. Інакше кажучи, майже половина 
# варіації залишається непоясненою цією моделлю.
# Загалом, хоча модель має певну прогностичну силу, її точність не надто висока. 
# Лінійна регресія не здатна вловити складні нелінійні залежності, які, судячи з 
# результатів, наявні в даних. Це вказує на потребу застосувати більш складні 
# методи машинного навчання для покращення якості прогнозів.

# 2. ДЕРЕВО РІШЕНЬ ( знаходить найкращий поділ даних для прогнозування цільової змінної)
# Налаштування параметрів через 5-кратну крос-валідацію
set.seed(123) 
# Визначаємо сітку параметрів для дерева рішень
rpart_grid <- expand.grid(
  cp = seq(0.001, 0.05, by = 0.005)  # параметр складності
)

# Налаштування контролю для крос-валідації
train_control <- trainControl( # контроль навчання
  method = "cv", # метод крос-валідації
  number = 5, # кількість фолдів
  verboseIter = FALSE # виводимо ітерації
)

# Тренування дерева рішень з оптимізацією параметрів
rpart_model <- train(
  UkrBlockBall100 ~ TerTypeName + SexTypeName + MathBlockBall100 + HistBlockBall100 + EngBlockBall100, # формула моделі
  data = train_data, # тренувальні дані
  method = "rpart", # метод навчання (дерево рішень)
  trControl = train_control, # контроль навчання
  tuneGrid = rpart_grid # сітка параметрів
)

# Виведення оптимальних параметрів
cat("Оптимальний параметр складності (cp) для дерева рішень:", rpart_model$bestTune$cp, "\n")

# Прогнозування за допомогою дерева рішень
rpart_predictions <- predict(rpart_model, newdata = test_data) # прогнозуємо значення для тестової вибірки
rpart_results <- evaluate_regression_model(rpart_predictions, y_test, "Decision Tree") # оцінюємо модель
regression_results <- rbind(regression_results, rpart_results) # додаємо результати до таблиці

# Процес навчання включав оптимізацію параметра складності (cp) за допомогою 5-кратної
# крос-валідації, де дані розбивалися на 5 частин для пошуку найкращого значення cp.
# Оптимальний параметр складності виявився досить низьким – 0,001. Це означає, що 
# найкраще дерево є досить деталізованим, тобто модель намагається виявити навіть 
# дрібні закономірності в даних.
# За результатами тестування на відкладеній вибірці, ми отримали MAE = 8.3226, що 
# майже ідентично лінійній регресії (7.9864). RMSE дорівнює 10.8578, що трохи гірше за 
# лінійну регресію (10.3697), а коефіцієнт детермінації R² = 0.5479, тобто модель 
# пояснює лише 55% варіації даних, що теж трохи гірше за лінійну регресію (59%).
# Цікаво, що дерево рішень, незважаючи на здатність вловлювати нелінійні 
# закономірності, не дало значного покращення порівняно з лінійною регресією. Це 
# може свідчити про те, що в даних все ж переважають лінійні залежності, або що 
# для даної задачі потрібні більш складні моделі, які здатні краще узагальнювати 
# закономірності.

# 3. RANDOM FOREST (ансамбль дерев рішень, які "голосують" за результа)
# Налаштування параметрів через 5-кратну крос-валідацію
set.seed(123)
# Визначаємо сітку параметрів для Random Forest (менша сітка для економії часу)
rf_grid <- expand.grid(
  mtry = c(2, 3, 4),       # кількість змінних на розщеплення
  splitrule = "variance",  # правило поділу для регресії
  min.node.size = c(5, 10) # мінімальна кількість спостережень у вузлі
)

# Тренування Random Forest з оптимізацією параметрів
rf_model <- train(
  UkrBlockBall100 ~ TerTypeName + SexTypeName + MathBlockBall100 + HistBlockBall100 + EngBlockBall100,
  data = train_data, # тренувальні дані
  method = "ranger", # метод навчання (Random Forest) з підтримкою паралельних обчислень
  trControl = train_control, # контроль навчання
  tuneGrid = rf_grid, # сітка параметрів
  num.trees = 100  # обмежена кількість дерев для швидкості змініть на 10
)

# Виведення оптимальних параметрів
cat("Оптимальний параметр mtry для Random Forest:", rf_model$bestTune$mtry, "\n")

# Прогнозування за допомогою Random Forest
rf_predictions <- predict(rf_model, newdata = test_data) # прогнозуємо значення для тестової вибірки
rf_results <- evaluate_regression_model(rf_predictions, y_test, "Random Forest") # оцінюємо модель
regression_results <- rbind(regression_results, rf_results) # додаємо результати до таблиці

# Random Forest "об'єднує думки" різних дерев для отримання кінцевого прогнозу. 
# Для налаштування моделі ми використовували 5-кратну крос-валідацію з різними 
# значеннями параметру mtry (2, 3 та 4), який визначає кількість змінних, що 
# розглядаються при кожному розгалуженні дерева.
# Оптимальним виявилось значення mtry = 2. Для економії обчислювальних ресурсів ми 
# обмежили кількість дерев до 100, але навіть з цим обмеженням модель показала 
# кращі результати, ніж попередня: MAE = 7.9913 , RMSE = 10.4461  і R² = 0.5815. Це 
# свідчить про те, що Random Forest трішки краще справляється з прогнозуванням балів НМТ 
# з української мови, ніж звичайне дерево рішень і сильно наблизився до лінійної регресії. 

# 4. XGBoost (гібридна модель, яка об'єднує дерева рішень та градієнтний спуск)
# Підготовка даних для XGBoost
X_train <- model.matrix(UkrBlockBall100 ~ TerTypeName + SexTypeName + MathBlockBall100 + 
  HistBlockBall100 + EngBlockBall100 - 1, data = train_data) # створюємо матрицю ознак для тренувальних даних
X_test <- model.matrix(UkrBlockBall100 ~ TerTypeName + SexTypeName + MathBlockBall100 + 
  HistBlockBall100 + EngBlockBall100 - 1, data = test_data) # створюємо матрицю ознак для тестових даних
y_train <- train_data$UkrBlockBall100 # фактичні значення для тренувальної вибірки

# Створення DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = y_train) # створюємо DMatrix для тренувальних даних
dtest <- xgb.DMatrix(data = X_test, label = y_test) # створюємо DMatrix для тестових даних

# Налаштування параметрів для XGBoost
xgb_params <- list(
  objective = "reg:squarederror", # цільова функція (квадратична помилка)
  eval_metric = "rmse", # оцінка якості (середньоквадратична помилка)
  eta = 0.1, # швидкість навчання (0.1 означає, що на кожній ітерації модель коригується на 10% помилки)
  max_depth = 6, # максимальна глибина дерева
  min_child_weight = 1, # мінімальна вага листя
  subsample = 0.8, # частка випадкових спостережень, що використовуються для навчання кожного дерева
  colsample_bytree = 0.8 # частка змінних, що використовуються для навчання кожного дерева
)

# Крос-валідація для вибору оптимальної кількості ітерацій
set.seed(123) # встановлюємо seed для повторюваності результатів
xgb_cv <- xgb.cv(
  params = xgb_params, # параметри для XGBoost
  data = dtrain, # тренувальні дані
  nrounds = 100, # кількість ітерацій
  nfold = 5, # кількість фолдів
  early_stopping_rounds = 10, # кількість ітерацій для ранньої зупинки
  verbose = FALSE # виводимо ітерації
)

# Отримання оптимальної кількості ітерацій
optimal_nrounds <- xgb_cv$best_iteration
cat("Оптимальна кількість ітерацій для XGBoost:", optimal_nrounds, "\n")

# Тренування XGBoost з оптимальними параметрами
xgb_model <- xgb.train(
  params = xgb_params, # параметри для XGBoost
  data = dtrain, # тренувальні дані
  nrounds = optimal_nrounds, # оптимальна кількість ітерацій
  verbose = FALSE # виводимо ітерації
)

# Прогнозування за допомогою XGBoost
xgb_predictions <- predict(xgb_model, dtest) # прогнозуємо значення для тестової вибірки
xgb_results <- evaluate_regression_model(xgb_predictions, y_test, "XGBoost") # оцінюємо модель
regression_results <- rbind(regression_results, xgb_results) # додаємо результати до таблиці

# Для налаштування моделі ми використали ряд параметрів, які контролюють процес 
# навчання: швидкість навчання 0.1, максимальна глибина дерев 6, а також параметри 
# підвибірки даних і ознак на рівні 0.8.
# Для визначення оптимальної кількості ітерацій навчання застосувалм 5-кратну 
# крос-валідацію з механізмом ранньої зупинки, яка показала, що найкращі 
# результати досягаються на 64-й ітерації.
# Результати тестування демонструють, що XGBoost забезпечує найкращу точність 
# прогнозування за показником RMSE (10.276) серед усіх випробуваних моделей. 
# Значення MAE (7.8505) також краще ніж у лінійної регресії та дерева рішень, а 
# коефіцієнт детермінації R² (0.595) вказує на високу пояснювальну здатність моделі майже 60%. 
# XGBoost виявився дуже ефективним у вирішенні цієї задачі регресії, що підтверджує 
# його репутацію як одного з найпотужніших алгоритмів машинного навчання.

# Порівняння всіх регресійних моделей
cat("\nПорівняння всіх регресійних моделей:\n")
print(regression_results)

# Аналізуючи таблицю порівняння чотирьох регресійних моделей, можна зробити декілька 
# важливих спостережень. Найкращі результати показала модель XGBoost з найнижчими 
# показниками помилок (MAE = 7.8505, RMSE = 10.276) та найвищим коефіцієнтом 
# детермінації (R² = 0.595). Це підтверджує перевагу складних ансамблевих методів 
# у розв'язанні задачі прогнозування балів НМТ з української мови.
# На другому місці опинилась Linear Regression з гіршими показниками (MAE = 7.9864 , 
# RMSE = 10.3697 , R² = 0.5876).
# На третьому місці опинився Random Forest з трохи гіршими показниками (MAE = 7.9913 , 
# RMSE = 10.4461 , R² = 0.5815), що також підкреслює ефективність ансамблевих методів.
# Найгірший результат показало Decision Tree з показниками 
# (MAE = 8.3226 , RMSE = 10.8578 , R² = 0.5479).
# Цікаво, що класична лінійна регресія виявилась більш ефективною, ніж одиночне дерево рішень 
# з точки зору пояснення дисперсії (R²).
# Загалом, результати підтверджують загальну тенденцію в машинному навчанні: 
# ансамблеві методи (особливо XGBoost) зазвичай перевершують одиночні моделі, а 
# також демонструють, що для цієї задачі різниця між моделями хоч і є, але не 
# настільки драматична (всі значення R² варіюються в межах 0.54-0.60).

# Графік порівняння метрик регресійних моделей
library(ggplot2)
library(reshape2)

# Підготовка даних для графіка
regression_results_melted <- melt(regression_results, id.vars = "Model", 
                                 variable.name = "Metric", value.name = "Value")

# Створення графіка порівняння моделей
ggplot(regression_results_melted, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Порівняння метрик регресійних моделей",
       x = "Модель", y = "Значення метрики") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Аналізуючи графік порівняння метрик регресійних моделей, можна зробити наступні 
# висновки:
# Усі чотири досліджені моделі демонструють доволі близькі результати, однак 
# простежується чітка тенденція до кращої ефективності ансамблевих методів. За 
# всіма трьома метриками найкращі показники має XGBoost 
# (MAE ≈ 7.85, RMSE ≈ 10.28, R² ≈ 0.595), що свідчить про його вищу точність і 
# кращу здатність пояснювати варіацію даних.
# Linear Regression та Random Forest займають друге місце з незначним відставанням 
# (MAE ≈ 7.99, RMSE ≈ 10.37 та RMSE ≈ 10.45, R² ≈ 0.58 та 0.59). 
# Лінійна регресія, попри свою простоту, показала кращі результати, 
# ніж дерево рішень, особливо за показниками RMSE та R².
# Загалом, різниця між моделями за коефіцієнтом детермінації не є критичною (в 
# межах 0.54-0.60), але для практичного застосування доцільно віддати перевагу 
# XGBoost. Ці результати підтверджують загальновідому перевагу 
# ансамблевих методів у вирішенні задач регресії.

# ----- ЗАДАЧА КЛАСИФІКАЦІЇ ----- 

# Використовуємо ті самі дані, що й для логістичної регресії

# Функція для оцінки моделей класифікації
evaluate_classification_model <- function(predictions, actual, model_name, pred_probs = NULL) {
  # Створюємо матрицю плутанини
  conf_matrix <- confusionMatrix(factor(predictions), factor(actual))
  
  # Отримуємо основні метрики
  accuracy <- conf_matrix$overall[["Accuracy"]] # точність
  precision <- conf_matrix$byClass[["Pos Pred Value"]] # точність
  recall <- conf_matrix$byClass[["Sensitivity"]] # повнота
  f1 <- conf_matrix$byClass[["F1"]] # F1-score
  
  # Розрахунок AUC, якщо доступні ймовірності
  auc_value <- NA
  if (!is.null(pred_probs)) {
    pred_obj <- prediction(pred_probs, actual)
    auc_value <- performance(pred_obj, "auc")@y.values[[1]]
  }
  
  # Виводимо результати
  cat("\n", model_name, "Evaluation:\n") # виводимо назву моделі
  cat("Accuracy:", round(accuracy, 4), "\n") # виводимо точність
  cat("Precision:", round(precision, 4), "\n") # виводимо точність
  cat("Recall:", round(recall, 4), "\n") # виводимо повноту
  cat("F1-score:", round(f1, 4), "\n") # виводимо F1-score
  if (!is.na(auc_value)) {
    cat("AUC:", round(auc_value, 4), "\n") # виводимо AUC
  }
  
  # Повертаємо дані у форматі таблиці
  result_df <- data.frame(
    Model = model_name, # виводимо назву моделі
    Accuracy = round(accuracy, 4), # виводимо точність
    Precision = round(precision, 4), # виводимо точність
    Recall = round(recall, 4), # виводимо повноту
    F1_score = round(f1, 4), # виводимо F1-score
    AUC = ifelse(!is.na(auc_value), round(auc_value, 4), NA) # виводимо AUC
  )
  
  return(result_df) # повертаємо дані у форматі таблиці
}

# Перевіряємо, чи існують logistic_data, якщо ні - використовуємо збережені дані
if (!exists("logistic_data")) {
  # Використовуємо дані з логістичного аналізу або створюємо нові
  need_data <- subset(my_data, SexTypeName %in% c(1, 2) & TerTypeName %in% c(2, 3)) # вибираємо дані
  need_data$SexTypeBinary <- ifelse(need_data$SexTypeName == 2, 1, 0) # створюємо бінарну змінну
  need_data$TerTypeBinary <- ifelse(need_data$TerTypeName == 2, 0, 1) # створюємо бінарну змінну
  
  # Створення бінарних змінних, якщо вони ще не існують
  if (!"UkrBallBinary" %in% colnames(need_data)) {
    need_data$UkrBallBinary <- ifelse(need_data$UkrBlockBall100 >= 180, 1, 0)
  }
  if (!"MathBallBinary" %in% colnames(need_data)) {
    need_data$MathBallBinary <- ifelse(need_data$MathBlockBall100 >= 180, 1, 0)
  }
  if (!"HistBallBinary" %in% colnames(need_data)) {
    need_data$HistBallBinary <- ifelse(need_data$HistBlockBall100 >= 180, 1, 0)
  }
  
  # Обрання стовпців для логістичної регресії
  logistic_data <- need_data[, c("UkrBallBinary", "MathBallBinary", "HistBallBinary", "TerTypeBinary", "SexTypeBinary")]
  logistic_data <- na.omit(logistic_data)
}

# Розділення даних на тренувальну та тестову вибірки (70/30)
set.seed(123) # встановлюємо seed для повторюваності результатів
train_indices <- createDataPartition(logistic_data$UkrBallBinary, p = 0.7, list = FALSE) # створюємо індекси для тренувальної вибірки
train_data_class <- logistic_data[train_indices, ] # створюємо тренувальну вибірку
test_data_class <- logistic_data[-train_indices, ] # створюємо тестову вибірку

# Зберігаємо фактичні значення для тестової вибірки
y_test_class <- test_data_class$UkrBallBinary

# Створюємо таблицю результатів для порівняння всіх моделей класифікації
classification_results <- data.frame(
  Model = character(),
  Accuracy = numeric(),
  Precision = numeric(),
  Recall = numeric(),
  F1_score = numeric(),
  AUC = numeric(),
  stringsAsFactors = FALSE
)

# 1. ЛОГІСТИЧНА РЕГРЕСІЯ
logistic_model <- glm(UkrBallBinary ~ MathBallBinary + HistBallBinary + TerTypeBinary + SexTypeBinary, 
                      data = train_data_class, 
                      family = "binomial") # створюємо модель логістичної регресії

# Прогнозування ймовірностей
log_probs <- predict(logistic_model, newdata = test_data_class, type = "response")
# Перетворення ймовірностей на класи
log_predictions <- ifelse(log_probs > 0.5, 1, 0)
# Оцінка моделі
log_results <- evaluate_classification_model(log_predictions, y_test_class, "Logistic Regression", log_probs) # оцінюємо модель
classification_results <- rbind(classification_results, log_results) # додаємо результати до таблиці

# Логістична регресія показала дуже високі результати у класифікації учасників за 
# ймовірністю отримання високого бала з української мови. Точність моделі (Accuracy) 
# становить 95,89%, що свідчить про надзвичайно високу загальну якість прогнозування. 
# При цьому модель демонструє високу точність у визначенні саме позитивного класу 
# (Precision = 96,56%), тобто майже не дає хибно-позитивних результатів.
# Особливо вражає показник повноти (Recall = 99,22%), який вказує на те, що модель 
# здатна виявити практично всіх учасників, які отримали високі бали. F1-score на 
# рівні 97,87% підтверджує збалансованість моделі з точки зору точності та повноти.
# Єдиним відносно нижчим показником є AUC (81,34%), хоча це все одно добрий результат. 
# Ця метрика показує здатність моделі розрізняти класи незалежно від порогу класифікації.
# Такі високі показники свідчать, що логістична регресія, попри свою простоту, дуже 
# ефективна для прогнозування високих балів з української мови на основі успішності 
# з інших предметів та демографічних характеристик учасників.

# 2. ДЕРЕВО РІШЕНЬ
# Визначаємо сітку параметрів для дерева рішень
rpart_grid_class <- expand.grid(
  cp = seq(0.001, 0.05, by = 0.005)  # параметр складності
)

# Налаштування контролю для крос-валідації
train_control_class <- trainControl(
  method = "cv", # метод крос-валідації
  number = 5, # кількість фолдів
  classProbs = TRUE, # використовуємо ймовірності
  summaryFunction = twoClassSummary # використовуємо функцію для оцінки
)

# Тренування дерева рішень з оптимізацією параметрів
set.seed(123) # встановлюємо seed для повторюваності результатів
# Перетворюємо бінарні змінні на фактори з правильними іменами
train_data_class$UkrBallBinaryFactor <- factor(train_data_class$UkrBallBinary, levels = c(0, 1), labels = c("No", "Yes"))
test_data_class$UkrBallBinaryFactor <- factor(test_data_class$UkrBallBinary, levels = c(0, 1), labels = c("No", "Yes"))

train_data_class$MathBallBinaryFactor <- factor(train_data_class$MathBallBinary, levels = c(0, 1), labels = c("No", "Yes"))
test_data_class$MathBallBinaryFactor <- factor(test_data_class$MathBallBinary, levels = c(0, 1), labels = c("No", "Yes"))

train_data_class$HistBallBinaryFactor <- factor(train_data_class$HistBallBinary, levels = c(0, 1), labels = c("No", "Yes"))
test_data_class$HistBallBinaryFactor <- factor(test_data_class$HistBallBinary, levels = c(0, 1), labels = c("No", "Yes"))

# Оновлений код тренування моделі з використанням факторних змінних
rpart_model_class <- train(
  UkrBallBinaryFactor ~ MathBallBinaryFactor + HistBallBinaryFactor + TerTypeBinary + SexTypeBinary,
  data = train_data_class,
  method = "rpart",
  trControl = train_control_class,
  tuneGrid = rpart_grid_class,
  metric = "ROC"
)

# Виведення оптимальних параметрів
cat("Оптимальний параметр складності (cp) для дерева рішень у задачі класифікації:", rpart_model_class$bestTune$cp, "\n")

# Прогнозування класів
rpart_predictions_class <- predict(rpart_model_class, newdata = test_data_class)
# Конвертуємо назад у числові значення для оцінки
rpart_predictions_numeric <- ifelse(rpart_predictions_class == "Yes", 1, 0)
# Прогнозування ймовірностей
rpart_probs_class <- predict(rpart_model_class, newdata = test_data_class, type = "prob")[, "Yes"]
# Оцінка моделі
rpart_results_class <- evaluate_classification_model(rpart_predictions_numeric, y_test_class, "Decision Tree", rpart_probs_class) # оцінюємо модель
classification_results <- rbind(classification_results, rpart_results_class) # додаємо результати до таблиці

# Дерево рішень продемонструвало надзвичайно високі показники ефективності в задачі 
# класифікації учасників НМТ за результатами української мови. Оптимальний параметр 
# складності моделі (cp = 0.016) був визначений методом 5-кратної крос-валідації.
# Основні метрики ефективності моделі:
# - Точність (Accuracy) 95,92% свідчить про високу загальну ефективність класифікації
# - Precision 96,69% показує, що модель майже не дає хибно-позитивних результатів
# - Recall 99,1% вказує на те, що модель виявляє практично всіх учасників з високими балами
# - F1-score 97,88% підтверджує збалансованість між точністю та повнотою
# Єдиним відносно слабким показником є AUC (68,36%), що свідчить про певні обмеження 
# моделі в розрізненні класів на всьому діапазоні порогових значень. Проте, 
# враховуючи інші високі показники, дерево рішень демонструє значну ефективність у 
# прогнозуванні високих балів з української мови на основі балів з математики, історії 
# та демографічних характеристик учасників.

# 3. RANDOM FOREST
# Визначаємо сітку параметрів для Random Forest
rf_grid_class <- expand.grid(
  mtry = c(2, 3, 4), # кількість ознак для розбиття
  splitrule = "gini", # правило розбиття
  min.node.size = c(1, 5, 10) # мінімальна кількість спостережень у листі
)

# Тренування Random Forest з оптимізацією параметрів
set.seed(123) # встановлюємо seed для повторюваності результатів
rf_model_class <- train(
  UkrBallBinaryFactor ~ MathBallBinaryFactor + HistBallBinaryFactor + TerTypeBinary + SexTypeBinary, # формула моделі
  data = train_data_class, # дані для тренування
  method = "ranger", # метод тренування
  trControl = train_control_class, # контроль тренування
  tuneGrid = rf_grid_class, # сітка параметрів
  num.trees = 100, # кількість дерев
  metric = "ROC", # метрика оцінки
  importance = "impurity" # важливість ознак
)

# Виведення оптимальних параметрів
cat("Оптимальні параметри для Random Forest у задачі класифікації:", 
    "mtry =", rf_model_class$bestTune$mtry, 
    "min.node.size =", rf_model_class$bestTune$min.node.size, "\n")

# Прогнозування класів
rf_predictions_class <- predict(rf_model_class, newdata = test_data_class)
rf_predictions_numeric <- ifelse(rf_predictions_class == "Yes", 1, 0)
# Прогнозування ймовірностей
rf_probs_class <- predict(rf_model_class, newdata = test_data_class, type = "prob")[, "Yes"]
# Оцінка моделі
rf_results_class <- evaluate_classification_model(rf_predictions_numeric, y_test_class, "Random Forest", rf_probs_class)
classification_results <- rbind(classification_results, rf_results_class)

# Алгоритм Random Forest продемонстрував надзвичайно високу ефективність у 
# класифікації учасників НМТ за результатами української мови. За допомогою 5-кратної 
# крос-валідації було визначено оптимальні параметри моделі: mtry = 2 (кількість 
# змінних для розгалуження) та min.node.size = 10 (мінімальна кількість спостережень у вузлі).
# Основні метрики ефективності:
# - Accuracy 95,78% свідчить про високу загальну точність моделі
# - Precision 95,91% вказує на низький рівень хибно-позитивних результатів
# - Recall 99,82% демонструє винятково високу здатність моделі знаходити майже всі випадки високих балів
# - F1-score 97,83% підтверджує оптимальний баланс між точністю та повнотою
# Особливо важливим є показник AUC (81,33%), який значно вищий порівняно з деревом 
# рішень (68,36%). Це підтверджує перевагу ансамблевих методів над одиночними 
# моделями завдяки об'єднанню результатів багатьох дерев.
# Random Forest досягає майже ідеальних результатів при виявленні учасників з 
# високими балами (Recall близький до 100%), що робить цей алгоритм особливо цінним 
# у задачах, де важливо не пропустити жодного потенційно високобального учасника.

# 4. XGBoost
# Підготовка даних для XGBoost
x_train_class <- model.matrix(~ as.factor(MathBallBinary) + as.factor(HistBallBinary) + TerTypeBinary + SexTypeBinary - 1, 
                              data = train_data_class) # створюємо матрицю ознак для тренувальної вибірки
x_test_class <- model.matrix(~ as.factor(MathBallBinary) + as.factor(HistBallBinary) + TerTypeBinary + SexTypeBinary - 1, 
                             data = test_data_class) # створюємо матрицю ознак для тестової вибірки
y_train_class <- train_data_class$UkrBallBinary # створюємо вектор цільової змінної для тренувальної вибірки

# Створення DMatrix
dtrain_class <- xgb.DMatrix(data = x_train_class, label = y_train_class) # створюємо DMatrix для тренувальної вибірки
dtest_class <- xgb.DMatrix(data = x_test_class, label = y_test_class) # створюємо DMatrix для тестової вибірки

# Налаштування параметрів для XGBoost
xgb_params_class <- list(
  objective = "binary:logistic", # цільова функція
  eval_metric = "auc", # метрика оцінки
  eta = 0.1, # швидкість навчання
  max_depth = 6, # максимальна глибина дерев
  min_child_weight = 1, # мінімальна вага листя
  subsample = 0.8, # частка даних для підбору
  colsample_bytree = 0.8 # частка ознак для підбору
)

# Крос-валідація для вибору оптимальної кількості ітерацій
set.seed(123) # встановлюємо seed для повторюваності результатів
xgb_cv_class <- xgb.cv( 
  params = xgb_params_class, # параметри для XGBoost
  data = dtrain_class, # дані для крос-валідації
  nrounds = 100, # кількість ітерацій
  nfold = 5, # кількість фолдів
  early_stopping_rounds = 10, # кількість ітерацій для зупинки
  verbose = FALSE # виводимо проміжні результати
)

# Отримання оптимальної кількості ітерацій
optimal_nrounds_class <- xgb_cv_class$best_iteration
cat("Оптимальна кількість ітерацій для XGBoost у задачі класифікації:", optimal_nrounds_class, "\n") 

# Тренування XGBoost з оптимальними параметрами
xgb_model_class <- xgb.train(
  params = xgb_params_class, # параметри для XGBoost
  data = dtrain_class, # дані для тренування
  nrounds = optimal_nrounds_class, # кількість ітерацій
  verbose = FALSE # виводимо проміжні результати
)

# Прогнозування ймовірностей
xgb_probs_class <- predict(xgb_model_class, dtest_class)
# Перетворення ймовірностей на класи
xgb_predictions_class <- ifelse(xgb_probs_class > 0.5, 1, 0)
# Оцінка моделі
xgb_results_class <- evaluate_classification_model(xgb_predictions_class, y_test_class, "XGBoost", xgb_probs_class)
classification_results <- rbind(classification_results, xgb_results_class)

# Модель XGBoost продемонструвала найвищу ефективність серед усіх досліджених 
# алгоритмів класифікації учасників НМТ за результатами української мови. За 
# допомогою 5-кратної крос-валідації з механізмом ранньої зупинки було визначено 
# оптимальну кількість ітерацій — 47, що забезпечило балансування між точністю та 
# перенавчанням.
# Основні метрики ефективності:
# - Accuracy 95,89% — найвища загальна точність серед усіх моделей
# - Precision 96,56% — найвища точність позитивних прогнозів, що свідчить про 
# мінімальну кількість хибно-позитивних результатів
# - Recall 99,22% — надзвичайно висока здатність виявляти учасників з високими балами
# - F1-score 97,87% — оптимальний баланс між точністю та повнотою
# - AUC 81,33% — однаковий показник з Random Forest і значно вищий, ніж у дерева рішень
# XGBoost ефективно поєднує множинні слабкі моделі (дерева рішень) з градієнтним 
# бустингом, що дозволяє йому досягти високої гнучкості при моделюванні складних 
# взаємозв'язків між предикторами.
# Порівняно з іншими моделями, XGBoost демонструє найбільш збалансовані результати 
# за всіма метриками, що робить його оптимальним вибором для прогнозування високих 
# балів з української мови на основі успішності з інших предметів та демографічних 
# характеристик учасників.

# Порівняння всіх класифікаційних моделей
cat("\nПорівняння всіх класифікаційних моделей:\n")
print(classification_results)

# Графік порівняння метрик класифікаційних моделей
classification_results_melted <- melt(classification_results, id.vars = "Model", 
                                      variable.name = "Metric", value.name = "Value")

# Створення графіка порівняння моделей
ggplot(classification_results_melted, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Порівняння метрик класифікаційних моделей",
       x = "Модель", y = "Значення метрики") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Висновки за результатами класифікації
cat("\n--- ВИСНОВКИ ЗА РЕЗУЛЬТАТАМИ КЛАСИФІКАЦІЇ ---\n")

# Визначення найкращої моделі за різними метриками
best_accuracy_model <- classification_results$Model[which.max(classification_results$Accuracy)] 
best_precision_model <- classification_results$Model[which.max(classification_results$Precision)]
best_recall_model <- classification_results$Model[which.max(classification_results$Recall)]
best_f1_model <- classification_results$Model[which.max(classification_results$F1_score)]
best_auc_model <- classification_results$Model[which.max(classification_results$AUC)]

# Визначення моделі, яка найчастіше виявляється кращою за різними метриками
model_names <- c(best_accuracy_model, best_precision_model, best_recall_model, best_f1_model, best_auc_model) # створюємо вектор з найкращих моделей
best_class_model <- names(sort(table(model_names), decreasing = TRUE)[1]) # визначаємо найкращу модель за більшістю метрик

cat("Результати порівняння моделей класифікації за різними метриками:\n")
cat("   - Найкраща модель за точністю (Accuracy): ", best_accuracy_model, "\n")
cat("   - Найкраща модель за точністю (Precision): ", best_precision_model, "\n")
cat("   - Найкраща модель за повнотою (Recall): ", best_recall_model, "\n")
cat("   - Найкраща модель за F1-score: ", best_f1_model, "\n")
cat("   - Найкраща модель за AUC: ", best_auc_model, "\n")
cat("   - Найкраща модель за більшістю метрик: ", best_class_model, "\n\n")


# Аналіз результатів чотирьох різних моделей класифікації демонструє, що всі 
# досліджені алгоритми досягають надзвичайно високої ефективності у прогнозуванні 
# високих балів з української мови.
# Основні спостереження:
# 1. Загальна точність (Accuracy) коливається в межах 95,78-95,92%, де найкращий 
# результат показує Decision Tree (95,92%), хоча різниця між моделями мінімальна.
# 2. Точність позитивних прогнозів (Precision) найвища у Decision Tree (96,69%), 
# що свідчить про найменшу кількість хибно-позитивних результатів у цієї моделі.
# 3. Повнота (Recall) досягає максимуму у Random Forest (99,82%), який демонструє 
# винятково високу здатність виявляти майже всі випадки високих балів.
# 4. F1-score майже ідентичний у всіх моделей (97,83-97,88%), що свідчить про 
# збалансованість між точністю та повнотою.
# 5. AUC значно вищий у Logistic Regression, Random Forest та XGBoost (≈81,33%) 
# порівняно з Decision Tree (68,36%), що вказує на кращу здатність цих моделей 
# розрізняти класи на різних порогових значеннях.
# Цікавим є те, що Logistic Regression і XGBoost демонструють практично ідентичні 
# результати за всіма метриками. Попри свою простоту, логістична регресія виявилася 
# настільки ж ефективною, як і більш складні ансамблеві методи.
# Враховуючи близькі значення метрик, для практичного застосування можна обрати 
# найпростішу модель – логістичну регресію, яка забезпечує відмінні результати при 
# найменших обчислювальних витратах.

