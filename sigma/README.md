sbt "runMain sigma.SigmaREPL --test"

sbt run

сигма-исчисление — вариант лямбда-исчисления с добавлением пар и проекций

Основные конструкции
1. Переменные:
    Базовые элементы вида x, y, z и т.д.

2. Сигма-абстракция:
    Аналог лямбда-абстракции: σx.M, где x — параметр, M — тело.
    
    Пример: σx.x — тождественная функция.

3. Пары:
    Упорядоченные пары вида (M, N).

    Пример: (x, y) — пара из x и y.

4. Проекции:

    π₁M (или fst M) — первая проекция, извлекает первый элемент пары.

    π₂M (или snd M) — вторая проекция, извлекает второй элемент пары.

5. Применение:
    Аппликация функции к аргументу: M N.
    Пример: (σx.x) y применяет тождественную функцию к y.


Вычислительные правила
1. β-редукция:
    (σx.M) N → [N/x]M — подстановка аргумента N вместо x в M.
    Пример: (σx.x) y → y.

2. Правила для пар:

    π₁(M, N) → M
    π₂(M, N) → N

3. α-конверсия:
    Переименование связанных переменных для избежания конфликтов.
    Пример: σx.σy.x y → σx1.σy.x1 y при необходимости.

Интерпретатор вычисляет выражения до нормальной формы(нельзя применить больше ни одно правило редукции)

Процесс:
    Разбор входной строки в абстрактное синтаксическое дерево (AST).
    Последовательное применение правил редукции.
    Вывод результата в удобочитаемой форме.

Примеры вычислений

    Тождественная функция:
    (σx.x) y → y

    Работа с парами:
    π₁( (σx.(x, y)) z ) → π₁(z, y) → z

    α-конверсия:
    (σx.σy.x) y → σx1.y (избегаем захвата переменной)
