sbt "runMain LS.LSREPL --test"

sbt run

Лямбда-исчисление:
    Абстракция и применение:
        Реализованы через Abs и App.
        Поддерживается каррирование (например, σx.σy.x).

    β-редукция:
        Реализована в методе reduce для аппликации (App(Abs(param, body), arg)).
        Используется подстановка с учётом α-конверсии (substitute).

    α-конверсия:
        Есть в методе alphaConvert для избежания захвата переменных.
        Пример из тестов: (σx.σy.x) y → σx1.y (переименование y во избежание конфликта).

    Нормальная форма:
        Метод evaluate выполняет редукцию до нормальной формы (если она существует).

Сигма-исчисление:
    Кортежи:
        Реализованы как Tuple с поддержкой произвольного числа элементов.

        Пример: (x, y) → Tuple(List(Var(x), Var(y))).

    Проекции:
        Реализованы через Proj.

        Работают корректно: π1(x, y) → x, π2(x, y) → y.

    Комбинация с лямбда-исчислением:

        Поддерживается вложенность: (σx.(x, x)) z → (z, z).

Ограничения:
    η-редукция:
        Не реализована (например, σx.f x не сводится к f, если x не свободна в f).

    Строгость оценки:
        Используется аппликативный порядок (редуцируются аргументы перед подстановкой).

        Нет поддержки ленивой оценки.

    Синтаксис:
        Для абстракции используется σ/s вместо традиционного λ.
        Проекции записываются как π1, π2 и т.д. (начиная с 1, но преобразуются в 0-based индекс).

    Типизация:
        Нет системы типов (чисто бестиповое исчисление).
