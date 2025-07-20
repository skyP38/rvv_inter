sbt "runMain lambda.LambdaREPL --test"

sbt run

Работа парсера(на поиск ошибок)
Testing '(λfλx.f (f x)) (λz.z) w'... ✗ Parse error: Position 1:4, found "λx.f (f x)"