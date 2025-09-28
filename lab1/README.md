# Лабораторная работа № 1


## Проект Эйлера №1, №30 

  * Студент: `Исупов Никита Александрович`
  * Группа: `P3331`
  * ИСУ: `408708`
  * Функциональный язык: `Erlang`

---
## Проблема № 1

* Название: `Multiples of 3 or 5`
* Задание: If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000.

---

## Решение через рекурсию

```erlang
sum_multiples_rec(0) ->
    0;
sum_multiples_rec(N) when (N rem 3 =:= 0 orelse N rem 5 =:= 0), N < 1000 ->
    N + sum_multiples_rec(N - 1);
sum_multiples_rec(N) ->
    sum_multiples_rec(N - 1).
```

## Решение через хвостовую рекурсию
```erlang
sum_multiples_tail(N) ->
    loop(N - 1, 0).

loop(0, Acc) ->
    Acc;
loop(N, Acc) when N rem 3 =:= 0; N rem 5 =:= 0 ->
    loop(N - 1, Acc + N);
loop(N, Acc) ->
    loop(N - 1, Acc).
```

## Решение через модульность 

```erlang
sum_multiples_module(N) when is_integer(N), N >= 0 ->
    Seq = lists:seq(1, N - 1),
    Multiples = lists:filter(
        fun(X) -> (X rem 3) =:= 0 orelse (X rem 5) =:= 0 end,
        Seq
    ),
    lists:foldl(
        fun(X, Acc) -> Acc + X end,
        0,
        Multiples
    ).
```

## Испоьльзование map

```erlang
sum_multiples_map(N) ->
    Seq = lists:seq(1, N - 1),
    Map = lists:map(
        fun(X) ->
            if
                X rem 3 =:= 0; X rem 5 =:= 0 -> X;
                true -> 0
            end
        end,
        Seq
    ),
    lists:sum(Map).
```

## List Comprehension

```erlang
sum_multiples_comp(N) ->
    lists:sum([X || X <- lists:seq(1, N - 1), (X rem 3 =:= 0) orelse (X rem 5 =:= 0)]).
```
## Решение через императивный язык
```java
public static int task1(int n) {
    int ans = 0;
    for (int i = 0; i < n; i++) {
        if (i % 3 == 0 || i % 5 == 0) ans += i;
    }
    return ans;
}
```
## Проблема № 30

* Название: `Digit Fifth Powers`
* Задание: Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

---

## Идея решения
Очевидно, что мы не сможем перебрать все числа, поэтому надо понять верхнюю границу, выше которой перебирать нет смысла. 
Максимальная цифра в любом числе это `9`, `9^5 = 59049`. 

* Число из одной цифры `9` дает `59049`. 
* Из двух цифр `9` соответственно `9^5 + 9^5 = 118098`.
* Трехзначное `999` `3 * 9^5 = 177147`.
* Четырехзначное `9999` `4 * 9^5 = 236196`.
* Пятизначное `99999` `5 * 9^5 = 295235`.
* Шестизначное `999999` `6 * 9^5 = 354294`.
* Семизначное `9999999` `7 * 9^5 = 413343`.

Отсюда видно, что даже максимально возможное семизначное число в итоге дает только шестизначное. Так как меньшие семизначные числа дадут еще меньшие суммы цифр, среди всех этих чисел не могут быть ответы. В итоге, максимальным числом, являющимся претендетном на ответ, может быть только шестизначное `6 * 9^5 = 354294`.

## Вспомогательные функции 
```erlang 
pow_list() ->
    [0, 1, 32, 243, 1024, 3125, 7776, 16807, 32768, 59049].

digits(0) -> [0];
digits(N) when N > 0 -> loop(N, []).

loop(0, Acc) ->
    Acc;
loop(N, Acc) ->
    D = N rem 10,
    loop(N div 10, [D | Acc]).

sum_digit_pows(N) ->
    Digits = digits(N),
    Pows = pow_list(),
    lists:foldl(fun(D, Acc) -> Acc + lists:nth(D + 1, Pows) end, 0, Digits).
```
## Решение через рекурсию

```erlang
sum_powers_rec() ->
    sum_powers_rec(?LIMIT).

sum_powers_rec(1) ->
    0;
sum_powers_rec(N) ->
    Rest = sum_powers_rec(N - 1),
    case sum_digit_pows(N) =:= N of
        true -> Rest + N;
        false -> Rest
    end.
```

## Решение через хвостовую рекурсию
```erlang 
sum_powers_tail() ->
    loop(?LIMIT, 0).

loop(1, Acc) ->
    Acc;
loop(N, Acc) ->
    UpdateAcc =
        case sum_digit_pows(N) =:= N of
            true -> Acc + N;
            false -> Acc
        end,
    loop(N - 1, UpdateAcc).
```

## Решение через модульность 
```erlang
sum_powers_module() ->
    Seq = lists:seq(2, ?LIMIT),
    Ans = lists:filter(fun(N) -> sum_digit_pows(N) =:= N end, Numbers),
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, Ans).
```
## Использование map
```erlang 
sum_powers_map() ->
    Seq = lists:seq(2, ?LIMIT),
    Map = lists:map(fun(N) -> {N, sum_digit_pows(N)} end, Numbers),
    Ans = lists:filter(fun({N, S}) -> N =:= S end, Map),
    lists:foldl(fun({N, _}, Acc) -> N + Acc end, 0, Ans).
```

## List comprehension
```erlang
sum_powers_comp() ->
    lists:sum([N || N <- lists:seq(2, ?LIMIT), sum_digit_pows(N) =:= N]).
```
## Решение через императивный язык
```java
final static int LIMIT = 354294;
final static int[] POWS = {0, 1, 32, 243, 1024, 3125, 7776, 16807, 32768, 59049};

public static int sumPowers(int n) {
    int sum = 0;
    while (n > 0) {
        int digit = n % 10;
        sum += POWS[digit];
        n /= 10;
    }
    return sum;
}

public static int task30() {
    int ans = 0;
    for (int i = 2; i <= LIMIT; i++) {
        if (i == sumPowers(i)) ans += i;
    }
    return ans;
}
```

## Выводы 

В процессе выполнения данной лабораторной работы я познакомился с основными концепциями функционального программирования, а также с синтаксическими возможностями языка `Erlang`. Например для реализации циклов использовал рекурсию (обычную и хвостовую), для ветвления - pattern matching. Также попробовал list comprehension - конструкция для создания списков по определнным правилам. В итоге могу сказать, что функциональный подход предлагает элегантные и выразительные средства решения задач, особенно эффективные для математических вычислений и обработки данных.
