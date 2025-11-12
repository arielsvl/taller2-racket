#lang racket
;; ===============================================
;; Taller 2 – Programación Declarativa
;; Estudiante: Alan Ariel Flores Martinez 00019420
;; ===============================================

;; Ejercicio 1 – Contar elementos positivos en una lista
;; Usar filter y length.
(define (contar-positivos lst)
  (length (filter (lambda (x) (> x 0)) lst)))

;; Ejemplo:
;; (contar-positivos '(3 -2 7 0 -5 9)) => 3
(displayln "Resultado Ejercicio 1:")
(displayln (contar-positivos '(3 -2 7 0 -5 9)))

;; Ejercicio 2 – Generar lista de cuadrados pares
;; map y filter secuenciales.
(define (cuadrados-pares lst)
  (map (lambda (x) (* x x))
       (filter even? lst)))

;; Ejemplo:
;; (cuadrados-pares '(1 2 3 4 5 6 7 8)) => '(4 16 36 64)
(displayln "Resultado Ejercicio 2:")
(displayln (cuadrados-pares '(1 2 3 4 5 6 7 8)))

;; Ejercicio 3 – Calcular el factorial de un número
;; Usar recursión simple.
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Ejemplo:
;; (factorial 5) => 120
(displayln "Resultado Ejercicio 3:")
(displayln (factorial 5))

;; Ejercicio 4 – Elevar cada número al cubo
;; Usar lambda dentro de map.
(define (cubos-lista lst)
  (map (lambda (x) (* x x x)) lst))

;; Ejemplo:
;; (cubos-lista '(2 3 4)) => '(8 27 64)
(displayln "Resultado Ejercicio 4:")
(displayln (cubos-lista '(2 3 4)))

;; Ejercicio 5 – Sumar todos los elementos impares
;; Usar filter y foldl.
(define (sumar-impares lst)
  (foldl + 0 (filter odd? lst)))

;; Ejemplo:
;; (sumar-impares '(1 2 3 4 5 6 7)) => 16
(displayln "Resultado Ejercicio 5:")
(displayln (sumar-impares '(1 2 3 4 5 6 7)))

;; Ejercicio 6 – Determinar si una lista contiene números negativos
;; Usar ormap con una lambda.
(define (contiene-negativo? lst)
  (ormap (lambda (x) (< x 0)) lst))

;; Ejemplo:
;; (contiene-negativo? '(5 9 -3 2)) => #t
(displayln "Resultado Ejercicio 6:")
(displayln (contiene-negativo? '(5 9 -3 2)))

;; Ejercicio 7 – Calcular la suma total de una lista
;; Usar foldl de manera sencilla
(define (suma-acumulada lst)
  (foldl + 0 lst))

;; Ejemplo:
;; (suma-acumulada '(1 2 3 4)) => 10
(displayln "Resultado Ejercicio 7:")
(displayln (suma-acumulada '(1 2 3 4)))

;; Ejercicio 8 – Concatenar cadenas de texto en una lista
;; Usar foldr con string-append.
(define (concatenar-cadenas lst)
  (foldr string-append "" lst))

;; Ejemplo:
;; (concatenar-cadenas '("Hola" " " "Mundo")) => "Hola Mundo"
(displayln "Resultado Ejercicio 8:")
(displayln (concatenar-cadenas '("Hola" " " "Mundo")))

;; Ejercicio 9 – Generar lista con el doble de los números mayores que 5
;; Combinar map y filter.
(define (dobles-mayores-que-cinco lst)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lst)))

;; Ejemplo:
;; (dobles-mayores-que-cinco '(3 6 8 2 10)) => '(12 16 20)
(displayln "Resultado Ejercicio 9:")
(displayln (dobles-mayores-que-cinco '(3 6 8 2 10)))

;; Ejercicio 10 – Invertir el orden de una lista
;; Usar foldl.
(define (invertir-lista lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

;; Ejemplo:
;; (invertir-lista '(1 2 3 4)) => '(4 3 2 1)
(displayln "Resultado Ejercicio 10:")
(displayln (invertir-lista '(1 2 3 4)))

;; Ejercicio 11 – Crear una función que reciba una función como parámetro
;; Aplicar función cuadrado a lista.
(define (aplicar-funcion f lst)
  (map f lst))

;; Ejemplo:
;; (define (cuadrado x) (* x x))
;; (aplicar-funcion cuadrado '(1 2 3 4)) => '(1 4 9 16)
(displayln "Resultado Ejercicio 11:")
(displayln (aplicar-funcion (lambda (x) (* x x)) '(1 2 3 4)))

;; Ejercicio 12 – Reto integrador
;; Calcular el promedio de los números mayores a 5.
(define (promedio-mayores-que-cinco lst)
  (let* ([mayores (filter (lambda (x) (> x 5)) lst)]
         [suma (foldl + 0 mayores)]
         [n (length mayores)])
    (if (= n 0)
        0
        (/ (exact->inexact suma) (exact->inexact n)))))

;; Ejemplo:
;; (promedio-mayores-que-cinco '(3 8 10 4 9 2 7)) => 8.5
(displayln "Resultado Ejercicio 12:")
(displayln (promedio-mayores-que-cinco '(3 8 10 4 9 2 7)))