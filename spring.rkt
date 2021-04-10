#lang scheme/base
(require scheme/mpair)
(provide (all-defined-out))
(define input-file "little_data.txt")
(define (split-line line)
  (foldl (lambda (x y) (let ((sentence (filter (lambda (z) (not (equal? z ""))) (regexp-split #px"\\s+" x)))) ; разбиение предложения на слова
                         (if (null? sentence) y (cons sentence y)))) 
         null
         (regexp-split #px"[\\.\\?!]" ; разбиение текста на предложения
                       (regexp-replace* #px"([;,:'])" ; выделяем знаки препинания
                                                         (string-downcase line)          
                                        " \\1 ") ;иными словами - отделяем знаки препинания пробелами, чтоб как отдельные слова были.
                       ))
  )

; построчно читаем предложения из файла
; каждую разбиванием на слова и добавляем в гpаф
(define (read-loop input-file)
  (define input-stream (open-input-file input-file))
  (let loop ((line (read-line input-stream)) (i 0))
    (if(not (eof-object? line))
        (begin 
          (let ((sentences (split-line line)))
            (println i)
            (if (null? sentences)(loop (read-line input-stream) (+ 1 i))
                (begin
                 (fill-graph (map (lambda (x) (append (list ".")(append x (list ".")))) sentences) forward-graph 0)
                 (fill-graph  (map (lambda (x) (append (list ".")(append x (list ".")))) sentences) reverse-graph 1)
          (loop (read-line input-stream)(+ 1 i) )
           )
         
                ) 
    )
  (close-input-port input-stream)
  )
        null
        )
    )
   (mfor-each (lambda (x)  (set-mcar! x (reverse (mcar x)))) (mcdr reverse-graph))

 )

(define n 4)

(define forward-graph (mlist (mcons 0 0)))

  

(define reverse-graph (mlist (mcons 0 0)))

(define (fill-graph sentences  graph fl)
  
   (define (inner-loop sentence pos cur-gram)
     (
      
      let* ((cur-words ( mcdr(massoc   cur-gram graph)))
           (next-word ( list-ref sentence pos))
           (next-gram (append ( cdr cur-gram)  (list next-word))))
       ;если следующей н-граммы не нашлось в графе-добавляем ее
        (if (not (massoc  next-gram graph))
                              (mappend! graph (mlist (mcons  next-gram (mlist (mcons 0 0)))))
                              '()
                            )
       ;если следующее слово нашлось в списке слов текущей н-граммы-увеличиваем счетчик, иначе-добавляем
        (if (massoc next-word cur-words)
            (
             set-mcdr! (massoc next-word cur-words) (+ 1 (mcdr(massoc next-word cur-words)))
             )
            (
             mappend! cur-words (mlist (mcons next-word 1))
            )
        )
       ;если мы не в конце предложения - продолжаем идти
        (if (not (= (+ 1 pos) (length sentence)))
            (
             inner-loop sentence (+ 1 pos) next-gram 
             ) null
         )
      
       )
    )
  ;конструируем первую н-грамму предложения
   (define (get-first-gram sentence)
     (
      let inner-loop ((sentence sentence) (pos 1) (res (list (car sentence))))
       (
        if (= pos n) 
           
            (reverse res)
           (
            inner-loop sentence (+ 1 pos) (cons (list-ref sentence pos) res )
           )
       )
     )
   )
    (for-each (lambda (x) (let* ((cur-sent (if (= fl 1) (reverse x) x))
                                (first-gram (get-first-gram cur-sent))) (begin
                            (if (not (massoc  first-gram graph))
                              (mappend! graph (mlist (mcons  first-gram (mlist (mcons 0 0)))))
                              null
                            )
                            (inner-loop cur-sent n  first-gram)))) (filter (lambda(x) (> (length x) (+ 2 n)))sentences))
  
   
)
  
          