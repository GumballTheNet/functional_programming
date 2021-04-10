#lang scheme/base

; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента

(require racket/string)
(require racket/format)
(require racket/require)
(require racket/include)
(require racket/set)
(require scheme/mpair)
(require "spring.rkt")

(read-loop input-file)
(define n-gram-lst (foldl (lambda (x y) (if (not (or (eq? (list-ref (mcar x) 0) ".") (eq? (list-ref (mcar x) (- (length (mcar x)) 1)) "."))) (cons (mcar x) y) y)) '() (mlist->list(mcdr forward-graph))))
(define rev-n-gram-lst (foldl (lambda (x y) (if (not (eq? (list-ref (mcar x) 0) ".")) (cons (mcar x) y) y)) '() (mlist->list(mcdr reverse-graph))))
(define forward-n-gram-lst (foldl (lambda (x y) (if  (eq? (list-ref (mcar x) 0) ".") (cons (mcar x) y) y)) '() (mlist->list(mcdr  forward-graph))))

(define (ask-patient-name)
  (begin
    (printf "next!\n")
    (printf "who are you?\n")
    (print '**)
    (read-line)
    ) 
  )

(define (split-answer str)
  (foldl (lambda (x y) (let ((sentence (filter (lambda (z) (not (equal? z ""))) (regexp-split #px"\\s+" x)))) ; разбиение предложения на слова
                         (if (null? sentence) y (cons sentence y)))) 
         null
         (regexp-split #px"[\\.\\?!]" ; разбиение текста на предложение
                       (regexp-replace* #px"([\\)\\(;,:'])" ; выделение знаков препинания
                                        str " \\1 ")))
  )
; склеиваение ответа из списка слов
(define (merge-answer lst)
  (regexp-replace* #px"([\\)']) " (regexp-replace* #px" ([\\);,:'\\.])" (string-join lst) "\\1") "\\1")
  )

(define (visit-doctor stop-word limit)
  (let loop ((i 0) (strategies strategies) (keywords get-keywords)) (
    if (= i limit) (println '(time to go home))( 
    let ((name(ask-patient-name))) (
      if (equal? name (~a stop-word))
          (println '(time to go home))
          (begin
            (printf "Hello, ~a!\n" name)
            (printf "what seems to be the trouble?")
            (doctor-driver-loop name)
            (loop (+ i 1) strategies keywords)
         )
      )
    )
   )
 )
)

(define (doctor-driver-loop name)
  ; answers - список всех ответов пользователя, keywords - список ключевых слов в templates, strategies - структура данных со сведениями обо всех стратегиях «Доктора»
  (let loop ((name name) (answers null) (keywords get-keywords) (strategies strategies))
    (newline)
    (print "**") ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read-line)))
      (cond 
        ((equal? user-response "goodbye") ; реплика "goodbye" служит для завершения работы с данным пациентом
         (printf "Goodbye, ~a!\n" name)
         (printf "see you next week\n"))
         
        (else (let ((answer (split-answer user-response)))
                (printf (merge-answer (reply  answer answers keywords strategies))) 
                (loop name (cons answers answer) keywords strategies)) ; каждое предложение сохраняем в историю ответов
              )
        )
      )
    )
  )
;объединяем слова из разных реплик пользователя в один список
(define (merge lst)
  (
   foldl (lambda (x y) (append x y)) '() lst))

(define strategies
  (list
   (list (lambda (user-response history keywords) #t) 1 (lambda (user-response history keywords) (qualifier-answer (car user-response))))
   (list (lambda (user-response history keywords) #t) 2 (lambda (user-response history keywords) (hedge)))
   (list (lambda (user-response history keywords) (not (null? history))) 2 (lambda (user-response history keywords) (history-answer history)))
   (list (lambda (user-response history keywords) (pred user-response keywords)) 3 (lambda (user-response history keywords) (keyword-answer (merge user-response) keywords)))
   (list (lambda (user-response history keywords) #t) 6 (lambda (user-response history keywords) (begin  (spring-answer (change-person (merge user-response)) n-gram-lst rev-n-gram-lst))))
   )
)

(define (pick-first-gram user-response filtered-lst)
  (
   let (;список весов - это список, элементы котрого-суммарное количество общих букв в одинаковых словах из реплики пользователя и н-граммы
         (weight-lst (cdr (foldl (lambda (x y) (let ((inter-set (set-intersect user-response x ))) (
                                              ;если одинаковых слов нет, то ставим 0
                                             if (null? inter-set) (if (=(car y) 0) ( append y (list x)) y)
                                             (
                                              let ((cur-length (foldl (lambda(x y) (+ (string-length x) y)) 0 inter-set)))
                                               (;если текущая длина больше рекорда - обновляем список
                                                if (> cur-length (car y)) (list cur-length x)
                                                 (;если равна-добавляем в конец новую н-грамму, иначе ничего не делаем
                                                  if (= cur-length (car y)) (append y (list x)) y)
                                                 )
                                              )
                                             )
                                               )) '(0) filtered-lst)))
                   )
         (begin
             ;выбираем случайную н-грамму из списка н-грамм с максимальными пересечениями  
         ( list-ref weight-lst (random (length weight-lst)))
         )
   
       )
      )
  
  
;выбираем геометрическим методом из списка слов для данной н-граммы случвйное
(define (pick-random-word current-gram graph)
  (
   let ((cur-words ( mcdr(massoc current-gram graph))))
    (
     let loop((random_point (random(foldl (lambda(x y)(+ (mcdr x) y)) 0 (mlist->list cur-words)))) (cur-words cur-words))
      (
        let ((current_weight(mcdr (mcar cur-words))))
        (if (> current_weight random_point) (mcar (mcar cur-words))
           (loop (- random_point current_weight) (mcdr cur-words))
        )
      )
    )
  )
)


(define (spring-forward-answer user-responce   forward-n-gram-lst)
  (
   let ((current-gram (pick-first-gram user-responce  forward-n-gram-lst)))
    (
   let inner-loop ((current-answer current-gram) (current-gram current-gram) (j 0))
    (
     let ((current-word (pick-random-word current-gram forward-graph)))
      (
       
       if (eq? current-word ".")
          (;проверяем, имеет ли смысл продолжать ответ
           if ( > (+ j (random 21)) 0) (
              cdr (append current-answer (list current-word))
           )
              (
               let ((new-gramm  (pick-first-gram current-gram 
                                forward-n-gram-lst)))
                (
               inner-loop (append current-answer new-gramm) new-gramm (+ 1 j))
              )
          )
          (begin
           (inner-loop (append current-answer (list current-word)) (append (cdr current-gram) (list current-word)) j)
          )
      )
      )
     )
   )
 )

(define (spring-answer user-responce n-gram-lst rev-n-gram-lst )
  (;выбираем первую н-грамму
   let* ((current-gram (pick-first-gram user-responce    n-gram-lst))
        (first-gram current-gram))
    (
   let inner-loop ((current-answer current-gram) (current-gram current-gram) (j 0))
    (
     let ((current-word (pick-random-word current-gram forward-graph)))
      (
       
       if (eq? current-word ".")
          (;проверяяем, имеет ли смысл идти дальше
           if ( > (+ j (random 21)) 0) (
              spring-backward-answer user-responce  (append current-answer (list current-word)) first-gram  rev-n-gram-lst
           )
              (;строим следующее предложение
               let ((new-gramm  (pick-first-gram current-gram 
                                 n-gram-lst)))(
               inner-loop (append current-answer new-gramm) new-gramm (+ 1 j))
              )
          )
          (begin;добавляем слово в текущее и идем дальше
           (inner-loop (append current-answer (list current-word)) (append (cdr current-gram) (list current-word)) j)
          )
      )
      )
     )
   )
 )
(define (spring-backward-answer user-responce current-answer current-gram  rev-n-gram-lst)
  (
   
   let inner-loop ((current-answer current-answer) (current-gram current-gram) (j 0))
    (
     let ((current-word (pick-random-word current-gram reverse-graph)))
      (
       ;если дошли до точки в начале предложения, то или генерируем предыдущее предложение, или останавливаемся и выдаем ответ
       if (eq? current-word ".")
          (
           if ( > (+ j (random 20)) 0) (
              cdr (append (list current-word)  current-answer )
           )
              (
               let ((new-gramm  (pick-first-gram current-gram 
                                 rev-n-gram-lst)))(
               inner-loop (append new-gramm current-answer) new-gramm (+ 1 j))
              )
          );продолжаем генерировать предложение в обратную сторону
          (begin
           (inner-loop (append (list current-word) current-answer ) (append  (list current-word) (reverse(cdr (reverse current-gram)))) j)
          )
      )
      )
     )
   )
 
(define (pick-random-with-weight lst) (
      let loop((random_point (random(foldl (lambda(x y)(+ (cadr x) y)) 0 lst))) (lst lst))
      (
        let ((current_weight(cadr (car lst))))
        (if (> current_weight random_point) (car lst)
           (loop (- random_point current_weight) (cdr lst))
        )
      )
   )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response history keywords strategies) ;добавить что на первой итерации стратегия 2 неприменима
  (
      let* ((possible_strats (filter (lambda(x)((list-ref x 0)  user-response history keywords)) strategies))
            (current_strategy(pick-random-with-weight possible_strats))) (
          (list-ref current_strategy 2)  user-response history keywords) 
  )   
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random '(("you seem to think that")
                               ("you feel that")
                               ("why do you believe that")
                               ("why do you say that")
                               ("are you really sure that")
                               ("it looks like you claim that")
                               ("if i am not mistaken, you said that")
                               )
                )
                (change-person user-response)
        )
 )

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace1 '(("am" "are")
                        ("are" "am")
                        ("i" "you")
                        ("me" "you")
                        ("mine yours")
                        ("my" "your")
						("myself" "yourself")
                        ("you" "i")
                        ("your" "my")
                        ("yours" "mine")
						("yourself" "myself"))
                      phrase)
 )
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
  )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace1 replacement-pairs lst)
        (cond ((null? lst) lst)
              (else 
               (let loop ((lst lst) (result '()))
                 (if (null? lst) (reverse result)
                     (let ((pat-rep (assoc (car lst) replacement-pairs)))
                       (loop (cdr lst) (if pat-rep (cons (cadr pat-rep) result) (cons (car lst) result)))
                     ) 
                 )         
               )
              )
       )
 )

(define (many-replace2 replacement-pairs lst)
  (map (lambda (x) (
        let ((pat-rep (assoc x replacement-pairs)))
         (if pat-rep (cadr pat-rep) x))) lst
  )
)

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
  (pick-random '(("please go on")
                 ("many people have the same sorts of feelings")
                 ("many of my patients have told me the same thing")
                 ("please continue")
                 ("i completely understand you")
                 ("would you like to tell me more about it?")
                 ("it is interesting and i should think more about it")
                 )
               )
  )

(define (history-answer history)
  (cons "earlier you said that"
        (change-person (pick-random history)) ; выбираем произвользую фразу из сохраненных и производим в ней замену лица
        )
  )



(define template '( 
  ( ; начало данных 1й группы
    ("depressed" "suicide" "exams" "university") ; список ключевых слов 1й группы
    ( ; список шаблонов для составления ответных реплик 1й группы 
	  ("when you feel depressed, go out for ice cream")
      ("depression is a disease that can be treated")
      ("exams are just a step of your life, don't worry!")
      ("if you commit a suicide, you will get into the hell, i'm afraid")
	)
  ) ; завершение данных 1й группы
  ( ; начало данных 2й группы ...
    ("mother" "father" "parents" "brother" "sister" "uncle" "ant" "grandma" "grandpa")
    (
	  ("tell me more about your" "*" ", i want to know everything about your" "*")
      ("why do you feel that way about your" "*" "?")
      ("i think your" "*" "is a nice percon, do you agree?")
      ("do you love your" "*" "?")
	)
  )
  (
    ("university" "scheme" "lections")
	(
	  ("your education is important")
	  ("how many time do you spend to learnmany?")
          ("do you enjoy" "*" "?")
          ("it would be better, if you attend your" "*")
	)
  )
  (
  ("girlfriend" "crush" "best-friend" "friend")
  (
   ("your" "*" "is an important part of your life, do you agree?")
   ("do you have any problems with your" "*" "?")
   ("friendship is magic, isn't it?")
   ("i'm sure your" "*" "adores you!")
 ))
  (
 ("games" "computer" "drugs" "wine")
 (
  ("being depended on" "*" "is horrible!")
  ("how long have you been using" "*" "?")
  ("you should less using" "*")
  ("i know how it is difficult to stop using" "*"))
))
)


(define get-keywords
  (foldl (lambda (x y) (append (filter (lambda (z) (not (member z y))) x) y))
         null
         (map car template)) 
)

(define (pred lst keywords) (
        ormap (lambda (y)
           (ormap (lambda (x) (equal? x y))
                  keywords)) lst)
)

(define (keyword-answer user-response keywords) (
   let* ((filtered_lst (filter (lambda(x)(member x keywords)) user-response))
       (current_keyword(pick-random filtered_lst))
       (variants (foldl (lambda(x y)(append y (cadr x))) null (filter (lambda(x)(member current_keyword (car x))) template))))
   (many-replace (list(list "*" current_keyword)) (pick-random variants))  
  )
)
   
             
         
> (visit-doctor 'ivan 2)
