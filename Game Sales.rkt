#lang racket

; Opening to user interface
(define (print-preface)
  (displayln "=================================")
  (displayln "   VIDEO GAME SALES FOR CS353")
  (displayln "Type 'exit' at any time to leave.")
  (displayln "=================================\n"))

; Provides the five search options
(define (print-options)
  (displayln "Search video games by what options? Please choose up to 3. You may press q at any time to instantly search.")
  (displayln "Name")
  (displayln "Date")
  (displayln "Publisher")
  (displayln "Genre")
  (displayln "Region"))

; Reads input lines
(define read-line-input (lambda () (read-line (current-input-port) 'any)))

; These functions help format data using cadr formatting for all of the portions of the csv file
(define (get-index formatted-data-line) (car formatted-data-line))
(define (get-rank formatted-data-line) (cadr formatted-data-line))
(define (get-title formatted-data-line) (caddr formatted-data-line))
(define (get-platform formatted-data-line) (cadddr formatted-data-line))
(define (get-date formatted-data-line) (car (cddddr formatted-data-line)))

(define (get-genre formatted-data-line)
  (cadr (cddddr formatted-data-line)))

(define (get-publisher formatted-data-line)
  (caddr (cddddr formatted-data-line)))

(define (get-north-america formatted-data-line)
  (cadddr (cddddr formatted-data-line)))

(define (get-europe formatted-data-line)
  (car (cddddr (cddddr formatted-data-line))))

(define (get-japan formatted-data-line)
  (cadr (cddddr (cddddr formatted-data-line))))

(define (get-rotw formatted-data-line)
  (caddr (cddddr (cddddr formatted-data-line))))

(define (get-global formatted-data-line)
  (cadddr (cddddr (cddddr formatted-data-line))))

(define (get-review formatted-data-line)
  (if (= (length formatted-data-line) 13)
      (car (cddddr (cddddr (cddddr formatted-data-line))))
      (car (cddddr (cddddr formatted-data-line)))))

(define (file-data->formatted-data-set file-data)
  (define (file-line->formatted-line line)
    (define (fix-split-line pre-split-line)
      (append (take pre-split-line 2) (list (string-append (third pre-split-line) "," (fourth pre-split-line))) (drop pre-split-line 4)))  

    (define pre-split-line (string-split line ","))
    (define split-line (if (= (length pre-split-line) 14)
                           (fix-split-line pre-split-line)
                           pre-split-line))
    (define (remove-quotes entry) (if (non-empty-string? entry)
                                      (substring entry 1 (sub1 (string-length entry)))
                                      entry))
    (list
     (string->number (first split-line))
     (string->number (second split-line))
     (remove-quotes (third split-line))
     (string->symbol (remove-quotes (fourth split-line)))
     (string->number (fifth split-line))
     (string->symbol (remove-quotes (sixth split-line)))
     (remove-quotes (seventh split-line))
     (string->number (eighth split-line))
     (string->number (ninth split-line))
     (string->number (tenth split-line))
     (string->number (list-ref split-line 10))
     (string->number (list-ref split-line 11))
     (string->number (list-ref split-line 12))))
  
  (map file-line->formatted-line file-data))

; These getter functions retrieve information from the user and store them for later usage
(define (get-name-input)
  (displayln "Name: ")
  (define name-input (read-line-input))
  (list 'NAME name-input))

(define (get-date-input)
  (displayln "Start Year: ")
  (define start-date (read-line-input))
  (displayln "End Year: ")
  (define end-date (read-line-input))
  (cond
    [(and (andmap char-numeric? (string->list start-date)) (andmap char-numeric? (string->list end-date)))
     (if (< (string->number start-date) (string->number end-date))
         (list 'DATE (string->number start-date) (string->number end-date))
         (list 'DATE (string->number end-date) (string->number start-date)))]
    [else
     (displayln "Invalid input.")
     (get-date-input)]))

(define (get-publisher-input)
  (displayln "Publisher: ")
  (define publisher-input (read-line-input))
  (list 'PUBLISHER publisher-input))

(define (get-region-input)
  (displayln "Region (North America, Europe, Japan, Other, Global): ")
  (define region-input (read-line-input))
  (cond
    [(string-ci=? region-input "North America")     (list 'REGION 'NORTH-AMERICA)]
    [(string-ci=? region-input "Europe")            (list 'REGION 'EUROPE)]
    [(string-ci=? region-input "Japan")             (list 'REGION 'JAPAN)]
    [(string-ci=? region-input "Other") (list 'REGION 'ROTW)]
    [(string-ci=? region-input "Global")            (list 'REGION 'GLOBAL)]
    [else
     (displayln "Invalid input.")
     (get-region-input)]))

(define (get-genre-input)
  (displayln "Genre: ")
  (define genre-input (read-line-input))
  (list 'GENRE (string->symbol genre-input)))

; Main logic
(define (run-menu)
  (define formatted-data-set (file-data->formatted-data-set (rest (file->lines "Video Games Sales.csv"))))
  (print-preface)
  
  (define (get-user-selections [selections '()])

    (define (contains-selection? choice selections)
      (define choice-symbol (cond
                              [(string-ci=? choice "Name") 'NAME]
                              [(string-ci=? choice "Date") 'DATE]
                              [(string-ci=? choice "Publisher") 'PUBLISHER]
                              [(string-ci=? choice "Region") 'REGION]
                              [(string-ci=? choice "Genre") 'GENRE]
                              [else 'ERROR]))
        (> (length (filter (λ (element) (if (symbol? element)
                                            (symbol=? element choice-symbol)
                                            #f))
                                            (flatten selections))) 0))
    
    (cond
      [(equal? (length selections) 3) selections]
      [else
       (print-options)
       (define user-criteria-choice (read-line-input))
       (cond
         [(contains-selection? user-criteria-choice selections)
          (displayln "Input already provided for this search")
          (get-user-selections selections)]
         [(string-ci=? user-criteria-choice "q") selections]
         [(string-ci=? user-criteria-choice "Name")      (get-user-selections (cons (get-name-input) selections))]
         [(string-ci=? user-criteria-choice "Date")      (get-user-selections (cons (get-date-input) selections))]
         [(string-ci=? user-criteria-choice "Publisher") (get-user-selections (cons (get-publisher-input) selections))]
         [(string-ci=? user-criteria-choice "Region")    (get-user-selections (cons (get-region-input) selections))]
         [(string-ci=? user-criteria-choice "Genre")     (get-user-selections (cons (get-genre-input) selections))]
         [(string-ci=? user-criteria-choice "exit")      (list 'QUIT)]
         [else
          (displayln "Invalid input.")
          (get-user-selections selections)])]))

  (define selections (sort (get-user-selections) symbol<? #:key first))
  
  (define (process-selection selection data-set)
    (define (process-name name-selection data-set)
      (filter (λ (formatted-line) (string-ci=? (get-title formatted-line) (first name-selection))) data-set))

    (define (process-date date-selection data-set)
      (filter (λ (formatted-line) (and (>= (get-date formatted-line) (first date-selection)) (< (get-date formatted-line) (second date-selection)))) data-set))
    
    (define (process-publisher publisher-selection data-set)
      (filter (λ (formatted-line) (if (equal? (memf (λ (arg) (if (string? arg)
                                                                 (string-ci=? arg (first publisher-selection))
                                                                 (equal? arg (first publisher-selection)))) formatted-line) #f)
                                      #f
                                      #t)) data-set))
                                                                              
    
    (define (process-region region-selection data-set)
      (cond
        [(equal? (first region-selection) 'NORTH-AMERICA)
         (map (λ (fl) (list (get-index fl) (get-rank fl) (get-title fl) (get-platform fl) (get-date fl) (get-genre fl) (get-publisher fl) (get-north-america fl) (get-review fl))) data-set)]
        [(equal? (first region-selection) 'EUROPE)
         (map (λ (fl) (list (get-index fl) (get-rank fl) (get-title fl) (get-platform fl) (get-date fl) (get-genre fl) (get-publisher fl) (get-europe fl) (get-review fl))) data-set)]
        [(equal? (first region-selection) 'JAPAN)
         (map (λ (fl) (list (get-index fl) (get-rank fl) (get-title fl) (get-platform fl) (get-date fl) (get-genre fl) (get-publisher fl) (get-japan fl) (get-review fl))) data-set)]
        [(equal? (first region-selection) 'ROTW)
         (map (λ (fl) (list (get-index fl) (get-rank fl) (get-title fl) (get-platform fl) (get-date fl) (get-genre fl) (get-publisher fl) (get-rotw fl) (get-review fl))) data-set)]
        [else data-set]))    
    
    (define (process-genre genre-selection data-set)
      (filter (λ (formatted-line) (equal? (first genre-selection) (get-genre formatted-line))) data-set))

    (cond
      [(equal? 'NAME (first selection))      (process-name (rest selection) data-set)]
      [(equal? 'DATE (first selection))      (process-date (rest selection) data-set)]
      [(equal? 'PUBLISHER (first selection)) (process-publisher (rest selection) data-set)]
      [(equal? 'REGION (first selection))  (process-region (rest selection) data-set)]
      [(equal? 'GENRE (first selection))     (process-genre (rest selection) data-set)]))

; Final user input before calculation
(define (print-sort-prompt)
  (displayln "Organize by Rank or Review?"))

  (define (process-selections selections data-set)
    (if (empty? selections)
        data-set
        (process-selections (rest selections) (process-selection (first selections) data-set))))

  (cond
    [(equal? selections (list 'QUIT))
     (displayln "Exiting, Goodbye")
     'QUIT]
    [else
     (print-sort-prompt)
     (define (get-sort-function)
       (define user-sort-input (read-line-input))
       (cond
         [(string-ci=? user-sort-input "Rank")  (λ (data-set) (sort data-set < #:key get-rank))]
         [(string-ci=? user-sort-input "Review") (λ (data-set) (sort data-set < #:key get-review))]
         [else
          (displayln "Invalid input.")
          (get-sort-function)]))
     (define user-sort (get-sort-function))
     (displayln "Index  Rank  Title  Platform  Year  Genre  Publisher  North America  Europe  Japan  Other  Global  Review")
     (for-each displayln (user-sort (process-selections selections formatted-data-set)))
     (displayln "")]))

; Continues running code until it is told otherwise
(define (while-not-q)
  (when (not (equal? (run-menu) 'QUIT))
    (while-not-q)))

(while-not-q)
