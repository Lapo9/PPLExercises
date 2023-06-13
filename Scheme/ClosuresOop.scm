#lang racket

(provide closuresOopTest)

(define (Person personName personSurname personAge)
  (let ([name (string-append personName " " personSurname)] [age personAge]) ;fields

    ;methods
    (define (set-person-name! newName newSurname) (set! name (string-append newName " " newSurname)))
    (define (get-person-name) name)
    (define (person-get-older!) (set! age (+ age 1)))
    (define (get-person-age) age)
    (define (display-person) (display "name: ") (display name) (newline) (display "age: ") (display age) (newline))

    ;what you actually get by instantiating an object is a "dispatcher"
    (lambda (method . args)
      (apply (case method
        [(set-person-name!)   set-person-name!]
        [(get-person-name)    get-person-name]
        [(person-get-older!)  person-get-older!]
        [(get-person-age)     get-person-age]
        [(display-person)     display-person]
        [else (error "Method not found")]
      ) args)
    )
  )
)

;inheritance
(define (Developer devName devSurname devAge devSalary)
  (let ([parent (Person devName devSurname devAge)] ;parent is the parent class object
        [salary devSalary]) ;new fields

    ;new methods
    (define (set-dev-salary! newSalary) (set! salary newSalary))
    (define (get-dev-salary) salary)
    (define (write-code requirements) (display (string-append "std::cout << '" requirements "';")))
    (define (display-dev) (display "DEVELOPER") (newline) (parent 'display-person) (display "salary: ") (display salary) (newline)) ;this is a sort of override

    ;what you actually get by instantiating an object is a "dispatcher"
    (lambda (method . args)
      (case method
        [(set-dev-salary!) (apply set-dev-salary! args)]
        [(get-dev-salary) (apply get-dev-salary args)]
        [(write-code) (apply write-code args)]
        [(display-dev) (apply display-dev args)]
        [else (apply parent (cons method args))] ;if you cannot recognize the method, try to call it on the parent
      )
    )
  )
)

;usage example
(define (closuresOopTest)
  (define myPerson1 (Person "Bruce" "Wayne" 29))
  (myPerson1 'person-get-older!)
  (myPerson1 'set-person-name! "Bat" "Man")
  (myPerson1 'display-person) ;will print "name: Bat Man age: 30"

  (newline)
  
  (define myDeveloper1 (Developer "Lapo" "Falcone" 23 9500))
  (myDeveloper1 'display-person) ;will print "name: Lapo Falcone age: 23"
  (myDeveloper1 'person-get-older!)
  (myDeveloper1 'display-dev) ;will print "DEVELOPER name: Lapo Falcone age: 24 salary: 9500"
  (myDeveloper1 'write-code "Bullshit") ;will print "std::cout << 'Bullshit';"
)