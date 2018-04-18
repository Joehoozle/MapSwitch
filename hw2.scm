(define adjacency-map '(
    (Alabama Mississippi Tennessee Georgia Florida)
    (Alaska)
    (Arkansas Texas Oklahoma Missouri Tennessee Mississippi Louisiana)
    (Arizona California Nevada Utah New-Mexico)
    (California Arizona Nevada Oregon)
    (Colorado New-Mexico Utah Wyoming Nebraska Kansas Oklahoma)
    (Connecticut New-York Massachusetts Rhode-Island)
    (Delaware Maryland Pennsylvania New-Jersey)
    (Florida Alabama Georgia)
    (Georgia Florida Alabama Tennessee North-Carolina South-Carolina)
    (Hawaii)
    (Idaho Oregon Washington Montana Wyoming Utah Nevada)
    (Indiana Illinois Michigan Ohio Kentucky)
    (Illinois Missouri Iowa Wisconsin Indiana Kentucky)
    (Iowa Missouri Illinois Wisconsin Minnesota South-Dakota Nebraska)
    (Kansas Colorado Nebraska Missouri Oklahoma)
    (Kentucky Missouri Illinois Indiana Ohio West-Virginia Virginia Tennessee)
    (Louisiana Texas Arkansas Mississippi)
    (Maine New-Hampshire)
    (Maryland Virginia West-Virginia Pennsylvania Delaware)
    (Massachusetts Rhode-Island Connecticut New-York Vermont New-Hampshire)
    (Michigan Wisconsin Indiana Ohio)
    (Minnesota North-Dakota South-Dakota Iowa Wisconsin)
    (Mississippi Louisiana Arkansas Tennessee Alabama)
    (Missouri Oklahoma Kansas Nebraska Iowa Illinois Kentucky Tennessee Arkansas)
    (Montana Idaho Wyoming South-Dakota North-Dakota)
    (Nebraska Colorado Kansas Missouri Iowa South-Dakota Wyoming)
    (Nevada California Arizona Utah Idaho Oregon)
    (New-Hampshire Maine Vermont Massachusetts)
    (New-Jersey Delaware Pennsylvania New-York)
    (New-Mexico Texas Oklahoma Colorado Arizona)
    (New-York Pennsylvania New-Jersey Connecticut Massachusetts Vermont)
    (North-Carolina South-Carolina Georgia Tennessee Virginia)
    (North-Dakota Montana South-Dakota Minnesota)
    (Ohio Michigan Indiana Kentucky West-Virginia Pennsylvania)
    (Oklahoma Texas New-Mexico Colorado Kansas Missouri Arkansas)
    (Oregon Washington Idaho Nevada California)
    (Pennsylvania Ohio West-Virginia Maryland Delaware New-Jersey New-York)
    (Rhode-Island Connecticut Massachusetts)
    (South-Carolina Georgia North-Carolina)
    (South-Dakota Nebraska Iowa Minnesota North-Dakota Montana Wyoming)
    (Tennessee Arkansas Missouri Kentucky Virginia North-Carolina Georgia Alabama Mississippi)
    (Texas New-Mexico Oklahoma Arkansas Louisiana)
    (Utah Nevada Idaho Wyoming Colorado Arizona)
    (Vermont New-York Massachusetts New-Hampshire)
    (Virginia North-Carolina Tennessee Kentucky West-Virginia Maryland)
    (Washington Oregon Idaho)
    (West-Virginia Virginia Kentucky Ohio Pennsylvania Maryland)
    (Wisconsin Minnesota Iowa Illinois Michigan)
    (Wyoming Idaho Montana South-Dakota Nebraska Colorado Utah)
  ))

(define (list-size-helper alist num)
  (cond
      ((null? alist) num)
      (#t (list-size-helper (cdr alist) (+ num 1)))
  )
)

(define (list-size alist) (list-size-helper alist 0))




(define (nth-item num alist) 
    (cond 
        ((equal? num 1) (car alist))
        (#t (nth-item (- num 1) (cdr alist)))
    ) 
)

(define (replace-nth-item index alist num)
        (cond 
            ((equal? index 1) (cons num (cdr alist)))
            (#t (cons (car alist) (replace-nth-item (- index 1) (cdr alist) num)))
        ) 
)

(define (swap-elements a b alist) 
    (let ((temp (nth-item a alist)))
        (replace-nth-item b (replace-nth-item a alist (nth-item b alist)) temp)
    )
)




(define (exists? a alist)
    (cond
        ((null? alist) #f)
        ((equal? a (car alist)) #t)
        (#t (exists? a (cdr alist)))
    )
)




(define (find-adjacentcy a b alist) 
    (cond
        ((null? alist) #f)
        ((equal? (car (car alist)) a) (exists? b (cdr (car alist))))
        ((equal? (car (car alist)) b) (exists? a (cdr (car alist))))
        (#t (find-adjacentcy a b (cdr alist)))
    )
)

(define (is-adjacent? a b) (find-adjacentcy a b adjacency-map))




(define (contains? alist a) 
    (cond
        ((null? alist) #f)
        ((equal? (car alist) a) #t)
        (#t (contains? (cdr alist) a))
    )
)

(define (get-children-helper alist i j num)
        (cond
            ((and (equal? i (- num 1)) (equal? j num)) (list (list (swap-elements i j alist) (list i j))))
            ((equal? j num) (cons (list (swap-elements i j alist) (list i j)) (get-children-helper alist (+ 1 i) (+ 2 i) num)))
            (#t (cons (list (swap-elements i j alist) (list i j)) (get-children-helper alist i (+ 1 j) num))) 
        )
)

(define (get-children alist) (get-children-helper alist 1 2 (list-size alist)))

(define (get-children-level level)
    (cond
        ((null? (cdr level)) (get-children (car (car level))))
        (#t (cons (get-children (car (car level))) (get-children-level (cdr level))))
    )
)


(define (is-goal-state? alist)
    (cond
        ((null? (cdr alist)) #t)
        ((is-adjacent? (car alist) (car (cdr alist))) (is-goal-state? (cdr alist)))
        (#t #f)
    )
)

(define (unvisited alist visited)
  
    (cond
        ((and (null? (cdr alist)) (not (contains? visited (car (car alist))))) alist)
        ((null? (cdr alist)) '())
        ((not (contains? visited (car (car alist)))) (cons (car alist) (unvisited (cdr alist) visited)))
        (#t (unvisited (cdr alist) visited))    
    )
)

(define (get-pure-states alist)
    (cond 
        ((null? (cdr alist)) (car (car alist)))
        (#t (cons (car (car alist)) (get-pure-states (cdr alist))))        
    )
)

(define (dfs-helper frontier levels complete-path visited current-depth deepness)    
    (display frontier)
    (newline)
    (cond
        ;if frontier is ever null, we need to pick a new depth
        ((null? frontier) #f)

        ;If head of frontier is goal state, return a list with it and the path we took to get there
        ((is-goal-state? (car (car frontier))) (append (car (car frontier)) (cons (cdr (car frontier)) complete-path)))

        ;Expand leftmost child element if it has not been visited yet as well as add it to visited list 
        ((and (not (equal? current-depth deepness)) (not (contains? visited (car (car frontier))))) (dfs-helper (get-children (car (car frontier))) (cons (cdr frontier) levels) (cons (cdr (car frontier)) complete-path) (append visited (car (car frontier))) (+ 1 current-depth) deepness))
        
        ;If we are not at designated depth yet and the leftmost element has already been visited, move to the next element to the right on the same level
        ((not (equal? current-depth deepness)) (dfs-helper (cdr frontier) levels complete-path visited current-depth deepness)) 
    
        ;head is not a goal state, but current level is empty so we need to go back up level
        ((null? (cdr frontier)) (dfs-helper (car levels) (cdr levels) (cdr complete-path) visited ( - current-depth 1) deepness))  

        ;head is not a goal state, but current level still 
        (#t (dfs-helper (cdr frontier) levels complete-path visited current-depth deepness))
    )
)

;(define (deepining frontier levels complete-path visited current-depth deepness)
 ;   (cond
  ;      ((dfs-helper frontier complete-level visited current-depth deepness) #t)
   ;     (#t (deepining frontier complete-level visited current-depth (+ 1 deepness)))        
    ;)
;)

(define (dis frontier) (dfs-helper (list (list frontier)) '() '() '() 1 4))
