; Dante Dalla Gasperina
; COEN 266
; HW 2
; Note: I tried to group/organize functions as nicely as possible depnding on what type of function they were. Also, my A* implementation is
; at the bottom.



; The data in the form of an adjacency map

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

;--------------------------------------------------------------------
 
; helper funtion to find the size of a list
(define (list-size-helper alist num)
  (cond
      ((null? alist) num)
      (#t (list-size-helper (cdr alist) (+ num 1)))
  )
)

; wrapper function to find the size of a list
(define (list-size alist) (list-size-helper alist 0))

;--------------------------------------------------------------------

; find the nth item in a list, starting from the left of the list
(define (nth-item num alist) 
    (cond 
        ((equal? num 1) (car alist))
        (#t (nth-item (- num 1) (cdr alist)))
    ) 
)

; replace the nth item of a list with another item
(define (replace-nth-item index alist num)
        (cond 
            ((equal? index 1) (cons num (cdr alist)))
            (#t (cons (car alist) (replace-nth-item (- index 1) (cdr alist) num)))
        ) 
)

; swap two elements by replacing them with each other's respective element
(define (swap-elements a b alist) 
    (let ((temp (nth-item a alist)))
        (replace-nth-item b (replace-nth-item a alist (nth-item b alist)) temp)
    )
)

;--------------------------------------------------------------------

; checks to see if an item 'a' is in a list
(define (exists? alist a)
    (cond
        ((null? alist) #f)
        ((equal? a (car alist)) #t)
        (#t (exists? (cdr alist) a))
    )
)

;--------------------------------------------------------------------

; helper function to check to see if two lists are adjacent
(define (find-adjacentcy a b alist) 
    (cond
        ((null? alist) #f)
        ((equal? (car (car alist)) a) (exists? (cdr (car alist)) b))
        ((equal? (car (car alist)) b) (exists? (cdr (car alist)) a))
        (#t (find-adjacentcy a b (cdr alist)))
    )
)

;wrapper function to check adjacency between two lists
(define (is-adjacent? a b) (find-adjacentcy a b adjacency-map))

;--------------------------------------------------------------------

; helper function to get potential children of a list after a swap
(define (get-children-helper alist i j num)
        (cond
            ((and (equal? i (- num 1)) (equal? j num)) (list (list (swap-elements i j alist) (list i j))))
            ((equal? j num) (cons (list (swap-elements i j alist) (list i j)) (get-children-helper alist (+ 1 i) (+ 2 i) num)))
            (#t (cons (list (swap-elements i j alist) (list i j)) (get-children-helper alist i (+ 1 j) num))) 
        )
)

; wrapper function to get potential children of a list after a swap
(define (get-children alist) (get-children-helper alist 1 2 (list-size alist)))

;--------------------------------------------------------------------

; checks to see whether the current list is a goal state
(define (is-goal-state? alist)
    (cond
        ((null? (cdr alist)) #t)
        ((is-adjacent? (car alist) (car (cdr alist))) (is-goal-state? (cdr alist)))
        (#t #f)
    )
)

;--------------------------------------------------------------------

; takes a list and returns a list of all of the unvisited elements of the passed-in list
(define (unvisited alist visited)
  
    (cond
        ((and (null? (cdr alist)) (not (exists? visited (car (car alist))))) alist)
        ((null? (cdr alist)) '())
        ((not (exists? visited (car (car alist)))) (cons (car alist) (unvisited (cdr alist) visited)))
        (#t (unvisited (cdr alist) visited))    
    )
)

;--------------------------------------------------------------------

; gets a list with only states of the list we are switching and not the switches themselves to make the visited list not depend on switches
; that were used to reach each state
(define (get-pure-states alist)
    (cond 
        ((null? (cdr alist)) (car (car alist)))
        (#t (cons (car (car alist)) (get-pure-states (cdr alist))))        
    )
)

;--------------------------------------------------------------------

; reverses the path list to display steps taken left to right
(define (reverse-path-helper a rest reversed)
    (cond 
        ((null? rest) (append reversed a))
        (#t (reverse-path-helper a (cdr rest) (append (car rest) reversed)))
    )
)

; wrapper function to reverse path list
(define (reverse-path a rest) (reverse-path-helper a rest '()))

;--------------------------------------------------------------------


(define (id-dfs-helper frontier levels complete-path visited current-depth deepness)    
    (cond
        ;if frontier is ever null, we need to pick a new depth
        ((null? frontier) #f)

        ;If head of frontier is goal state, return a list with it and the path we took to get there
        ((is-goal-state? (car (car frontier))) (list (car (car frontier)) (reverse-path (cdr (car frontier)) complete-path)))

        ;Expand leftmost child element if it has not been visited yet as well as add it to visited list 
        ((and (not (equal? current-depth deepness)) (not (exists? visited (car (car frontier))))) (id-dfs-helper (get-children (car (car frontier))) (cons (cdr frontier) levels) (cons (cdr (car frontier)) complete-path) (cons (car (car frontier)) visited) (+ 1 current-depth) deepness))
        
        ;If we are not at designated depth yet and the leftmost element has already been visited, move to the next element to the right on the same level
        ((not (equal? current-depth deepness)) (id-dfs-helper (cdr frontier) levels complete-path visited current-depth deepness)) 
    
        ;head is not a goal state, but current level is empty so we need to go back up level
        ((null? (cdr frontier)) (id-dfs-helper (car levels) (cdr levels) (cdr complete-path) visited ( - current-depth 1) deepness))  

        ;head is not a goal state, but current level still 
        (#t (id-dfs-helper (cdr frontier) levels complete-path visited current-depth deepness))
    )
)

; a wrapper function to deal with increasing depth of search
(define (id-dfs-iteration frontier levels complete-path visited current-depth deepness) 
    (let ((found (id-dfs-helper frontier levels complete-path visited current-depth deepness)))
        (cond 
            (found found)
            (#t (id-dfs-deepening frontier levels complete-path visited (+ 1 current-depth) deepness))
        )
    )    
)

; a wrapper function used to check bounds of the current depth and the final depth and decide when to deduce there is no possible way of achieving the goal
(define (id-dfs-deepening frontier levels complete-path visited current-depth deepness)
    (cond
        ((equal? deepness 1) (list (car (car frontier)) '()))
        ((equal? current-depth deepness) #f)
        (#t (id-dfs-iteration frontier levels complete-path visited current-depth deepness))
    )
)

; wrapper funtion for dis search
(define (id-dfs frontier) (id-dfs-deepening (list (list frontier)) '() '() '() 1 (list-size frontier)))






; -----------------------------------------------------------------------------------------------------------------------------------------------

; This is the A* implementation where I added a couple of functions as well as changed up a couple others. My heruistic
; is based upon the fact that a location with less adjacencies is more likely to be in the front of the list than it is to be in the rest of the list. 
; The reasong behind this is that the states with more adjacencies are more likely to be connected to 2 locations than one with less.
; Basically, I found the location with the minimum amount of adjacencies and always preferenced the states that resulted with this location being in the 
; front of the list. Besides the preferencing of these child states, my algorithm performs like my original id-dfs. 

;--------------------------------------------------------------------------------------

; function used to figure out size of a location's adjacency list
(define (adjacency-size-helper element adjacencies)
    (cond
        ((equal? (car (car adjacencies)) element) (list-size (car adjacencies)))
        (#t (adjacency-size-helper element (cdr adjacencies)))
    )
)

; wrapper for finding length of a location's adjacency list
(define (adjacency-size element) (adjacency-size-helper element adjacency-map))


;--------------------------------------------------------------------------------------

; find the location with the minimum number of adjacencies
(define (find-min-adjacencies-helper alist min)
    (cond
        ((null? alist) min)
        ((< (adjacency-size (car alist)) (adjacency-size min)) (find-min-adjacencies-helper (cdr alist) (car alist)))
        (#t (find-min-adjacencies-helper (cdr alist) min))
    )
)

; wrapper for findin the location with the minimum amount of adjacencies
(define (find-min-adjacencies alist) (find-min-adjacencies-helper (cdr alist) (car alist)))

;--------------------------------------------------------------------------------------

; function to preference locations with the smallest amount of adjacencies in the first position
(define (order-children-astar-helper alist min-values rest min)
    (cond
        ((null? alist) (append min-values rest))
        ((equal? (car (car (car alist))) min) (order-children-astar-helper (cdr alist) (cons (car alist) min-values) rest min))
        (#t (order-children-astar-helper (cdr alist) min-values (cons (car alist) rest) min))
    )
)

; wrapper for ordering children
(define (order-children-astar alist min) (order-children-astar-helper alist '() '() min))

;--------------------------------------------------------------------------------------

; function detailing the A* searching algorithm
(define (A*-helper frontier levels complete-path visited current-depth deepness)    
    (cond
        ;if frontier is ever null, we need to pick a new depth
        ((null? frontier) #f)

        ;If head of frontier is goal state, return a list with it and the path we took to get there
        ((is-goal-state? (car (car frontier))) (list (car (car frontier)) (reverse-path (cdr (car frontier)) complete-path)))

        ;Expand leftmost child element if it has not been visited yet as well as add it to visited list 
        ((and (not (equal? current-depth deepness)) (not (exists? visited (car (car frontier))))) (A*-helper (order-children-astar (get-children (car (car frontier))) (find-min-adjacencies (car (car frontier)))) (cons (cdr frontier) levels) (cons (cdr (car frontier)) complete-path) (cons (car (car frontier)) visited) (+ 1 current-depth) deepness))
        
        ;If we are not at designated depth yet and the leftmost element has already been visited, move to the next element to the right on the same level
        ((not (equal? current-depth deepness)) (A*-helper (cdr frontier) levels complete-path visited current-depth deepness)) 
    
        ;head is not a goal state, but current level is empty so we need to go back up level
        ((null? (cdr frontier)) (A*-helper (car levels) (cdr levels) (cdr complete-path) visited ( - current-depth 1) deepness))  

        ;head is not a goal state, but current level still 
        (#t (A*-helper (cdr frontier) levels complete-path visited current-depth deepness))
    )
)

; a wrapper function to deal with increasing depth of search
(define (A*-iteration frontier levels complete-path visited current-depth deepness) 
    (let ((found (A*-helper frontier levels complete-path visited current-depth deepness)))
        (cond 
            (found found)
            (#t (A*-deepining frontier levels complete-path visited (+ 1 current-depth) deepness))
        )
    )    
)

; a wrapper function used to check bounds of the current depth and the final depth and decide when to deduce there is no possible way of achieving the goal
(define (A*-deepining frontier levels complete-path visited current-depth deepness)
    (cond
        ((equal? deepness 1) (list (car (car frontier)) '()))
        ((equal? current-depth deepness) #f)
        (#t (A*-iteration frontier levels complete-path visited current-depth deepness))
    )
)

; wrapper funtion for A* search
(define (A* frontier) (A*-deepining (list (list frontier)) '() '() '() 1 (list-size frontier)))