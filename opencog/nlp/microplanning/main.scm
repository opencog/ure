; loading additional dependency
(load-scm-from-file "../opencog/nlp/microplanning/sentence-forms.scm")
(load-scm-from-file "../opencog/nlp/microplanning/helpers.scm")
(load-scm-from-file "../opencog/nlp/microplanning/anaphora.scm")


; =======================================================================
; Main microplanning functions
; =======================================================================

; -----------------------------------------------------------------------
; microplanning -- The main microplanning function call
;
; Accepts a SequentialAndLink containing a list of atoms to be spoken.
; "utterance-type" is either 'declarative', 'interrogative', 'imperative'
; or 'interjective'
;
(define (microplanning seq-link utterance-type)
	(define chunks '())
	(define chunks-utterance-type '())
	
	(define (wrap-setlink atoms ut)
		; add additional link base on utterance type
		(SetLink (get-utterance-link ut atoms) atoms)
	)

	(cond ((equal? 'SequentialAndLink (cog-type seq-link))
		; initialize the sentence forms as needed
		(microplanning-init)
	
		; get the best chunking result, and store the utterance type of each individual chunk
		(set-values! (chunks-utterance-type chunks) (make-sentence-chunks (cog-outgoing-set seq-link) utterance-type))
	
		(cond ((not (null? chunks))
			; insert anaphora
			(set! chunks (insert-anaphora chunks (map get-sentence-forms chunks-utterance-type)))
		
			; wrap SetLink around each chunk for Surface Realization
			(map wrap-setlink chunks chunks-utterance-type)
		      )
		      (else
			#f
		      )
		)
	      )
	      (else
		(scm-error 'wrong-type-arg "microplanning" "Wrong type (expecting SequentialAndLink): ~A" (list seq-link) (list seq-link))
	      )
	)
)

; -----------------------------------------------------------------------
; make-sentence-chunks -- The main helper function for handling a microplanning request
;
; Calls make-sentence repeatedly until all informations have been spoken, or
; no longer say-able.  Accepts a list of links 'atoms-set' from within the original
; SequentialAndLink, and the utterance-type.
;
(define (make-sentence-chunks atoms-set utterance-type)
	(define all-comb '())
	(define all-comb-utterance-type '())
	(define all-comb-utterance-variation '())
	(define all-comb-leftover-count '())

	(define (recursive-helper curr-unused curr-chunks curr-uts)
		(define ut (list utterance-type))
		
		; helper function for branching into different utterance type
		(define (sub-helper ut)
			(define new-chunk (make-sentence curr-unused atoms-set ut))
			; if make-sentence returns empty, it means nothing is 'say-able'
			(cond ((not (null? new-chunk))
				; TODO keep some of the atoms (those that do not satisfy sentence forms) for later use?
				(recursive-helper
					(lset-difference equal? curr-unused new-chunk)
					(cons new-chunk curr-chunks) 
					(cons ut curr-uts)
				)
			      )
			      ((not (null? curr-chunks))
				; unable to form more chunks, store the created chunks & their corresponding utterance-type
				(set! all-comb (cons curr-chunks all-comb))
				(set! all-comb-utterance-type (cons curr-uts all-comb-utterance-type))
				(set! all-comb-leftover-count (cons (length curr-unused) all-comb-leftover-count))
			      )
			)		
		)
		
		; if not the first sentence and has "interrogative" utterance type, allow "declarative"
		(if (and (not (null? curr-chunks)) (string=? "interrogative" utterance-type))
			(set! ut (list "interrogative" "declarative"))
		)

		; use the sub-helper to keep calling make sentence until all atoms are used, on all allowed utterance type
		(cond ((not (null? curr-unused))
			(map sub-helper ut)
		      )
	      	      (else
			; finished all atoms, store the created chunks & their corresponding utterance-type
			(set! all-comb (cons curr-chunks all-comb))
			(set! all-comb-utterance-type (cons curr-uts all-comb-utterance-type))
			(set! all-comb-leftover-count (cons 0 all-comb-leftover-count))
		      )
		)
	)

	; loop make-sentence on remaining atoms
	(recursive-helper atoms-set '() '())

	; find which set of chunks is best; a set of chunks is best if:
	; - least amount of leftover atoms
	; - least amount of sentences
	; - least variation in utterance type between sentences
	(cond ((not (null? all-comb))
		; count how many times the utterance type changes within a set of chunks
		(set! all-comb-utterance-variation
			(map (lambda (x) (count (lambda (a b) (not (string=? a b))) x (cdr x))) all-comb-utterance-type))	

		; gather all information into one variable
		(set! all-comb (zip all-comb-leftover-count all-comb-utterance-type all-comb-utterance-variation all-comb))
	
		; only keeps the sets with least leftover
		(sort! all-comb (lambda (x y) (< (car x) (car y))))
		(filter! (lambda (x) (= (car x) (caar all-comb))) all-comb)

		; only keeps the sets with the least sentences
		(sort! all-comb (lambda (x y) (< (length (cadr x)) (length (cadr y)))))
		(filter! (lambda (x) (= (length (cadr x)) (length (cadar all-comb)))) all-comb)

		; only keeps the sets with the least changes between utterance type
		(sort! all-comb (lambda (x y) (< (caddr x) (caddr y))))
		(set! all-comb (car all-comb))

		; reverse since we were adding to the front
		(values (reverse (cadr all-comb)) (reverse (cadddr all-comb)))
	      )
	      (else
		(values '() '())
	      )
	)
)

; -----------------------------------------------------------------------
; make-sentence -- Create a single sentence
;
; Greedily add new atoms (using some heuristics) to form a new sentence.
; Accepts a list of un-said links, the complete set, and the utterance type.
;
(define (make-sentence atoms-unused atoms-set utterance-type)
	(define favored-forms (get-sentence-forms utterance-type))
	; initialize weight bases on "time ordering"
	(define time-weights '())
	; initialize weight (0, 1) bases on whether the atom satisfy a sentence form
	(define form-weights '())
	; initialize weight bases on linkage to other atoms
	(define link-weights '())

	; comb-proc should takes in param "time", "form", "link" in this order
	(define (calc-weights choices bases comb-proc)
		(define choices-len (length choices))

		; helper function: get all nodes for a atom, get all nodes in bases, count how many common nodes
		(define (link-count atom)
			(define atom-nodes (cog-get-all-nodes atom))
			; XXX need to optimize performance here?
			(define used-nodes (delete-duplicates (append-map cog-get-all-nodes bases)))
			(length (lset-intersection equal? atom-nodes used-nodes))
		)

		(set! time-weights (list-tabulate choices-len (lambda (i) (- choices-len i))))
		(set! form-weights (map (lambda (atom) (if (match-sentence-forms atom favored-forms) 1 0)) choices))
		(set! link-weights (map link-count choices))
		
		(display "current weights\n")
		(display time-weights)
		(display "\n")
		(display form-weights)
		(display "\n")
		(display link-weights)
		(display "\nweights done\n")

		; create a combined weight using comb-proc
		(map comb-proc time-weights form-weights link-weights)
	)

	(define atoms-used (lset-difference equal? atoms-set atoms-unused))
	(define atoms-failed '())
	(define chunk '())

	; helper function to sort the atoms list base on weights and choose the highest one
	(define (pick-atom a-list weights)
		(define assoc-list (sort (map cons a-list weights) (lambda (x y) (> (cdr x) (cdr y)))))
		(caar assoc-list)
	)

	; helper function for looping
	(define (recursive-helper atoms-to-try)
		(define head (car atoms-to-try))
		(define result (check-chunk atoms-to-try utterance-type))
		(define atoms-not-tried (lset-difference equal? atoms-unused atoms-to-try))

		(define (give-up-unadded-part)
			; atoms that did not work
			(define dead-set (lset-difference equal? atoms-to-try chunk))
			; atoms that worked
			(define good-set (lset-intersection equal? atoms-to-try chunk))

			(set! atoms-failed (append dead-set atoms-failed))
			(set! atoms-unused (lset-difference equal? atoms-unused dead-set))

			(display "giving up atoms\n")
			(display atoms-failed)
			(display "\n")

			; try "saying" the previous working iteration again (if available) and re-choose a new atom
			(if (null? good-set)
				(if (not (null? atoms-unused))
					(recursive-helper (list (pick-atom atoms-unused (calc-weights atoms-unused atoms-used (lambda (t f l) (* (+ t l) f))))))
				)
				(recursive-helper good-set)
			)
		)
		(define (update-chunk)
			; add anything that has not been included in the chunk yet
			(set! chunk (lset-union equal? atoms-to-try chunk))
			(set! atoms-unused atoms-not-tried)
			(display "updating chunk\n")
			(display chunk)
			(display "\nupdating chunk ended\n")
		)

		; temporary variables not all conditions needed
		(define temp-var1 '())
		(define temp-var2 '())
		(define temp-differences '())

		(display "trying atoms\n")
		(display atoms-to-try)
		(display "\n....\n")

		(cond ; not say-able
		      ((= result 0)
			(display "not say-able\n")
			; could be the atoms cannot be said unless bringing in additional atoms (such as "and/or/that")
			; try to add more (up to 3 different links)
			(cond ((<= (length atoms-to-try) 3)
				; look to see if the newest link has any ConceptNode that is solo
				; (ie. appear only once in the current set)
				(set! temp-var1 (cog-get-all-nodes head))
				(set! temp-var2 (append-map cog-get-all-nodes (cdr atoms-to-try)))
				(set! temp-differences (filter has-word-inst? (lset-difference equal? temp-var1 temp-var2)))

				(cond ((null? temp-differences)
					(give-up-unadded-part)
				      )
				      (else
					; find the first link in atoms-leftover that contains one of the solo word
					(set! temp-var1
						(find (lambda (a)
							(find (lambda (w) (cog-has-node? a w)) temp-differences)
						     )
						     atoms-not-tried
						)
					)

					; if an atom with the solo word exists
					(if temp-var1
						(recursive-helper (cons temp-var1 atoms-to-try))
						(give-up-unadded-part)
					)
				      )
				)
			      )
			      ; give up on all the atoms not "say-able"
			      (else
				(give-up-unadded-part)
			      )
			)
		      )
		      ; not long/complex enough
		      ((= result 1)
			(display "result not long enough\n")
			; add the currently "say-able" stuff to our chunk
			(update-chunk)

			; only continue if there are more to say
			(if (not (null? atoms-unused))
				(recursive-helper
					(cons
						; weight is (time-weights + link-weights) * (2 - form-weights), ie. prefer links that do not
						; satisify a sentence form, meaning it cannot be "said" on its own.
						(pick-atom atoms-unused (calc-weights atoms-unused chunk (lambda (t f l) (* (+ t l) (- 2 f)))))
						atoms-to-try
					)
				)
			)
		      )
		      ; too long/complex
		      (else
			(display "result too long\n")
			(update-chunk)
		      )
		)
	)

	; the initial critera for choosing a starting point would be (time-weights + link-weights) * form-weights
	(recursive-helper (list (pick-atom atoms-unused (calc-weights atoms-unused atoms-used (lambda (t f l) (* (+ t l) f))))))

	; return the sentence chunk (reverse because we've been adding to the front)
	(reverse chunk)
)

; -----------------------------------------------------------------------
; get-sentence-forms -- Get the sentence forms base on utterance type
;
; Base on the utterance type, get the sentence forms defined for it.
;
(define (get-sentence-forms utterance-type)
	(cog-outgoing-set (car
		(cond ((string=? "declarative" utterance-type)
			(cog-chase-link 'InheritanceLink 'OrLink (ConceptNode "declarative"))
		      )
		      ((string=? "interrogative" utterance-type)
			(cog-chase-link 'InheritanceLink 'OrLink (ConceptNode "interrogative"))
		      )
		      ((string=? "imperative" utterance-type)
			'()
		      )
		      ((string=? "interjective" utterance-type)
			'()
		      )
		)
	))
)

; -----------------------------------------------------------------------
; get-utterance-link -- Get the additional link required for an utterance type
;
; Get the additional link required for SuReal to match to sentence of a
; specific utterance type.  To be inserted as part of the output SetLink.
;
(define (get-utterance-link utterance-type atoms)
	; needs the following to handle utterance type with multiple speech acts
	; by doing a hacky search for VariableNode in the sentence-form link
	(define favored-forms (get-sentence-forms utterance-type))
	(define (search-varnode)
		(define (helper atom)
			(and (match-sentence-forms atom favored-forms) (cog-has-atom-type? atom 'VariableNode))
		)
		
		(any helper atoms)
	)
	
	'()
#|
	(cond ((string=? "declarative" utterance-type)
		(InheritanceLink (InterpretationNode "_") (ConceptNode "DeclarativeSpeechAct"))
	      )
	      ((string=? "interrogative" utterance-type)
		; TruthQuerySpeechAct will have no VariableNode on the main sentence-form link
		(if (search-varnode)
			(InheritanceLink (InterpretationNode "_") (ConceptNode "InterrogativeSpeechAct"))
			(InheritanceLink (InterpretationNode "_") (ConceptNode "TruthQuerySpeechAct"))
		)
	      )
	      ((string=? "imperative" utterance-type)
		(InheritanceLink (InterpretationNode "_") (ConceptNode "ImperativeSpeechAct"))
	      )
	      ((string=? "interjective" utterance-type)
		(InheritanceLink (InterpretationNode "_") (ConceptNode "InterjectiveSpeechAct"))
	      )
	)
|#
)

; -----------------------------------------------------------------------
; check-chunk -- Check if a chunk is "say-able"
;
; Call Surface Realization pipeline to see if a chunk of 'atoms' are
; "say-able" for a specific 'utterance-type', and if so, determines whether
; the sentence is too long or complex.
;
; Returns 0 = not sayable, 1 = sayable, 2 = too long/complex
;
(define (check-chunk atoms utterance-type)
	(define (get-pos n)
		(define wi (r2l-get-word-inst n))
		(if (null? wi)
			"_"
			(cog-name (car (word-inst-get-pos wi)))
		)
	)
	(define (sort-n-count l)
		(define sorted-list (sort l string<?))
		(define (count-helper curr-subset curr-pos curr-count)
			(cond ((null? curr-subset)
				(cons (cons curr-pos curr-count) '())
			      )
			      ((not (string=? (car curr-subset) curr-pos))
				(cons (cons curr-pos curr-count) (count-helper (cdr curr-subset) (car curr-subset) 1))
			      )
			      (else
				(count-helper (cdr curr-subset) curr-pos (+ 1 curr-count))
			      )
			)
		)
		(if (not (null? sorted-list))
			(count-helper (cdr sorted-list) (car sorted-list) 1)
			'()
		)
	)

	; XXX optimization needed?
	(define all-nodes (delete-duplicates (append-map cog-get-all-nodes atoms)))
	(define pos-alist (sort-n-count (map get-pos all-nodes)))
	
	(define (pos-check? al)
		(define pos (car al))
		(define count (cdr al))
		
		(cond ; prefer max 2 verbs per sentence
		      ((and (string=? "verb" pos) (>= count 2))
			#f
		      )
		      ; prefer max 3 nouns per sentence
		      ((and (string=? "noun" pos) (>= count 3))
			#f
		      )
		      ; prefer max 5 adjectives
		      ((and (string=? "adj" pos) (>= count 5))
			#f
		      )
		      ; prefer max 3 adverbs
		      ((and (string=? "adv" pos) (>= count 3))
			#f
		      )
		      (else
			#t
		      )
		 )
	)

	(define pos-result (every pos-check? pos-alist))

	(define temp-set-link (SetLink (get-utterance-link utterance-type atoms) atoms))

	; do something with SuReal to see if all atoms can be included in a sentence
	(define say-able #f)
	(receive (sentences weights) (sureal temp-set-link)
		(if (any (lambda (x) (>= x 0)) weights)
			(set! say-able #t)
		)
	)

	(display "\nSuReal output: ")
	(display (sureal temp-set-link))
	(display "\n")
	
	; remove the temporary SetLink
	(cog-delete temp-set-link)

	(cond 
	      ; not long/complex but sayable
	      ((and pos-result say-able) 1)
	      ; long/complex but sayable
	      ((and (not pos-result) say-able) 2)
	      ; not sayable
	      (else 0)
	)
)


;;;;;;;; brain organizing stuff 

; if not long enough
; add external links that share a node? how to determine what to include?
; add the next highest weight atom?
;    prefer atoms with low form-weights & high link weights (with the new chunk, not the old stuff said)?


; some atoms used can be leave out of used to be reused later?
; eg. atoms that do not satisfy a sentence form?


; for interrogative, need to call SuReal as both declarative and interrogative
; and then need to store which one is actually used to form a sentence, before inserting anaphora



