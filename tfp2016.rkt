#lang at-exp racket
(require xml)
;; Role = Symbol | [Listof Symbol]
;; (person Role String String String (U #f String))
(struct person (role first last affil url) #:transparent)

(struct person/email person (email))
(struct person/co person (country))

;; start PC

(define dvh 
  (person/co '(chair) 
	     "David" "Van Horn" 
	     "University of Maryland"
	     "https://www.cs.umd.edu/~dvanhorn/" 
	     "US"))

(define epfl '(Eacute "cole Polytechnique F" eacute "d" eacute "rale de Lausanne"))
(define pc
  (list
   (person/co '(pcm)
	      "Chris" "Martens"
	      "University of California, Santa Cruz"
	      "http://www.cs.cmu.edu/~cmartens/"
	      "US")
   (person/co '(pcm)
	      "Nada" "Amin"
	      epfl
	      "http://namin.org/"
	      "CH")
   (person/co '(pcm)
	      "Kenichi" "Asai"
	      "Ochanomizu University"
	      "http://www.is.ocha.ac.jp/~asai/"
	      "JP")
   (person/co '(pcm)
	      '("Ma" 322 "gorzata") "Biernacka"
	      "University of Wroclaw"
	      "http://www.ii.uni.wroc.pl/~mabi/"
	      "PL")
   (person/co '(pcm)
	      "Laura" "Castro"
	      '("University of A Coru" ntilde "a")
	      "https://sites.google.com/a/madsgroup.org/laura-castro/"
	      "ES")
   (person/co '(pcm)
	      "Ravi" "Chugh"
	      "University of Chicago"
	      "http://people.cs.uchicago.edu/~rchugh/"
	      "US")
   (person/co '(pcm)
	      "Silvia" "Ghilezan"
	      "University of Novi Sad"
	      "http://imft.ftn.uns.ac.rs/~silvia/"
	      "SR")
   (person/co '(pcm)
	      "Clemens" "Grelck"
	      "University of Amsterdam"
	      "https://staff.science.uva.nl/c.u.grelck/"
	      "NL")
   (person/co '(pcm)
	      "John" "Hughes"
	      "Chalmers University of Technology"
	      "http://www.cse.chalmers.se/~rjmh/"
	      "SE")
   (person/co '(pcm)
	      "Suresh" "Jagannathan"
	      "Purdue University"
	      "https://www.cs.purdue.edu/homes/suresh/"
	      "US")
   (person/co '(pcm)
	      "Pieter" "Koopman"
	      "Radboud University Nijmegen"
	      "https://www.cs.ru.nl/~pieter/"
	      "NL")
   (person/co '(pcm)
	      "Geoffrey" "Mainland"
	      "Drexel University"
	      "https://www.cs.drexel.edu/~mainland/"
	      "US")
   (person/co '(pcm)
	      "Jay" "McCarthy"
	      "University of Massachusetts, Lowell"
	      "https://jeapostrophe.github.io/"
	      "US")
   (person/co '(pcm)
	      "Heather" "Miller"
	      epfl
	      "http://heather.miller.am/"
	      "CH")
   (person/co '(pcm)
	      "Manuel" "Serrano"
	      "INRIA, Sophia-Antipolis"
	      "https://www-sop.inria.fr/members/Manuel.Serrano/"
	      "FR")
   (person/co '(pcm)
	      "Scott" "Smith"
	      "Johns Hopkins University"
	      "https://www.cs.jhu.edu/~scott/"
	      "US")
   (person/co '(pcm)
	      '(Eacute "ric") "Tanter"
	      "University of Chile"
	      "http://pleiad.cl/people/etanter"
	      "CL")
   (person/co '(pcm)
	      "Amal" "Ahmed"
	      "Northeastern University"
	      "http://www.ccs.neu.edu/home/amal/"
	      "US")
   (person/co '(pcm)
	      "Niki" "Vazou"
	      "University of California, San Diego"
	      "http://goto.ucsd.edu/~nvazou/"
	      "US")
   (person/co '(pcm)
	      "Stephanie" "Weirich"
	      "University of Pennsylvania"
	      "https://www.cis.upenn.edu/~sweirich/"
	      "US")))

;; end PC

(define people (cons dvh pc))
(define unknown (person "??" "??" "??" "??" "??"))

(define contest-url "??")

;; Person Symbol -> Boolean
(define (has-role? p s)
  (define r (person-role p))
  (cond [(symbol? r) (eq? r s)]
        [else (ormap (lambda (r) (eq? r s)) r)]))


;; Person -> String
(define (person-last-normalize p)
  (define l (person-last p))
  (if (string? l)
      l
      (apply string-append 
	     (map (lambda (s)
		    (case s
		      [(ntilde) "n"]
		      [else s]))
		  l))))


;; Role -> [Listof Person]
(define (get-roles r)
  (sort (filter (λ (p) (has-role? p r)) people)
        string-ci<?
        #:key person-last-normalize))

;; Role -> Person
(define (get-role r)
  (match (get-roles r)
    [(list p) p]
    [(list) unknown]
    [_ (error 'get-role "Multiple roles")]))

;; Still need: 'industry 'local 'contest 'src

(define www "www/")
(define year "2016")
(define nth "17th")
(define city-img "fountain.png")
(define city '("College Park, Maryland (near Washington, DC)"))
(define dates '("June 8 " ndash " 10"))
(define affiliated-dates '("June 7"))

(define keywords
  @string-append{functional programming, functional programming languages, 
                 software design, software engineering, types, type system,
                 objects, object systems, classes, class systems, paper,
                 workshop, Erlang, Haskell, ML, Scheme, 2013})

(define (link url . s)
  (if url
      `((a ((href ,url)) ,@s))
      s))

(define (listify x)
  (if (cons? x) x (list x)))

(define (person-link p)
  (apply link (person-url p) (append (listify (person-first p))
				     (list " ")
				     (listify (person-last p)))))

(define (row desc p)
  (row/co desc p #f))

(define (row/co desc p person-country)
  `(tr
    (td ((align "left")) ,desc ,@(if (string=? desc "") '() '(":")))
    (td ((align "left"))
        ,@(person-link p))
    (td ((align "left"))
        ,@(local [(define affil (person-affil p))]
	    (append (if (cons? affil) affil (list affil))
                    (if person-country (list " (" (person-country p) ")") '()))))))


(define (role-row desc r)
  (row desc (get-role r)))

(define (role-row/co desc r)
  (row/co desc (get-role r) person/co-country))


(define (role-rows desc r)
  (define ps (get-roles r))
  (cons (row desc (first ps))
        (map (λ (p) (row "" p)) (rest ps))))

(define (role-oxford r)
  (define ps (get-roles r)) 
  (add-between (map person-name/link ps) ", " 
               #:before-last 
               (cond [(< (length ps) 3) " and "]
                     [else ", and "])))

(define (person-name p)
  (cond [(cons? (person-first p))
         (append (person-first p) (list " " (person-last p)))]
        [else
         (list (person-first p) " " (person-last p))]))

(define (role-person/email r)
  (define p (get-role r))
  `(a ((href ,(string-append "mailto:" (person/email-email p))))
     ,@(person-name p)))

(define (person-name/link p)
  (if (person-url p)
      `(a ((href ,(person-url p)))
          ,@(person-name p))
      `(span ,@(person-name p))))

(define col2
  `(div 
    ((class "col2"))
    
    (ul 
     ((class "hidden"))
     (li 
      ((class "roomy"))
      (a 
       ((class "navigation")
        (href "index.html"))
       "TFP 2016"))


     (li
      ((class "roomy"))
      (a ((class "navigation")
          (href "schedule.html"))
         "Schedule"))

     
     (li
      ((class "roomy"))
      (a ((class "navigation")
          (href "accepted.html"))
         "Accepted papers"))
     
     
     (li
      ((class "roomy"))
      (a ((class "navigation")
          (href "cfp.html"))
         "Call for papers"))

     (li
      ((class "roomy"))
      (a ((class "navigation")
          (href "pc.html"))
         "Program committee"))

     (li
      ((class "roomy"))
      (a ((class "navigation")
          (href "submit.html"))
         "Submissions"))

     (li
      ((class "roomy"))
      (a ((class "navigation")
          (href "local.html"))
         "Local information"))

     (li
      ((class "roomy"))
      (a ((class "navigation")
          (href "registration.html"))
         "Registration"))

     (li
      ((class "roomy"))
      (a ((class "navigation")
          (href "tfpie.html"))
         "TFPIE 2016"))                 
    
     (li
      ((class "roomy"))
      (a ((class "navigation")
          (href "http://www-fp.cs.st-andrews.ac.uk/tifp/"))
         "More about TFP"))

     (li
      ((class "roomy"))
      (a ((class "navigation"))
	 "Sponsors")))
         
    (img ((class "sponsor") (src "cyberpoint-logo.png")))
    (img ((class "sponsor") (src "galois-logo.png")))
    (img ((class "sponsor") (src "trail-of-bits-logo.png")))
    (img ((class "sponsor") (src "umd-cs-logo.png")))))

    

(define footers
  `((div
    ((id "footer"))
    (div 
     ((class "whited"))
     (p ((class "left")) "")))))
    
(define important-dates
  `[(h3 "Important dates")
    (table
     ((cellpadding "5"))
     (tr (td "Submissions of draft papers:")
         (td "April 25, 2016"))
     (tr (td "Notification:")
	 (td "May 2, 2016"))
     (tr (td "Registration:")
	 (td "June 1, 2016"))
     (tr (td "TFPIE Workshop:")
	 (td "June 7, 2016"))
     (tr (td "TFP Symposium:")
	 (td "June 8" ndash "10, 2016"))
     (tr (td "Student papers feedback:")
	 (td "June 14, 2016"))
     (tr (td "Submission for formal review:")
	 (td "July 14, 2016"))
     (tr (td "Notification of acceptance:")
	 (td "September 14, 2016"))
     (tr (td "Camera ready paper:")
	 (td "October 14, 2016")))])

(define (make-page title col)
  `(html
    (head
     @title{Trends in Functional Programming @,year : @,title}
     (meta ((http-equiv "Content-Type")
            (content "text/html; charset=us-ascii")))
     (meta ((name "keywords")
            (content ,keywords)))
     (link ((href "column.css")
            (rel "stylesheet")
            (type "text/css"))))
    (body
     (div
      ((id "content"))
      (div 
       ((id "header"))
       (center
        (div
         ((class "whited"))
         @h1[((style "font-size:400%"))]{TFP @,year}
         @h2{The @,nth Symposium on Trends in Functional Programming})))
      (div
       ((class "colmask leftmenu"))
       (div
        ((class "colleft"))
	,col
	,col2))
      ,@footers))))

(define program-committee
  `((h3 "Program Committee")
    (table
     ((cellpadding "5")
      (summary "Organizers"))
     ,(role-row/co "Program Chair" 'chair)
     (tr (td "Program Committee:"))
     ,@(map (lambda (p) (row/co "" p person/co-country)) (get-roles 'pcm)))))


(define schedule.xexpr
  (make-page
    "Schedule"
    `(div
      ((class "col1"))
      (center (h1 "Schedule of events"))

      (p "All sessions for TFP and TFPIE take place in 1110 Kim Engineering.  See "
	 (a ((href "local.html")) "local information") " for a map.")

      
      (h2 "Tuesday, June 7: TFPIE")
      (h3 "9:00" ndash "4:30, See the " (a ((href "http://wiki.science.ru.nl/tfpie/TFPIE2016")) "TFPIE") " website for the schedule.")
      (h2 "Wednesday, June 8: TFP, Day 1")
      
      (h3 "9:00" ndash "9:15: " (a ((href "tfp2016-welcome.pdf")) "Chair's opening remarks"))
      (dl (dd "David Van Horn"))
      
      (h3 "9:15" ndash "10:15: Invited talk")
      (dl
       (dt "Static and Dynamic Type Checking: A Synopsis")
       (dd "Ronald Garcia"))
      (h3 "10:15" ndash "10:45: Break: Coffee, tea, fruit")
      (h3 "10:45" ndash "12:15: Session I")
      (p "Session chair: Marco Moraz" aacute "n")
      (dl
       (dt (a ((href "papers/TFP_2016_paper_11.pdf")) "What is Your Function? Static Pattern Matching on Function Behavior"))
       (dd "Leandro Facchinetti, Pottayil Harisanker Menon, Zachary Palmer, Alexander Rozenshteyn and Scott Smith")
       (dt (a ((href "papers/TFP_2016_paper_10.pdf")) "Project Report: Dependently typed programming with lambda encodings in Cedille"))
       (dd "Ananda Guneratne, Chad Reynolds and Aaron Stump")
       (dt (a ((href "papers/TFP_2016_paper_15.pdf")) "The Random Access Zipper: Simple, Purely-Functional Sequences"))
       (dd "Kyle Headley and Matthew Hammer")
       
       
       )
      (h3 "12:15" ndash "1:30: Lunch: 2460 A.V. Williams")
      (h3 "1:30" ndash "3:00: Session II")
      (p "Session chair: Pieter Koopman")
      (dl
       (dt (a ((href "papers/TFP_2016_paper_16.pdf")) "Improving Sequential Performance of Erlang based on a Meta-tracing Just-In-Time Compiler"))
       (dd "Ruochen Huang, Hidehiko Masuhara and Tomoyuki Aotani")
       (dt (a ((href "papers/TFP_2016_paper_21.pdf")) "Functional, Reactive Web Abstractions"))
       (dd "Loic Denuziere and Adam Granicz")
       (dt (a ((href "papers/TFP_2016_paper_22.pdf")) "Lightweight Affine Static Capabilities"))
       (dd "Brian Mastenbrook and Kevin Marth"))
      
      (h3 "3:00" ndash "3:30: Break: Coffee, tea, fruit")
      (h3 "3:30" ndash "4:30: Session III")
      (p "Session chair: David Van Horn")

      (dl
       (dt (a ((href "papers/TFP_2016_paper_2.pdf")) "Space-Efficient Latent Contracts"))
       (dd "Michael Greenberg")
       (dt (a ((href "papers/TFP_2016_paper_7.pdf")) "Type-Safe Functions and Tasks in a Shallow Embedded DSL for Microprocessors"))
       (dd "Pieter Koopman and Rinus Plasmeijer"))

      (h2 "Thursday, June 9: TFP, Day 2")
      
      (h3 "9:00" ndash "10:30: Session IV")
      (p "Session chair: David Van Horn")
      (dl
       (dt (a ((href "papers/TFP_2016_paper_3.pdf")) "Using DSLs to help people solve rule-based problems"))
       (dd "Nico Naus and Johan Jeuring")
       (dt (a ((href "papers/TFP_2016_paper_5.pdf")) "Threading the Arduino with Haskell"))
       (dd "Mark Grebe and Andy Gill")
              (dt (a ((href "papers/TFP_2016_paper_9.pdf")) "Functional BIP: Embedding Connectors in Functional Programming Languages"))
       (dd "Romain Edelmann, Simon Bliudze and Joseph Sifakis")
       )

      (h3 "10:30" ndash "11:00: Break: Coffee, tea, fruit")
      (h3 "11:00" ndash "12:00: Session V")
      (p "Session chair: Chris Martens")
      (dl
       (dt (a ((href "papers/TFP_2016_paper_8.pdf")) "Cactus Environment Machine: Shared Environment Call-by-Need"))
       (dd "George Stelle, Darko Stefanovic, Stephen Olivier and Stephanie Forrest")
       (dt (a ((href "papers/TFP_2016_paper_17.pdf")) "Hazelnut: A Minimal Bidirectionally Typed Structure Editor"))
       (dd "Cyrus Omar, Michael Hilton, Ian Voysey, Jonathan Aldrich and Matthew Hammer")
)
       
      (h3 "12:00" ndash "1:45: Lunch: 2460 A.V. Williams")
      (h3 "2:00" ndash "6:30: " (a ((href "https://docs.google.com/document/d/12PdRO2WZAIfrUOqhafEnzx66HAhbjgKRsT_7hHRJV6k/edit?usp=sharing")) "Social outing"))
      (p (a ((href "http://americanart.si.edu/")) "Smithsonian American Art Museum")
	 " and the "
	 (a ((href "http://renwick.americanart.si.edu/")) "Renwick Gallery"))
      
      (h3 "7:00" ndash "9:30: TFP Dinner")
      (p (a ((href "http://agoradc.net/")) "Agora") " (" (a ((href "agora_tfp_menu.pdf")) "menu") ")")
      (p "1527 17th Street NW Washington, DC")



      (h2 "Friday, June 10: TFP, Day 3")
      
      (h3 "9:00" ndash "10:15: Invited talk")
      (dl
       (dt "Type- and Example-Driven Program Synthesis")
       (dd "Steve Zdancewic"))
      (h3 "10:15" ndash "10:45: Break")
      (h3 "10:45" ndash "12:15: Session VI")
      (p "Session chair: Scott Smith")

      (dd
       (dt (a ((href "papers/TFP_2016_paper_14.pdf")) "Automatic Parallelization and Transparent Fault Tolerance (Project article)"))
       (dd "Kei Davis, Dean Prichard, David Ringo, Loren Anderson and Jacob Marks")
       (dt (a ((href "papers/TFP_2016_paper_20.pdf")) "Proving Type Class Laws for Haskell"))
       (dd "Andreas Arvidsson, Moa Johansson and Robin Touche")
       (dt (a ((href "papers/TFP_2016_paper_4.pdf")) "Dynamic Flow Analysis for JavaScript"))
       (dd "Nico Naus and Peter Thiemann"))
      
      (h3 "12:15" ndash "1:30: Lunch: 2460 A.V. Williams")

      (h3 "1:30" ndash "3:00: Session VII")
      (p "Session chair: John Hughes")
      (dl
       (dt (a ((href "papers/TFP_2016_paper_1.pdf")) "Separation of Concerns in iTasks " mdash " Implementing a Command & Control System in a Pure Functional Language"))
       (dd "Jurri" euml "n Stutterheim, Peter Achten and Rinus Plasmeijer")
       (dt (a ((href "papers/TFP_2016_paper_18.pdf")) "A Type Checker for Annotated OCaml Abstract Syntax Trees, or An Effective Type System for OCaml"))
       (dd "Pierrick Couderc, Michel Mauny, Gr" eacute "goire Henry and Fabrice Le Fessant")

       (dt (a ((href "papers/TFP_2016_paper_6.pdf")) "A Type Inference System Based on Saturation of Subtyping Constraints"))
       (dd "Benoit Vaugon and Michel Mauny")

       )

      (h3 "3:00" ndash "3:10: Chair's closing remarks")
      (dl (dd "David Van Horn")))))
     
      
(define accepted.xexpr
  (make-page
    "Accepted papers"
     `(div
        ((class "col1"))
	(center
	  (h1 "Papers accepted for presentation at TFP"))
	(dl
	 (dt "Separation of Concerns in iTasks -- Implementing a Command & Control System in a Pure Functional Language")
	 (dd "Jurri" 'euml "n Stutterheim, Peter Achten and Rinus Plasmeijer")

	 (dt "Space-Efficient Latent Contracts")
	 (dd "Michael Greenberg")

	 (dt "Using DSLs to help people solve rule-based problems")
	 (dd "Nico Naus and Johan Jeuring")

	 (dt "Dynamic Flow Analysis for JavaScript")
	 (dd "Nico Naus and Peter Thiemann")

	 (dt "Threading the Arduino with Haskell")
	 (dd "Mark Grebe and Andy Gill")

	 (dt "A Type Inference System Based on Saturation of Subtyping Constraints")
	 (dd "Benoit Vaugon and Michel Mauny")

	 (dt "Type-Safe Functions and Tasks in a Shallow Embedded DSL for Microprocessors")
	 (dd "Pieter Koopman and Rinus Plasmeijer")

	 (dt "Cactus Environment Machine: Shared Environment Call-by-Need")
	 (dd "George Stelle, Darko Stefanovic, Stephen Olivier and Stephanie Forrest")

	 (dt "Functional BIP: Embedding Connectors in Functional Programming Languages")
	 (dd "Romain Edelmann, Simon Bliudze and Joseph Sifakis")

	 (dt "Project Report: Dependently typed programming with lambda encodings in Cedille")
	 (dd "Ananda Guneratne, Chad Reynolds and Aaron Stump")

	 (dt "What is Your Function? Static Pattern Matching on Function Behavior")
	 (dd "Leandro Facchinetti, Pottayil Harisanker Menon, Zachary Palmer, Alexander Rozenshteyn and Scott Smith")

	 (dt "Automatic Parallelization and Transparent Fault Tolerance (Project article)")
	 (dd "Kei Davis, Dean Prichard, David Ringo, Loren Anderson and Jacob Marks")

	 (dt "The Random Access Zipper: Simple, Purely-Functional Sequences")
	 (dd "Kyle Headley and Matthew Hammer")

	 (dt "Improving Sequential Performance of Erlang based on a Meta-tracing Just-In-Time Compiler")
	 (dd "Ruochen Huang, Hidehiko Masuhara and Tomoyuki Aotani")

	 (dt "Hazelnut: A Minimal Bidirectionally Typed Structure Editor")
	 (dd "Cyrus Omar, Michael Hilton, Ian Voysey, Jonathan Aldrich and Matthew Hammer")

	 (dt "A Type Checker for Annotated OCaml Abstract Syntax Trees, or An Effective Type System for OCaml")
	 (dd "Pierrick Couderc, Michel Mauny, Gr" 'eacute "goire Henry and Fabrice Le Fessant")


	 (dt "Proving Type Class Laws for Haskell")
	 (dd "Andreas Arvidsson, Moa Johansson and Robin Touche")

	 (dt "Functional, Reactive Web Abstractions")
	 (dd "Loic Denuziere and Adam Granicz")

	 (dt "Lightweight Affine Static Capabilities")
	 (dd "Brian Mastenbrook and Kevin Marth")))))
	     
	 

(define cfp.xexpr
  (make-page 
   "Call for Papers"
   `(div
     ((class "col1"))
     (center
      (h1 "Call for Papers"))
     ,@important-dates         

     (h3 "Scope")
     
     (p "The symposium recognizes that new trends may arise through
various routes.  As part of the Symposium's focus on trends we
therefore identify the following five article categories. High-quality
articles are solicited in any of these categories:"

	(ul 
	 (li "Research Articles: leading-edge, previously unpublished research work")
	 (li "Position Articles: on what new trends should or should not be")
	 (li "Project Articles: descriptions of recently started new projects")
	 (li "Evaluation Articles: what lessons can be drawn from a finished project")
	 (li "Overview Articles: summarizing work with respect to a trendy subject")))


     (p "Articles must be original and not simultaneously submitted for publication to any other forum. They may consider any aspect of functional programming: theoretical, implementation-oriented, or  experience-oriented.  Applications of functional programming techniques to other languages are also within the scope of the symposium.")

     (p "Topics suitable for the symposium include, but are not limited to:"
	
	(ul 
	 (li "Functional programming and multicore/manycore computing")
	 (li "Functional programming in the cloud")
	 (li "High performance functional computing")
	 (li "Extra-functional (behavioural) properties of functional programs")
	 (li "Dependently typed functional programming")
	 (li "Validation and verification of functional programs")
	 (li "Debugging and profiling for functional languages")
	 (li "Functional programming in different application areas:
security, mobility, telecommunications applications, embedded systems,
global computing, grids, etc.")
	 (li "Interoperability with imperative programming languages")
	 (li "Novel memory management techniques")
	 (li "Program analysis and transformation techniques")
	 (li "Empirical performance studies")
	 (li "Abstract/virtual machines and compilers for functional languages")
	 (li "(Embedded) domain specific languages")
	 (li "New implementation strategies")
	 (li "Any new emerging trend in the functional programming area")))

     (p "If you are in doubt on whether your article is within the scope of "
	"TFP, please contact the TFP 2016 program chair, David Van Horn.")

     (h3 "Best Paper Awards")
     
     (p "To reward excellent contributions, TFP awards a prize for the best paper accepted for the formal proceedings.TFP traditionally pays special attention to research students, acknowledging that students are almost by definition part of new subject trends. A student paper is one for which the authors state that the paper is mainly the work of students, the students are listed as first authors, and a student would present the paper. A prize for the best student paper is awarded each year.")

     (p "In both cases, it is the PC of TFP that awards the prize.  In case the best paper happens to be a student paper, that paper will then receive both prizes.")

     ,@program-committee)))

(define pc.xexpr
  (make-page
   "Program committee"
   `(div
     ((class "col1"))
     ,@program-committee)))

(define submit.xexpr
  (make-page
   "Submissions"
   `(div
     ((class "col1"))
     (center (h1 "Submissions"))
     (p "Acceptance of articles for presentation at the symposium is based on a lightweight peer review process of extended abstracts (4 to 10 pages in length) or full papers (20 pages). The submission must clearly indicate which category it belongs to: research, position, project, evaluation, or overview paper. It should also indicate which authors are research students, and whether the main author(s) are students.  A draft paper for which ALL authors are students will receive additional feedback by one of the PC members shortly after the symposium has
taken place.")

     (p "The URL for submitting papers is: ")
     (center (a ((href "https://easychair.org/conferences/?conf=tfp2016")) "https://easychair.org/conferences/?conf=tfp2016"))
     
     (p "Papers must be written in English and formatted using the LNCS style. For more information about formatting please consult the Springer LNCS web site:")
     (center (a ((href "http://www.springer.com/computer/lncs?SGWID=0-164-6-793341-0"))
		"http://www.springer.com/computer/lncs?SGWID=0-164-6-793341-0")))))
	    	       

(define index.xexpr
  `(html
    (head
     @title{TFP @,year}
     (meta ((http-equiv "Content-Type")
            (content "text/html; charset=us-ascii")))
     (meta ((name "keywords")
            (content ,keywords)))
     (meta ((name "content")
            (content "The conference provides a forum for researchers and developers to hear about the latest work on the design, implementations, principles, and uses of functional programming. The conference covers the entire spectrum of work, from practice to theory, including its peripheries.")))
     (link ((href "column.css")
            (rel "stylesheet")
            (type "text/css"))))
    (body
     (div 
      ((id "content"))
      (div
       ((id "header"))
       (center
        (div 
         ((class "whited"))
	 @h1[((style "font-size: 400%"))]{TFP @,year}
         @h2{The @,nth Symposium on Trends in Functional Programming})))
      (div
       ((class "colmask leftmenu"))
       (div
        ((class "colleft"))
        (div
         ((class "col1"))
         (center
          (div
           ((class "photo"))
           (img ((class "city") (src ,city-img))))
          (h3 @em{University of Maryland, @,@city}
	      (br)
	      @em{TFPIE: June 7, @,year} (br)
	      @em{TFP: @,@dates, @,year}
              (br)
              #;@em["Affiliated TFPIE event: "  ,@affiliated-dates]))
	      
	 (p "The symposium on Trends in Functional Programming (TFP) is an international forum for researchers with interests in all aspects of functional programming, taking a broad view of current and future trends in the area. It aspires to be a lively environment for presenting the latest research results, and other contributions (see the "
	    (a ((href "cfp.html")) "call for papers") 
	    " for details). Authors of draft papers will be invited to submit revised papers based on the feedback receive at the symposium.  A post-symposium refereeing process will then select a subset of these
articles for formal publication.")
                  
	 (p "TFP 2016 will be the main event of a pair of functional
	 programming events. TFP 2016 will be accompanied by "
	    (a ((href "https://wiki.science.ru.nl/tfpie/TFPIE2016"))
	       "the International Workshop on Trends in Functional Programming
	 in Education (TFPIE)")
	    ", which will take place on June 7nd.")

	    (p "Submit here:")
	    (center (a ((href "https://easychair.org/conferences/?conf=tfp2016")) "https://easychair.org/conferences/?conf=tfp2016"))

	    (h3 "Invited Speakers")
	    (table (tr (td (img ((src "garcia-sm.jpg"))))
		       (td (a ((href "http://www.cs.ubc.ca/~rxg/")) "Ronald Garcia")
			   ", University of British Columbia: "
			   (em "Static and Dynamic Type Checking: A Synopsis")))
		   (tr (td (img ((src "zdancewic-sm.jpg"))))
		       (td (a ((href "https://www.cis.upenn.edu/~stevez/")) "Steve Zdancewic")
			   ", University of Pennsylvania:"
			   (em "Type- and Example-Driven Program Synthesis"))))
	    
         ,@important-dates
         
	 )
       
        ,col2)))
     ,@footers)))


  
(define local.xexpr
  (make-page 
   "Local information"	     
   `(div
     ((class "col1"))
     (center
      (h1 "Local information"))
     (h2 "Conference venue")
     (p "The conference will take place at the " 
	"University of Maryland in College Park, Maryland.")

     (p "The symposium will be held in room 1110 of the Jeong H. Kim Engineering Building:")

     (iframe ((src "https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d3101.103782837034!2d-76.93881540552391!3d38.99012769370171!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x0000000000000000%3A0x13a9812612a93c51!2sJeong+H.+Kim+Engineering+Building!5e0!3m2!1sen!2sus!4v1464905314434")
	       (width "600")
	       (height "450")
	       (frameborder "0")
	       (style "border:0")
	       (allowfullscreen "true")))

     
     (h2 "University of Maryland at College Park")

     (img ((src "umd-landscape-sm.png") (class "right")))

     (p "Founded in 1856, UMD is the flagship public university of "
	"Maryland, serving over 37,000 students just outside "
	"Washington, DC")

     (p "The campus is 1,250 acres (5.1 km" (sup "2") 
	") and noted for its "
	"red-brick, Georgian architecture, large central lawn, and 400 "
	"acres (1.6 km" (sup "2") ") of urban forest.")


     (h2 "Functional Programming in the DC Region")

     (p "The Washington, DC region is home to a large, thriving "
	"functional programming community.  There are frequent "
	"meet-ups for FP languages and several authors of FP books are "
	"based in DC")

     (img ((src "plum-logo.png") (class "left")))

     (p "The lab for Programming Languages at the University of "
	"Maryland (PLUM) is an active research group comprised of "
	"nearly 20 researchers with a strong emphasis on functional "
	"languages.")

     (p "Within a four hour drive of UMD are researchers at CMU, Penn, "
	"Johns Hopkins, Princeton, and many more.")

     (p "DC is home to several national laboratories and funding "
	"agencies that use or support FP research.")

     (h2 "Washington, DC")
     
     (img ((src "dc-sm.png") (class "right")))

     (p "Washington, DC is the national capital of the United "
	"States.  It is home to a number of national monuments, parks, "
	"museums, libraries, and the national zoo, all of which have "
	"free admission.")

     (h3 "Getting to DC: By plane")

     (p "DC is served by three major airports with many direct "
	"international flights:")

     (ul 
      (li "DCA - Washington National Airport: "
	  "Located near downtown Washington, DC, this is the most "
	  "convenient airport to fly in to.  It is located on the "
	  "WMATA metro Yellow Line, which runs to UMD during peak "
	  "hours, and requires only a single transfer to the Green "
	  "Line during off-peak hours.  International flights into "
	  "DCA are pretty limited (but there are some).")

      (li "IAD - Dulles International Airport: "
	  "Located about 30 miles West of Washington, DC. this is "
	  "the largest and most international airport in the region "
	  "with direct flights to most major international hubs in "
	  "the world. Travelling from Dulles to DC or College Park "
	  "will require a long and expensive cab ride, a shuttle bus, "
	  "or a somewhat convoluted trip on the WMATA public "
	  "transportation system.")

      (li "BWI - Baltimore-Washington International Airport: "
	  "Located between Washington, DC and Baltimore, MD, this "
	  "airport is about 25 miles Northeast of College Park. "
	  "It is a smaller airport, but include some carriers "
	  "(such as Southwest Airlines) not available at DCA or IAD."))

     (h3 "Getting to DC: By train")

     (p "DC is in the " (a ((href "https://www.amtrak.com/northeast-train-routes")) "\"Northeast Corridor\"") ", one of the few regions "
	"of the US with frequent train service from "
	(a ((href "http://amtrak.com/")) "Amtrak") ", which serves "
	"Washington Union station, located in downtown DC and on "
	"the WMATA metro Red Line.  It is fairly pleasant to travel "
	"to DC from Philadelphia, New York, Boston, and points in "
	"between by Amtrak train.")

     (p "On a more regional scale, Union Station is also served by "
	"the "
	(a ((href "http://mta.maryland.gov/marc-train"))
	   "Maryland Area Regional Commuter (MARC)") 
	" train and the "
	(a ((href "http://www.vre.org/")) 
	   "Virginia Railway Express (VRE)")
	" train .")

     (h3 "Getting around DC")


     (p "There is an excellent public transportation system and "
	"regional train system.  There are many taxis available. "
	"Uber and Lyft operate in the region.")

     (p "If you plan to use public transportation, you purchase a "
	(a ((href "http://www.wmata.com/fares/smartrip/"))
	   "SmarTrip")
	" card, which are available for purchase at all Metro "
	"stations.  The rechargeable card costs $10, but includes "
	"$8 of stored value (so the card itself only costs $2). "
	"Using a SmarTrip card reduces the fare of each trip "
	"by $1, so the card pays for itself with one round-trip. ")

     (p "Metro fares are based on distance travelled (and time "
	"of day), so you will need to use your card to enter "
	(span ((style "font-weight: bold")) "and exit")
	" the system.")

     (p "The city is compact, walkable, and bike-friendly.  The "
	"Capital Bikeshare program makes it easy and convenient to "
	"navigate the city by bicycle.")

     (p "There are several rental car companies in the area and "
	"also several car-sharing services such as Car2Go, "
	"ZipCar, Enterprise CarShare, and Hertz OnDemand. "
	"However, "
	(span ((style "font-weight: bold"))
	      "driving a car in the city is not advised."))

     (h2 "Travelling to College Park from DC")     

     (p "College Park, Maryland is in the Washington DC area "
	" and accessible from DC by the Metro "
	"system.")

     (p "Take the " (a ((href "http://wmata.com/")) "WMATA")
	" Green Line to the College Park" ndash "UMD "
	"station.  During peak hours, the Yellow Line \"rush plus\" "
	"trains also service the College Park"ndash"UMD station.")

     (p "From the College Park station, you can "
	(a ((href "https://goo.gl/maps/b7VhogUXNiq"))
	   "walk 1.3 miles (2.1km) to campus")
	", or you can take the "
	(a ((href "http://www.transportation.umd.edu/shuttle.html"))
	   "UMD Shuttle Bus #104")
	", which boards in the lot to the West of the metro station.  Exit the "
	"bus at the Glenn L. Martin Wind Tunnel, which is next door to "
	"the Computer Science Department in the A. V. Williams "
	"Building and across the street from the Jeong H. Kim Engineering Building.")

     (p "The summer schedule for the 104 is available " (a ((href "http://www.dots.umd.edu/shuttle/schedules/summer/104.pdf")) "here") ".  You can also use the "
	(a ((href "http://www.transportation.umd.edu/nextbus.html")) "NextBus") " service to get real-time bus data.")


     (h2 "Accommodations")

     (p "There are several hotels within walking distance of the TFP venue.")

     (p "Budget hotels ($75-100):")

     (ul (li (a ((href "http://ter.ps/bmv")) "Howard Johnson Inn College Park"))
         (li (a ((href "http://ter.ps/bmw")) "Clarion Inn"))
	 (li (a ((href "http://ter.ps/bmx")) "Quality Inn & Suites")))

     (p "Midrange hotels ($100-150):")

     (ul (li (a ((href "http://ter.ps/bmy")) "College Park Hotel & Suites"))
         (li (a ((href "http://ter.ps/bmz")) "Best Western Plus College Park Hotel")))

     (p "Hi-end hotels ($200+):")

     (ul (li (a ((href "http://ter.ps/bn0")) "College Park Marriott")))
     
     (p "If you'd like to stay in Washington, DC, there are many hotels (they are, in "
        "general, more expensive) and AirBnB options.  To easy travel to College "
	"Park, it is recommended you choose a location near a Green / Yellow line Metro "
	"station.")

     (h2 "Where to eat in College Park")

     (p "There are basically two areas with several restaurants within "
	"walking distance of the TFP venue: there's a row of restaurants "
	"along Route 1 (Baltimore Avenue) that is about a 5 minute walk "
	"away, and there's downtown College Park, which is about a 15 "
	"minute way.")

     (h3 "Restaurants on Route 1, Baltimore Avenue")

     (dl
      (dt (a ((href "http://www.theboardandbrew.com/")) "The Board and Brew")
	  ", $$")
      (dd "Cafe with coffee, espresso drinks, craft beer, sandwiches, "
	  "and small plates.  Oh, and lots and lots of board games.")
      (dt (a ((href "http://sweetgreen.com/")) "Sweetgreen")
	  ", $$")      
      (dd "Salads: \"simple, seasonal, healthy salads and grain bowls made in-house from scratch, using whole produce delivered that morning.\"")
      (dt (a ((href "http://bobbysburgerpalace.com/")) 
	     "Bobby's Burger Palace")
	  ", $$")
      (dd "Burgers: gourmet burgers by celebrity chef Bobby Flay.")
      (dt (a ((href "http://www.shanghaitokyo.co/")) "Shanghai Tokyo")
	  ", $$")
      (dd "Pan-Asian style Chinese, Thai, and sushi.")
      (dt (a ((href "http://looneyspubmd.com/college-park/")) "Looney's Pub")
	  ", $$")
      (dd "College bar with typical bar food menu.  Expect lots of undergrads.")
      (dt (a ((href "http://www.yelp.com/biz/town-hall-liquors-college-park")) "Town Hall") ", $")
      
      (dd "Townie dive bar (and liquor store). OK to bring in outside "
	  "food. A PLUM Lab favorite. Cash only. (ATM sometimes works.)")
      
      (dt (a ((href "http://www.yelp.com/biz/food-factory-college-park-2"))
	     "Food Factory")
	  ", $")
      (dd "Afghan food with a good buffet option.  Better than the name suggests.")
      (dt (a ((href "http://phodlite.com/")) "Pho D'Lite")
	  ", $")
      (dd "South East Asian food with several pho dishes.")
      (dt (a ((href "http://www.yelp.com/biz/hanami-japanese-restaurant-college-park"))
	     "Hanami Japanese Restaurant")
	  ", $$")
      (dd "Japanese dishes including sushi bar"))
      
    

     (h3 "Restaurants in downtown College Park")
     (p "More to come soon.")

     (h2 "Where to eat in DC")
     (p "There are many great restaurants in DC that are easily "
	"accessible from College Park by metro if you'd like to "
	"have a more adventurous evening.  Here are just a few "
	"options.")

     (dl
      (dt (a ((href "http://www.jaleo.com/dc/")) "Jaleo") ", $$$")
      (dd "Spanish tapas by Jos" eacute " Andr" eacute "s in the heart
of the Penn Quarter. "
	  "On Green/Yellow Line between Gallery Place and Archives. "
	  "Reservations recommended.")
      (dt (a ((href "http://jackrosediningsaloon.com/")) "Jack Rose Dining Saloon")
	  ", $$")
      (dd "2,390 bottles of Whisk(e)y on the wall.  Take one down, "
	  "pass it around.  Closest metro is Dupont Circle on the "
	  "Red Line.")
      (dt (a ((href "http://www.rosesluxury.com/")) "Rose's Luxury")
	  ", $$$")
      (dd "Highly sought out restaurant in Capitol Hill area. "
	  "Expect a long wait.  You might be able to make a "
	  "reservation for a group of 6-8."))

     (p "More to come soon."))))


(define tfpie.xexpr
  (make-page 
   "TFPIE"	     
   `(div
     ((class "col1"))
     (center
      (h1 "TFPIE satellite event"))
     ;(h2 "Conference venue")
     (p (a ((href "https://wiki.science.ru.nl/tfpie/TFPIE2016")) 
	   "The 5th International Workshop on Trends in Functional
Programming in Education")
	", TFPIE 2016, will be co-located with the
Symposium on Trends in Functional Programming."))))


(define registration.xexpr
  (make-page 
   "Registration"	     
   `(div
     ((class "col1"))
     (center
      (h1 "Registration"))
     (p "Registration is available at this site: " (a ((href "https://www.regonline.com/tfp2016")) "https://www.regonline.com/tfp2016"))
     (p "The registration fee is $100.  (UMD students are eligible for a discount; email the chair.)  This includes registration for TFP and TFPIE, "
     "lunch for all four days, a social outing, and the TFP dinner in Washington, DC."))))


(define (write-page xexpr fn)
  (with-output-to-file (string-append www fn)
    #:exists 'replace
    (λ ()      
      @displayln{<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">}
      (write-xexpr xexpr))))

(write-page index.xexpr "index.html")
(write-page cfp.xexpr "cfp.html")
(write-page pc.xexpr "pc.html")
(write-page submit.xexpr "submit.html")
(write-page tfpie.xexpr "tfpie.html")
(write-page registration.xexpr "registration.html")
(write-page local.xexpr "local.html")
(write-page accepted.xexpr "accepted.html")
(write-page schedule.xexpr "schedule.html")
;(write-page program.xexpr "program.html")


