					;-*-Lisp-*-

; Rare-Item Sound Synthesis Algorithm

(progn
  (require 'cookie1) ; This is the package that the vector shuffler uses in the "shuffler" function.
  (require 'cl-lib)
  (shell) ; initiates shell - you only need to do this once - in fact you should ONLY do this once.

(defmacro measure-time (&rest body)
  "This measures the time for code to be evaluated (outputs into message buffer). Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "Generation %s" gen-number)
     (message "Time to run generation: %.03f" (float-time (time-since time)))
     (setq gen-time (format "%.02f" (float-time (time-since time))))
     )
  )

;------------------FUNCTIONS FOR WRITING LOG FILE ---------
(defun batch-set-up ()
  "prints info into log file: how many generations will be created in this current batch and what will the final generation be numbered."
  (save-current-buffer
    (set-buffer "log.txt")
    (goto-char (point-max))
    (newline)
    (insert (format "------------------------------------------------"))
    (newline)
    (insert (format "NEW BATCH: Starting Generation: %s Ending Generation: %s" gen-number (+ gen-number no-of-iterations)))
    (newline)
    (sit-for 0)
    )
  )

(defun log-writer (minfilesize maxfilesize)
  "writes into a log file"
  (save-current-buffer
    (set-buffer "log.txt")
    (newline)
    (insert (format "Generation: %s Best File Size: %s \t Time Taken: %s seconds \t Batch Progress: %s/%s mp3 size: min %s max %s"
		    (- gen-number 1) (nth 0 (nth 0 survivors)) gen-time (- iteration-counter 1) no-of-iterations
		    minfilesize maxfilesize
		    ))
    (write-file log-file)
    (sit-for 0)
    )
  )

;-------------------- FOLDER AND FILE MANAGEMENT FUNCTIONS --------------

(defun directory-contents (folds file-type)
  "returns directory contents for folder [folds] of all files matching [file-type]."
					; file-type must be of the form ".xxx" e.g. ".wav" or ".mp3"
  (setq all-files1 (directory-files folds))
  (setq all-files)
  (mapcar (function (lambda (x)
		      (if (equal (string-suffix-p file-type x 1) t)
			  (push x all-files)
			)
		      )) all-files1)
  all-files
  )

  (defun latest-gen ()
    "Checks whether the algrithm has been run before and outputs a global variable [previously-run]."
    (setq previously-run 0)
    (mapcar (function (lambda (x)
			(if (equal (string-prefix-p "gen-" x) t)
			    (progn
			      (setq high-generation (string-to-number (nth 1 (split-string x "[-]"))))
			      (if (> high-generation previously-run)
				  (setq previously-run high-generation)
				)
			      )
			  )
			)) (directory-files  main-folder))
    )

  (defun bestof-maker ()
    "Checks whether there is a folder entitled 'bestof' in the main folder.  If not, makes one"
    (if (equal (file-directory-p (format "%sbestof/" main-folder)) nil)
	(progn
	  (shell-process-cd main-folder)
	  (shell-command "mkdir bestof")
	)
      )
    )
  

    (defun title-converter (g-num title-length)
      "Creates titles for each generation folder with lots of 0s on the front.
 Takes two arguments - the generation number (g-num), 
  and the total length of any folder title (title-length)"
      (let (
	    (g-conv (number-to-string g-num))
	    (0counter 0)
	    (extra-0s "")
	    )
	(while (< 0counter (- title-length (length g-conv)))
	  (setf extra-0s (format "%s%s" extra-0s "0"))
	  (setf 0counter (+ 0counter 1))
	  )
	(format "gen-%s%s" extra-0s g-conv)  ; returns folder name
	)
      )

  
  (defun gen-folder-maker (m-folds title-in)
    "Creates a new folder for the new generation.  
It takes two arguments, the main-folder and
a title string formated by the title-converter function."
    ; sets this folder as the one to use for the current generation
  (setq current-gen-folder title-in)
   ;Creates new folder named gen-[#] in folder [m-folds] 
  (shell-process-cd m-folds)
  (shell-command (format "mkdir %s" current-gen-folder))
 )


;--------------------MUTATION FUNCTIONS ------------------------


(defun make-new-parents (no-files target-folder percentage-change bit-change)
  "This function creates [no-files] of files in [target-folder] by calling the program plusminusv3-starter.exe.
This program allows for defining the maximum values for the starting noise files [bit-change] and how many of the files should deviate from 0."
    ;; plusminusv3-starter.exe takes 4 arguments when called via the command line:
  ;- [1] output file name
  ;- [2] random seed
  ;- [3] Percentage chance of value changing (out of 100) (int)
  ;- [4] maximum change in integer values.  (maximum 32652)
  ;        This is the maximum amount a integer can be moved up or down.  
  ;        Thus 8 would allow an integer to change by +8 or -8. 
    
   (shell-process-cd target-folder)
   (let (
	 (file-upper 0)
	 )
     (while (< file-upper no-files)
       (setq randseed (random 65000))
       (shell-command (format "%s %s.wav %s %s %s"
			      parent-noise-address (+ file-counter file-upper) randseed percentage-change bit-change))
       (setf file-upper (+ 1 file-upper))
       )
     (setq file-counter (+ file-counter no-files))
     )
   )

(defun cross-breeding-plusminus (gen-folder gen-files percentage-change bit-change file-repeater)
  "Cross breeds all files in list [gen-files] in folder [gen-folder].
  Specifies the percentage of the file to change (out of 100) [percentage-change] and the maximum change up or down of any sample in bits [bit-change].
  New addition: [bit-change] should be a list of values which is cycled through.
  Also it runs this process on each file [file-repeater] number of times."
; plusminusv3.exe takes 5 arguments when called via the command line:
  ;- [1] input file 1  
  ;- [2] output file name
  ;- [3] random seed
  ;- [4] Percentage chance of value changing (out of 100) (int)
  ;- [5] maximum change in integer values.  (maximum 32652)
  ;        This is the maximum amount a integer can be moved up or down.  
  ;        Thus 8 would allow an integer to change by +8 or -8. 
  (let ((count-repeater 1))
    (while (<= count-repeater file-repeater)
      (shell-process-cd gen-folder)
      (mapcar (function (lambda (z)
			  (shell-command (format "%s %s %s.wav %s %s %s"
						 plusminus-address z (number-to-string file-counter) (random 60000) percentage-change
						  (nth (mod count-repeater (length bit-change)) bit-change)
						 ))
			  (setq file-counter (+ file-counter 1))
			  )) gen-files)
      (setf count-repeater (+ count-repeater 1))
      )
    )
  )


(defun cross-breeding-splicer-fixed (gen-folder gen-files min-percentage max-percentage file-repeater)
  "Cross breeds all files in list [gen-files] in folder [gen-folder].
  Swaps a chunk (between min-percentage and max-percentage) of two files and outputs two new files.
  Also it runs this process on each file [file-repeater] number of times."
  (let ((count-repeater 1))
    (while (<= count-repeater file-repeater)
      (shell-process-cd gen-folder)
      (mapcar (function (lambda (z)
; plusminusv3.exe takes 7 arguments when called via the command line:
;- [1] input file 1  
;- [2] input file 2
;- [3] output file 1 name
;- [4] output file 2 name
;- [5] random seed
;- [6] minimum percentage of file to copy (int)
;- [7] maxmum percentage of file to copy (int)*/			  
			  (shell-command (format "%s %s %s %s.wav %s.wav %s %s %s"
						 splicer-address (nth 0 z) (nth 1 z) (number-to-string file-counter) (number-to-string (+ 1 file-counter))
						 (random 60000) min-percentage max-percentage))
			  (setq file-counter (+ file-counter 2))
			  )) gen-files)
      (setf count-repeater (+ count-repeater 2))
      )
    )
  )


  (defun file-matcher (list-of-files)
    "Takes in a list of files and works out all possible combinations."
    (setq file-match (list))
    (mapcar (function (lambda (x)
			  (mapcar (function (lambda (y)
					      (if (not (equal x y))
						  (setq file-match (push (list x y) file-match))
						)
					      )) list-of-files)
			  )) list-of-files)
      file-match
      )
    

;-----------------------------MP3 FUNCTIONS --------------------------
(defun mp3-sizes ()
  "Returns a series of lists with info about the mp3 files created and their sizes.
  mp3-files-and-size - a list of paired lists with the the file names and sizes of all mp3 files."
  (setq mp3-files-and-size (list))
  (mapcar (function (lambda (x)
		      (setq mp3-files-and-size
			    (push
			     (list (nth 7 (file-attributes (format "%s%s" mp3-folder x))) x)
			     mp3-files-and-size))
		      )) (directory-contents mp3-folder ".mp3"))
  )

(defun pair-sorter (listin element-to-sort-by)
  "This takes in a list of x element lists and sorts them according to the element defined by the variable elements-to-sort-by."	
  (let (
	(sort-elem (list))
	(ordered-segment (list))
	(ordered-list (list))
	)
    (mapcar (function (lambda (z)
			(setf sort-elem (append sort-elem (list (nth element-to-sort-by z))))
			)) listin)
    (setf sort-elem (sort (cl-remove-duplicates sort-elem) '<)) 
    (mapcar (function (lambda (r)
			(setf ordered-segment (list))
			(mapcar (function (lambda (s)
					    (if (equal (nth element-to-sort-by s) r)
						(setf ordered-segment (push s ordered-segment))
					      )
					    )) (reverse listin))
			(setf ordered-list (append ordered-list (list ordered-segment)))
			)) sort-elem)
    ordered-list
    )
  )


(defun convergence-prevention (listin no-to-output no-of-top-values)
  "Takes in an ordered list of lists of values and ouputs a list of no-to-output values with no-of-top-values being taken from the top values, and the rest from lower values."
  (let (
	(top-val-counter 0)
	(value-block 0)
	(block-lengths (list 0))
	(block-counter 0)
	(block-no 0)
	(block-found 0)
	(block-to-start-at 0)
	(generation-selection (list))
	)
    (mapcar (function (lambda (r)
			(setf block-counter (+ (length r) block-counter))
			(if (> (length r) no-of-top-values)
			    (progn
			      (setf block-lengths (append block-lengths (list block-counter)))
			      )		  
			  )
			(if (and
			     (> block-counter no-of-top-values)
			     (equal block-found 0)
			     )
			    (progn
			      (setf block-to-start-at (+ 1 block-no))
			      (setf block-found 1)
			      )
			  )
			(setf block-no (+ block-no 1))
			)) listin)   
    (let
	(
	 (shuffled-as-list (list))
	 (shuffled-blocks (list))
	 (shuffled-var nil)
	 )
      (mapcar (function (lambda (r)
			  (setf shuffled-var (shuffler r))
			  (setf shuffled-as-list (append shuffled-as-list shuffled-var))
			  (setf shuffled-blocks (push shuffled-var shuffled-blocks))
			  )) listin)
      (setf shuffled-blocks (reverse shuffled-blocks))
      (print (format "Number of mp3 sizes: %s" (length shuffled-blocks)))
      (if (< (length (nthcdr (nth block-to-start-at block-lengths) shuffled-as-list))
	     (- no-to-output no-of-top-values)
	     )
	  (progn
	    (print "ERROR: Not enough mp3 file values to prevent convergence")
	    (setf generation-selection (append generation-selection (butlast shuffled-as-list (- (length shuffled-as-list) no-to-output))))
	    )
	(progn    
	  (setf generation-selection (append generation-selection (butlast shuffled-as-list (- (length shuffled-as-list) no-of-top-values))))
	  (let
	      (
	       (block-size (- (length shuffled-blocks) block-to-start-at))
	       (block-used nil)
	       (block-loop 0)
	       (select-counter 0)
	       )
	    (setf block-used (last shuffled-blocks block-size))
	    (while (< select-counter (- no-to-output no-of-top-values))
	      (mapcar (function (lambda (r)
				  (if (and
				       (not (equal (nth block-loop r) nil))
				       (< select-counter (- no-to-output no-of-top-values))
				       )
				      (progn 
					(setf generation-selection (append generation-selection (list (nth block-loop r))))
					(setf select-counter (+ 1 select-counter))
					)
				    )
				  )) block-used)
	      (setf block-loop (+ 1 block-loop))
	      )
	    )
	  )
	)
      generation-selection
      )
    )
  )

(defun shuffler (listin)
  "Shuffles a list and returns shuffled list."
  (append (cookie-shuffle-vector (vconcat listin)) nil)
  )

(defun posex-changer (listin)
  "This converts an address from one with forward-slashes to one with back-slashes."
  (let (
	(listin-split (split-string listin "[/]"))
	(end "")
	)
    (mapcar (function (lambda (x)
			(setf end (format "%s%s\\" end x))
			)) (butlast listin))
    end
    )
  )

)		;end of defuns!

;------------

; EVALUATE THE DEFUNS ABOVE FIRST!!!!

;-------------------------
			


;MAIN ALGORITHM - Evaluate the last parentheses of the progn to run.

(progn

;--------------------------------------------
;HOW MANY TIMES SHOULD THE ALGORITHM BE RUN?

(setq no-of-iterations 3)

;---------------------------------

;-------------------------------------------------
;-------------------------------GLOBAL VARIABLES AND FILE LOCATIONS -------------------
					;First, define the folder locations
(setq root-folder "c:/users/david/desktop/compression-project/rarity-algorithm/")

(setq main-folder "c:/users/david/desktop/compression-project/rarity-algorithm/generations/2018-08-29/")
;Here is your noise making .exe:
(setq parent-noise-address "c:/users/david/documents/raritygenerator/code/plusminusv3-starter.exe")
;Here is your plusminus mutation .wav file making .exe:
(setq plusminus-address "c:/users/david/documents/raritygenerator/code/plusminusv3.exe")
;Here is the splicing program that cross-splices two files
(setq splicer-address "c:/users/david/documents/raritygenerator/code/splicer-fixed.exe")


; Number of starting noise files
(setq starting-noise-files 40)
; Number of files carried over to next generation
(setq number-of-offspring-survivors 40)
; Number of top scoring files carried forward, the rest will be randomly selected from the other values
(setq number-of-top-scorers 15)
;Number of randomly-generated files each generation
(setq randomly-generated-files-per-generation 1)
;Number leading zeroes on folder titles
(setq title-length 8)

;MP3 Settings
; Location of LAME Audio Encoder exe:
(setq lame-location "c:/Program Files (x86)/Lame for Audacity/lame.exe")


;--------------------------------------------------------------------------------------------------
;------------------------------PREPARE THE FOLDER FOR WRITING AND SET UP THE LOG FILE.------------



;This code is to check whether this algorithm has previously run in this folder and how far it got previously.
; This should mean the algorithm can automatically start where it left off - preventing mistakes and over-writing.
; Make bestof folder if not already there:
(bestof-maker)
; This variable holds all of the address of all the bestof files
; so that they can be moved at the end of each set of generations
(setq bestof-list (list))
(latest-gen)   ;Checks whether the algrithm has been run before and outputs a global variable [previously-run]
(if (equal previously-run 0)
    (progn
     ;This means the algorithm has not been run in this folder before
     ;Set file counter.  Each file created is sequentially numbered.  This holds across generations.  
     ;N.B. There might be a limit at which Emacs reaches a maximum number but I don't know what this is or if I will ever reach it.
      (setq file-counter 1)
      ;This is the generation number - starts at 1.
      (setq gen-number 1)
      ; Makes folder whose name is the generation number (this will be gen-000...1 - the first folder) 
      (gen-folder-maker main-folder (title-converter gen-number title-length))
      (setq current-gen-location (format "%s%s/" main-folder current-gen-folder))
      )
  (progn
    ;This is run if the algorithm has run in this folder before
    ;This is the generation number
    (setq gen-number previously-run)
    (setq current-gen-folder (title-converter gen-number title-length))
    (setq current-gen-location (format "%s%s/" main-folder current-gen-folder))
    ;This calculates the file in the most recent folder with the highest number and resets the file-counter to 1+ that number.
    (setq old-files (directory-contents current-gen-location ".wav"))
    (setq old-counter 0)
    (mapcar (function (lambda (x)
			(setq old-number (string-to-number (nth 0 (split-string  x "[.]"))))
			(if (> old-number old-counter)
			    (setq old-counter old-number)
			  )
			)) old-files)
    (setq file-counter (+ old-counter 1))
    )
  )

;This code reads in any existing log file or creates a new one if none has been created.
(setq log-file (format "%slog.txt" main-folder))
(if (equal (file-exists-p log-file) t)
    (progn
      (find-file log-file)
      )
  (progn
    (save-current-buffer
      (get-buffer-create "log.txt")
      )
    )
  )

;This writes info about this batch into the log.txt file.
(batch-set-up)

;-------------------------------------------------------
;WHILE LOOP STARTS HERE

(setq iteration-counter 1)
(while (<= iteration-counter no-of-iterations)

  (measure-time ; measures time for each generation

;--------------------------------------------
;----------------GENERATE NEW PARENTS-----------------
   
;Generates new parents in folder [current-gen-folder] defined in [gen-folder] function using plusminusv3-starter.exe
;with [percentage-change] number of changed bits by [bit-change] amount
  (if (equal gen-number 1)
      (make-new-parents starting-noise-files current-gen-location 99 16384) ; Also try 16384
    (make-new-parents randomly-generated-files-per-generation current-gen-location 99 16384) ; don't create any new blank files per generation?
    )

;------------------------------------------------------------------------
;------------------MUTATION-------------------------------------
  
; Gets all file names from folder 
(setq file-list (directory-contents (format "%s%s" main-folder current-gen-folder) ".wav"))
;Generates a list with all possible 2-file combinations of these files
;This is saved in the variable file-match
(file-matcher file-list)

; -----CROSS BREEDING --------
;Cross-breeding algorithm upon all parent files in variable file-match into current-gen-folder
;SPLICING
(cross-breeding-splicer-fixed current-gen-location file-match 25 50 1)
;INCREMENTING PLUS/MINUS
(cross-breeding-plusminus current-gen-location file-list 33 (list 6000 3000 2000 1000 500 16000) 20)

;---------------------------------------------------------------------
;-------------------------FITNESS FUNCTION--------------------------

(setq all-wav-files (directory-contents current-gen-location ".wav")) ;collects names of all wav files

; Make new folder in current-gen-folder for holding all mp3s
(setq mp3-folder (format "%s/mp3/" current-gen-location)) ;mp3 folder variable
(mkdir mp3-folder)

;----WARNING: IF YOU CHANGE THE LOCATION OF THE LAME ENCODER YOU WILL HAVE TO CHANGE THIS!
(shell-process-cd current-gen-location)
(shell-command "for %F in (*.wav) do (\"c:/Program Files (x86)/Lame for Audacity/lame.exe\" --preset standard --quiet %F mp3/%~nF.mp3)")
 
; Creates a list of smaller lists containing the sizes AND file names of all mp3 files in mp3 folder [mp3-files-and-size]
(mp3-sizes)
;Order mp3 list mp3-files-and-size by file size:
(setq sorted-mp3-list (pair-sorter mp3-files-and-size 0))
;Split up list and select amount of top scoring and other files to use.
; This helps prevent convergence problems.
(setq survivors (convergence-prevention sorted-mp3-list number-of-offspring-survivors number-of-top-scorers))

;----------------------------------------------------------------------------
;----------------------------------PREPARE THE NEXT GENERATION -------------

;Copy the file location of the smallest file so that it can be moved to a special "bestof" directory at the end of all generations.
(setq best (format "%s.wav" (nth 0 (split-string (nth 1 (nth 0 survivors)) "[.]"))))
(setq bestof-list (cons (list
			 (format "%s%s" current-gen-location best)
			 (format "%sbestof/%s-%s-%s"  main-folder current-gen-folder (nth 0 (nth 0 survivors)) best)
			 ) bestof-list)
      )

;Make new directory for next generation
(setq old-gen-location current-gen-location)  
(setq gen-number (+ gen-number 1))
;New gen-folder use:
(gen-folder-maker main-folder (title-converter gen-number title-length)) ; this sets the current-gen-folder variable
; Copy all surviving children from previous generation folder to this new one.
(setq current-gen-location (format "%s%s/" main-folder current-gen-folder))
(mapcar (function (lambda (x)
		    (setq mp3conv (format "%s.wav" (nth 0 (split-string (nth 1 x) "[.]"))))
		    (copy-file (format "%s%s" old-gen-location mp3conv)
			       (format "%s%s"  current-gen-location mp3conv))
		    )) survivors)
(setq iteration-counter (+ iteration-counter 1))
)
; Writes info to log file:
(log-writer (nth 0 (nth 0 survivors)) (nth 0 (nth 0 (last survivors))) )
)
;Copy all files that have been added to the "bestof" list into the "bestof" folder.
(mapcar (function (lambda (y)
		    (copy-file (nth 0 y) (nth 1 y))
		    ))
	bestof-list)

)
;--------------------------------------------------
;-------------------RUN HERE----------------------
