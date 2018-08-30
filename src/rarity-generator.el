					;-*-Lisp-*-
; TODO 2018-08-30
; 1. make number-of-top-scorers a percentage
; 2. set up a expected output variable so that you know how many files will be created.
; 3. Try implementing a large-scale genetic algorithm to control all the parameters.
; --- base it on cycles of 3 iterations with a particular setting that is then changed and the parameters ranked according to effectiveness.
; 4. separate the two mutations which are grouped together so they can be altered separately.

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

(defun log-writer (minfilesize maxfilesize folder)
  "writes into a log file"
  (save-current-buffer
    (set-buffer "log.txt")
    (newline)
    (insert (format "Generation: %s Best File Size: %s \t Time Taken: %s seconds \t Batch Progress: %s/%s mp3 size: min %s max %s"
		    (- gen-number 1) (nth 0 (nth 0 survivors)) gen-time (- iteration-counter 1) no-of-iterations
		    minfilesize maxfilesize
		    ))
    (write-file (format "%slog.txt" folder))
    (sit-for 0)
    )
  )

;-------------------- FOLDER AND FILE MANAGEMENT FUNCTIONS --------------

(defun directory-contents (folds file-type)
  "returns directory contents for folder [folds] of all files matching [file-type]."
					; file-type must be of the form ".xxx" e.g. ".wav" or ".mp3"
  (let
      (
       (files-in-directory (directory-files folds))
       (all-files (list))
       )
    (mapcar (function (lambda (x)
			(if (equal (string-suffix-p file-type x 1) t)
			    (push x all-files)
			  )
			)) files-in-directory)
    all-files
    )
  )


  (defun latest-gen (prefix folder)
    "Checks whether the algrithm has been run in folder [folder] before by checking
whether there are any folders with the prefix [prefix] and outputs the variable run-check which is used
for the global variable [previously-run]."
    (let ((run-check 0)
	  (high-generation)
	  )
      (mapcar (function (lambda (x)
			  (if (equal (string-prefix-p prefix x) t)
			      (progn
				(setf high-generation (string-to-number (nth 1 (split-string x "[-]"))))
				(if (> high-generation previously-run)
				    (setf run-check high-generation)
				  )
				)
			    )
			  )) (directory-files  main-folder))
      run-check
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

;--------------------MUTATION FUNCTIONS ------------------------

  (defun file-matcher (list-of-files)
    "Takes in a list of files and works out all possible combinations."
    (let ((file-combs (list)))
      (mapcar (function (lambda (x)
			  (mapcar (function (lambda (y)
					      (if (not (equal x y))
						  (setf file-combs (push (list x y) file-combs))
						)
					      )) list-of-files)
			  )) list-of-files)
      file-combs
      )
    )
    

;-----------------------------MP3 FUNCTIONS --------------------------
  (defun mp3-sizes (folder)
    "Returns a series of lists with info about the mp3 files in [folder] and their sizes.
  mp3-files-and-size - a list of paired lists with the the file names and sizes of all mp3 files."
    (let
	((mp3-output (list)))
      (mapcar (function (lambda (x)
			  (setf mp3-output
				(push
				 (list (nth 7 (file-attributes (format "%s%s" folder x))) x)
				 mp3-output))
			  )) (directory-contents folder ".mp3"))
      mp3-output
      )
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
;---NOTE: eventually have the program set up so that it can be run from the command line with only two parameters:
; the amount of times to run and the file containing all of the variables.

;--------------------------------------------
;HOW MANY TIMES SHOULD THE ALGORITHM BE RUN?
(setq no-of-iterations 3)
;-------------------------------------------------
;-------------------------------GLOBAL VARIABLES AND FILE LOCATIONS -------------------
;FOLDER LOCATIONS
(setq root-folder "c:/users/david/desktop/compression-project/rarity-algorithm")
(setq main-folder (format "%s/generations/2018-08-29/" root-folder))

;Define the addresses of the different external .exe's
;GENERATORS
(setq generator-noise-address (format "%s/build/generator_noise.exe" root-folder))
;MUTATORS 
(setq plusminus-address (format "%s/build/mutator_plus-minus.exe" root-folder))
(setq splicer-address (format "%s/build/mutator_splicer.exe" root-folder))
(setq swapper-address (format "%s/build/mutator_swapper.exe" root-folder)) 
;MP3 Settings
; Location of LAME Audio Encoder exe:
(setq lame-location (format "%s/build/lame/lame.exe" root-folder))
;Number leading zeroes on folder titles
(setq title-length 8)

; Number of starting noise files
(setq starting-noise-files 3)
; Maximum deviation from 0 in parent files
(setq maximum-parent-values 16384)
; Number of files carried over to next generation
(setq number-of-offspring-survivors 7)
; Number of top scoring files carried forward, the rest will be randomly selected from the other values
(setq number-of-top-scorers 3) ;IMPORTANT - THIS MUST BE SMALLER THAN number-of-offspring-survivors - MAYBE TRANSFORM INTO PERCENTAGE?
;Number of randomly-generated files each generation
(setq randomly-generated-files-per-generation 1)


;PLUS MINUS
(setq mutator-plus-minus-chance-of-value-changing 50) ; Chance of an individual value in the file being changed (a percentage out of 100)
(setq mutator-plus-minus-minimum-value-change 200)
(setq mutator-plus-minus-maximum-value-change 1000); maximum amount + or - a value can be changed by
(setq mutator-plus-minus-repeats 2) ; amount of times to re-run the splicer code over all files.

;SPLICER
(setq mutator-splicer-minimum 40) ;minimum_size - minimum percentage of file to copy
(setq mutator-splicer-maximum 60) ;maximum_size - maximum percentage of file to copy
(setq mutator-splicer-repeats 2) ; amount of times to re-run the plus-minus code over all files.
;SWAPPER
(setq mutator-swapper-minimum 40) ;minimum_size - minimum percentage of file to copy
(setq mutator-swapper-maximum 60) ;maximum_size - maximum percentage of file to copy
(setq mutator-swapper-repeats 2) ; amount of times to re-run the swapper code over all files.


;--------------------------------------------------------------------------------------------------
;------------------------------PREPARE THE FOLDER FOR WRITING AND SET UP THE LOG FILE.------------

;This code is to check whether this algorithm has previously run in this folder and how far it got previously.
; This should mean the algorithm can automatically start where it left off - preventing mistakes and over-writing.
; Checks whether there is a folder entitled 'bestof' in the main folder.  If not, makes one:
(if (equal (file-directory-p (format "%sbestof/" main-folder)) nil)
    (progn
      (shell-process-cd main-folder)
      (shell-command "mkdir bestof")
      )
  )
; This variable holds all of the address of all the bestof files
; so that they can be moved at the end of each set of generations
(setq bestof-list (list))
;Checks whether the algrithm has been run before and outputs a global variable [previously-run]
; which either equals 0 if the algorithm has never been run before, or the number of the last generation
(setq previously-run (latest-gen "gen-" main-folder))

(if (equal previously-run 0)
    (progn
     ;This means the algorithm has not been run in this folder before
     ;Set file counter.  Each file created is sequentially numbered.  This holds across generations.  
     ;N.B. There might be a limit at which Emacs reaches a maximum number but I don't know what this is or if I will ever reach it.
      (setq file-counter 1)
      ;This is the generation number - starts at 1.
      (setq gen-number 1)
      ; Makes folder whose name is the generation number (this will be gen-000...1 - the first folder) 
      (setq current-gen-folder (title-converter gen-number title-length))
   ;Creates new folder named gen-[#] in main-folder
      (shell-process-cd main-folder)
      (shell-command (format "mkdir %s" current-gen-folder))     
      (setq current-gen-location (format "%s%s/" main-folder current-gen-folder))
      )
  (progn
    ;This is run if the algorithm has run in this folder before
    ;This is the generation number
    (setq gen-number previously-run)
    (setq current-gen-folder (title-converter gen-number title-length))
    (setq current-gen-location (format "%s%s/" main-folder current-gen-folder))
    ;This calculates the file in the most recent folder with the highest number and resets the file-counter to 1+ that number.
    (let ((old-counter 0)
	  (old-number)
	  )
      (mapcar (function (lambda (x)
			  (setf old-number (string-to-number (nth 0 (split-string  x "[.]"))))
			  (if (> old-number old-counter)
			      (setf old-counter old-number)
			    )
			  )) (directory-contents current-gen-location ".wav"))
      (setq file-counter (+ old-counter 1))
      )
    )
  )

;This code reads in any existing log file or creates a new one if none has been created.
(let ((log-file (format "%slog.txt" main-folder)))
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
;  This code creates [no-files] of files in  by calling the program plusminusv3-starter.exe.
; This code allows for defining the maximum values for the starting noise files [bit-change] and how many of the files should deviate from 0."   
   (shell-process-cd current-gen-location)
   (let (
	 (file-upper 0)
	 (no-files)
	 )
     (if (equal gen-number 1)
	 (setf no-files starting-noise-files) ;number of new files to create at the beginning of each generation
       (setf no-files randomly-generated-files-per-generation)
       )
     
     (while (< file-upper no-files)
; generator_noise.exe output_file_name file_length random_seed maximum_change
       (shell-command (format "%s %s.wav %s %s %s"
			      generator-noise-address 
			      (+ file-counter file-upper) ;output_file_name
			      44100 ;file_length in samples
			      (random 65000) ;random seed
			      maximum-parent-values))
       (setf file-upper (+ 1 file-upper))
       )
     (setq file-counter (+ file-counter no-files))
     )
  

;------------------------------------------------------------------------
;------------------MUTATION-------------------------------------

   (let
       (
       ; Gets all file names from folder 
	(file-list (directory-contents (format "%s%s" main-folder current-gen-folder) ".wav"))
	(file-match)
	(loop-val)
	)
;---------------------MUTATION 1 --------------------------------
;------------------PLUS-MINUS---------------------------------
;Takes in an input .wav file (input_file_name) and adjusts values in the file be a random amount up or down.  Then outputs this adjusted file (output_file_name).  Changes a set percentage of the values in a given file up or down by a specified amount.
;mutator_plus-minus.exe input_file_name output_file_name random_seed percentage_chance_of_value_changing maximum_change_of_value     

       (dotimes (num mutator-plus-minus-repeats loop-val)
	 (mapcar (function (lambda (z)
			     (shell-command (format "%s %s %s.wav %s %s %s"
						    plusminus-address ; executable location
						    z ;input_file_name
						    (number-to-string file-counter) ;output_file_name
						    (random 60000) ;random seed
						    mutator-plus-minus-chance-of-value-changing ; percentage_chance_of_value_changing
						    mutator-plus-minus-maximum-value-change ;maximum_change_of_value
						    mutator-plus-minus-maximum-value-change ;maximum_change_of_value
						    ))
			     (setq file-counter (+ file-counter 1))
			     )) file-list)
	 )
       

;---------------------MUTATION 2 and 3 --------------------------------
;------------------ SPLICER AND SWAPPER ---------------------------------
     
;Generates a list with all possible 2-file combinations of these files
;This is saved in the variable file-match
     (setf file-match (file-matcher file-list))


;mutator_splicer.exe input_file_name_1 input_file_name_2 output_file_name random_seed minimum_size maximum_size
     (mapcar (function (lambda (z)
					;SPLICER
			 (setf loop-val 0)
			 (dotimes (num mutator-splicer-repeats loop-val)
			   (shell-command (format "%s %s %s %s.wav %s %s %s"
						  splicer-address ; location of executable
						  (nth 0 z) ;input_file_name_1
						  (nth 1 z) ;input_file_name_2
						  (number-to-string file-counter) ;output_file_name
						  (random 60000) ;random_seed
						  mutator-splicer-minimum ;minimum_size
						  mutator-splicer-maximum ;maximum_size
						  )
					  )
			   (setq file-counter (+ file-counter 1))
			   )
					;SWAPPER
			 (setf loop-val 0)
			 (dotimes (num mutator-swapper-repeats loop-val)
			   (shell-command (format "%s %s %s %s.wav %s.wav %s %s %s"
						  swapper-address ; location of executable
						  (nth 0 z) ;input_file_name_1
						  (nth 1 z) ;input_file_name_2
						  (number-to-string file-counter) ;output_file_name_1
						  (number-to-string (+ file-counter 1)) ;output_file_name_2
						  (random 60000) ;random_seed
						  mutator-swapper-minimum ;minimum_size
						  mutator-swapper-maximum ;maximum_size
						  )
					  )
			   (setq file-counter (+ file-counter 2))
			   )
			 )) file-match)
     )
     
;---------------------------------------------------------------------
;-------------------------FITNESS FUNCTION--------------------------

   (let
       (
	(mp3-folder (format "%s/mp3/" current-gen-location))
	(mp3-files-and-size)
	(sorted-mp3-list)
	)
   
; Make new folder in current-gen-folder for holding all mp3s
(mkdir mp3-folder)

;----WARNING: IF YOU CHANGE THE LOCATION OF THE LAME ENCODER YOU WILL HAVE TO CHANGE THIS!
(shell-process-cd current-gen-location)
(shell-command "for %F in (*.wav) do (c:/users/david/desktop/compression-project/rarity-algorithm/build/lame/lame.exe --preset standard --quiet %F mp3/%~nF.mp3)")
 
; Creates a list of smaller lists containing the sizes AND file names of all mp3 files in mp3 folder [mp3-files-and-size]
(setf mp3-files-and-size (mp3-sizes mp3-folder))
;Order mp3 list mp3-files-and-size by file size:
(setf sorted-mp3-list (pair-sorter mp3-files-and-size 0))
;Split up list and select amount of top scoring and other files to use.
; This helps prevent convergence problems.
(setq survivors (convergence-prevention sorted-mp3-list number-of-offspring-survivors number-of-top-scorers))
)
;----------------------------------------------------------------------------
;----------------------------------PREPARE THE NEXT GENERATION -------------

;Copy the file location of the smallest file so that it can be moved to a special "bestof" directory at the end of all generations.
   (let
       (
	(best (format "%s.wav" (nth 0 (split-string (nth 1 (nth 0 survivors)) "[.]"))))
	(old-gen-location current-gen-location)
	(mp3conv)
	)
     
     (setq bestof-list (cons (list
			      (format "%s%s" current-gen-location best)
			      (format "%sbestof/%s-%s-%s"  main-folder current-gen-folder (nth 0 (nth 0 survivors)) best)
			      ) bestof-list)
	   )    
;Make new directory for next generation
     (setq gen-number (+ gen-number 1))
; This creates a new folder and also  sets the current-gen-folder variable
     (setq current-gen-folder (title-converter gen-number title-length))
   ;Creates new folder named gen-[#] in main-folder
     (shell-process-cd main-folder)
     (shell-command (format "mkdir %s" current-gen-folder))
     
; Copy all surviving children from previous generation folder to this new one.
     (setq current-gen-location (format "%s%s/" main-folder current-gen-folder))
     (mapcar (function (lambda (x)
			 (setf mp3conv (format "%s.wav" (nth 0 (split-string (nth 1 x) "[.]"))))
			 (copy-file (format "%s%s" old-gen-location mp3conv)
				    (format "%s%s"  current-gen-location mp3conv))
			 )) survivors)
     )
   
   (setq iteration-counter (+ iteration-counter 1))
   )
; Writes info to log file:
  (log-writer (nth 0 (nth 0 survivors)) (nth 0 (nth 0 (last survivors))) main-folder)
)
;Copy all files that have been added to the "bestof" list into the "bestof" folder.
(mapcar (function (lambda (y)
		    (copy-file (nth 0 y) (nth 1 y))
		    ))
	bestof-list)

)
;--------------------------------------------------
;-------------------RUN HERE----------------------
