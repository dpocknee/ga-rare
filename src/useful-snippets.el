					;-*-Lisp-*-



(progn
  (shell-process-cd (format "%s/bestof/" main-folder))
  (shell-command "for %F in (*.wav) do (\"c:/Program Files (x86)/Lame for Audacity/lame.exe\" --preset standard --quiet %F %~nF.mp3)")
  )

;FILE SIZE
(progn
  (newline)
  (insert (format "%s" (nth 7 (file-attributes "c:/users/david/documents/raritygenerator/test/"))))
  )


(progn
  ;creates a set of all possible blank and compressed  
   (shell-process-cd "c:/users/david/documents/raritygenerator/mp3sizes/")
   (mapcar (function (lambda (y)
		    (mapcar (function (lambda (x)
					(shell-command (format "\"%s\" -V %s -q %s noise1sec.wav noise1sec-V%s-q%s.mp3" lame-location x y x y))			
					(filesizes x y)
					)) (list 0 1 2 3 4 5 6 7 8 9))
		    )) (list 0 1 2 3 4 5 6 7 8 9)
		       )
   )


;------------------------
;-------------TESTING BATCH
(shell)
(progn
(shell-process-cd "C:/Users/David/Documents/raritygenerator/blankgeneration1/speedtest/")
(setq gen-number 1)
(measure-time
 (shell-command "for %F in (*.wav) do (\"c:/Program Files (x86)/Lame for Audacity/lame.exe\" --preset standard --quiet %F mp3/%~nF.mp3)")
 )
)
;-------------------------
