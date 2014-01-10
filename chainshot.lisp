;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
;;; Filename: chainshot.lisp
;;;           Implements Chainshot game for CAP5635 Project
;;; Author:   Neeraj Rao
;;;           neeraj AT cise.ufl.edu
;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

(
 defun chainshot ()
   ;;; load data file with board configuration
   ;;; Save configuration into a list
   (format t "Please enter the name of the data file you wish to use~%")
   (setq datafilename (read-line))
   (setq filefound nil) ;;; todo: change this to nil again, delete the line below and uncomment the two lines above - neeraj
   (loop while (not filefound) do
     (cond
       ((probe-file datafilename)
         (setq filefound t)
       )
       (t
         (format t "ERROR: File ~S not found, please check that it exists or enter another file name~%~%" datafilename)
         (setq datafilename (read-line))
       )
     )
   )
   (format t "~%INFO: Using file ~S~%" datafilename)
   (with-open-file (stream datafilename :direction :input)
     (setq configuration (loop for line = (read-line stream nil)
                               until (null line)
                               do( ; remove any spaces between positions in data file
                                 setq line (remove-if (lambda (x) (eq x #\Space)) (coerce line 'list))
                               )
                               collect line
                         )
     )
   )

   ;;; Convert the list to a 2d array
   (defvar myarray (list-to-2d-array configuration))
   (defvar printingtime 0) ;;; we do not want to count the time spent printing output
   (defvar movelist '()) ;;; print final list of moves
   (setq algorithm '())
   (setq griddimension (car (array-dimensions myarray)))
   (display-board)

   ;;; Ask the user to enter next move
   ;;; Validate input (accept only numbers and make sure they are positive and less than grid size)
   ;;; Ask again if there was an error
   (setq gameover nil) ;;; flag to signal when game is over
   (loop while (not gameover) do
     ;;; (display-board)
     (format t "~%Enter your next move in the format 'Row Col' e.g. '1 1' (without the quotes):~%")
     (format t "(You can also enter 'a': autoplay, 'r': show game rules, 'h': how to play, 'q': quit)~%")
     (setq nextMove (read-line))
     (setq spacePos (position #\Space nextMove))
     (setq gameover t)
     (defvar totalscore 0) ;;; we define totalscore as a global variable so we don't have to pass it around to the solve-game and update-game. If the user starts off playing manually and switches to auto solving at some point during the game, the scores from his manual moves are also preserved.
     (defvar processedelements (make-array (list griddimension griddimension))) ;;; keep track of elements we have already processed so we don't repeat it. Processed elements will be t, unprocessed will be nil. Set in id-groups
     ;;; validate input - only positive and only numbers
     (cond
       ((string= nextMove "r")
         (format t "---------------------------------------------------------------------------------------------------------------------~%")
         (format t "Rules of Chainshot:~%")
         (format t "Chainshot is played on a 5x5 to 25x25 grid where each square is initially occupied~%")
         (format t "by a colored bead. There are beads of 5 different colors: Red (R), Green (G), Blue (B),~%")
         (format t "Black (L), and Orange (O).~%~%")
         (format t "A game starts with the entire grid filled with beads. The objective of this game is~%")
         (format t "to remove as many beads as possible -– hopefully, every bead –- from the grid while~%")
         (format t "obtaining the highest score. Only beads that are part of a group can be removed from~%")
         (format t "from the grid. A group is defined as beads  of  the  *same*  color  that  touch  each~%")
         (format t "other  vertically  or  horizontally  (but  NOT diagonally). To remove a bead, you must~%")
         (format t "select its position by specifiying its row and column. The row and column range depends~%")
         (format t "on the size of the grid. For exmaple, for a 5x5 grid, the row and column can vary from~%")
         (format t "1 to 5.~%~%")
         (format t "When a bead and its group are removed, all the beads above it in the same column drop~%")
         (format t "down to fill up the empty space. Any vacancies thus created at the top of the column will~%")
         (format t "remain empty. If at any point an entire column becomes empty because of the removal of a~%")
         (format t "group, the columns to the right will move left to fill up the empty space. The rightmost~%")
         (format t "column will be left empty.~%~%")
         (format t "The scoring of any move is computed as (n-2)^2 where “n” is the number of beads removed.~%")
         (format t "The best game score is determined by: the smallest number of beads left on the board,~%")
         (format t "then the highest sum of the individual moves. You must try to score as hight as possible.~%")
         (format t "---------------------------------------------------------------------------------------------------------------------~%")
         (setq gameover nil)
       )
       ((string= nextMove "h")
         (format t "---------------------------------------------------------------------------------------------------------------------~%")
         (format t "How to Play:~%")
         (format t "To make a move, you must select a bead on the board that is part of a group of beads of similar color.~%")
         (format t "A group of beads is defined as a collection of *more than 2* vertically or horizontally adjacent beads of the same color. ~%")
         (format t " There are beads of 5 different colors: Red (R), Green (G), Blue (B), Black (L), and Orange (O).~%~%")
         (format t "To select a given bead, you must enter its coordinates in the format 'row column' (without the quotes), where~%")
         (format t "the row and column numbers are as displayed beside the board (the vertical column of numbers are the row numbers~%")
         (format t "and the horizontal row of numbers are the column numbers). Remember: row goes first, column goes second. ~%~%")
         (format t "If you select a bead that *is* part of a group of beads, the entire group will be removed and, if the game~%")
         (format t "is not over, you will be given the opportunity to make another move. The game is over when there are no beads left~%")
         (format t "on the board or there are no groups left on the board.~%~%")
         (format t "Good luck!~%")
         (format t "---------------------------------------------------------------------------------------------------------------------~%")
         (setq gameover nil)
       )
       ((string= nextMove "a")
         (format t "Please select which algorithm to use (enter '1' or '2' etc):~%")
         (format t "1. AI Search 1: target largest groups first (intuitive)~%")
         (format t "2. AI Search 2: target groups that leave fewest groups behind first (best-first search)~%")
         (format t "3. AI Search 3: use Depth-First Iterative Deepening~%")
         (format t "4. AI Search 4: use Blind Depth-First search~%")
         (format t "q. Quit~%")
         (setq gameover nil)
         (loop while (not gameover) do
           (setq algorithm (read-line)) ;;; which algorithm to use
           (cond
             ((string= algorithm "1")
               (setq starttime (get-internal-run-time))
               (solve-game "intuitive1")
               (setq endtime (get-internal-run-time))
               (setq gameover t)
             )
             ((string= algorithm "2")
               (setq starttime (get-internal-run-time))
               (solve-game "intuitive2")
               (setq endtime (get-internal-run-time))
               (setq gameover t)
             )
             ((string= algorithm "3")
               (setq starttime (get-internal-run-time))
               (solve-game "DFID")
               (setq endtime (get-internal-run-time))
               (setq gameover t)
             )
             ((string= algorithm "4")
               (setq starttime (get-internal-run-time))
               (solve-game "DFS")
               (setq endtime (get-internal-run-time))
               (setq gameover t)
             )
             ((string= algorithm "q")
               (quit)
             )
             (t
               (format t "ERROR: Please enter one of the outlined choices e.g., '1' or 'q' etc.~%~%")
             )
           )
         )
         (display-board)
       )
       ((string= nextMove "q")
         (quit)
       )
       (t
         (setq move (string-to-list nextMove))
         (cond
           ((= (length move) 2)
             (handler-case
               (progn
                 ;;; (setq row (parse-integer (car move)))
                 ;;; (setq column (parse-integer (cadr move))))
                 (setq row (parse-integer (subseq nextMove 0 spacePos)))
                 (setq column (parse-integer (subseq nextMove spacePos))))
               (parse-error ()
                 (progn
                   (format t "Error: Arguments must be numbers~%")
                   (setq gameover nil)
                 )
               )
             )
             (cond
               (gameover
                 (cond
                   ((and (or (> 1 row) (> 1 column) (< griddimension row) (< griddimension column)))
                     (progn
                       (format t "Error: Arguments must be positive and < ~S~%" griddimension)
                       (setq gameover nil)
                     )
                   )
                   (gameover ;;; arguments supplied are valid, continue with game
                     ;;; (format t "You entered: ~S ~S~%" row column)
                     (setq gameover (update-game (list (- row 1) (- column 1)))) ;;; subtract 1 to make subscripts array-compatible
                     (display-board)
                   )
                 )
               )
             )
           )
           (t ;;; the number of arguments supplied != 2
             (format t "Error: You must supply two arguments~%")
             (setq gameover nil)
           )
         )
       )
     )
   )
   (format t "~%INFO: Game is over!~%")

   (cond
     ((not (string= algorithm "4"))
       ;;; count number of beads left
       (setq remainingbeads 0)
       (loop for row from 0 to (- griddimension 1) do
         (loop for column from 0 to (- griddimension 1) do
           (cond
             ((aref myarray row column)
               (setq remainingbeads (+ remainingbeads 1))
             )
           )
         )
       )
       (format t "~%Moves made: ~S~%" (mapcar #'(lambda (x) (loop for a in x collect (mapcar #'1+ a))) movelist)) ;;; correct array indices for display
       (format t "INFO: Beads Left:~D, Total Score: ~D~%" remainingbeads totalscore)
     )
     (t ;;; For DFS, the latest myarray does not necessarily reflect the best sequence of moves that we made
       (format t "~%Moves made: ~S~%" (mapcar #'(lambda (x) (loop for a in x collect (mapcar #'1+ a))) movelist)) ;;; correct array indices for display
       (format t "INFO: Beads Left:~D, Total Score: ~D~%" remainingbeads totalscore)
     )
   )

   (cond
     ((string= nextMove "a") ;;; print CPU time if the game was solved by the program (rather than manually by user)
       (format t "INFO: Total CPU time: ~8,4,,,'0F seconds~%" (- (/ (- endtime starttime) internal-time-units-per-second) printingtime)) ;;; subtract the time spent printing output
     )
   )
)
(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Solves the game automatically
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun solve-game (algorithm)
    (cond
      ((string= algorithm "intuitive1")
        (format t "---------------------------------------------~%")
        (format t "Targeting largest groups first (intuitive)...~%")
        (format t "---------------------------------------------~%")
        (setq done nil)
        (loop while (not done) do
          ;;; this logic is the same as check-groups except that we do not stop after the first group is found. Instead,
          ;;; we make a list of all the groups by noting down one of the beads that belongs to the group. We also note
          ;;; the group size.
          ;;; (print totalscore) ;;; diagnostic
          (setq processedelements (make-array (list griddimension griddimension))) ;;; must always call this before you call list-groups or id-groups
          (setq allgroups (list-groups)) ;;; make a list of all groups currently on the board
          ;;; (print allgroups) ;;; diagnostic
          (setq groupsizelist '()) ;;; holds a list of group sizes for the corresponding group in allgroups
          (loop for group in allgroups do
            (setq groupsizelist (append groupsizelist (list (length group))))
            ;;; (print groupsizelist) ;;; diagnostic
          )
          (setq largestgroupsizeatthispoint (reduce #'max groupsizelist))
          (setq largestgroup (nth (position largestgroupsizeatthispoint groupsizelist :test #'equal) allgroups)) ;;; pick out the largest group by first finding the maximum group size, determining its offset in groupsizelist and using the same offset to extract the corresponding group members from allgroups
          (display-board)
          (setq thismovescore (* (- largestgroupsizeatthispoint 2) (- largestgroupsizeatthispoint 2))) ;;; calculate this move's score
          (setq totalscore (+ totalscore thismovescore)) ;;; calculate this move's score
          ;(format t "~%INFO: Tiles removed: ~D, Score of this move: ~D, Total Score: ~D~%" largestgroupsizeatthispoint thismovescore totalscore)
          (setq movelist (append movelist (list largestgroup)))
          (remove-group largestgroup)
          ;;; (format t "Removing ~S~%" largestgroup) ;;; diagnostic
          (setq done (check-game-over))
          ;;; (print largestgroup) ;;; diagnostic
          ;;; (format t "~%") ;;; diagnostic
        )
      )
      ((string= algorithm "intuitive2")
        (format t "-----------------------------------------------------------------------------~%")
        (format t "Targeting groups that leave fewest groups behind first (best-first search)...~%")
        (format t "-----------------------------------------------------------------------------~%")
        (setq processedelements (make-array (list griddimension griddimension))) ;;; must always call this before you call list-groups or id-groups
        (setq groupsleft (list-groups)) ;;; make a list of all groups currently on the board
        (loop while groupsleft do
          (display-board)
          (setq numresultinggroups '())
          (setq groupsizes '()) ;;; used for scoring
          (loop for group in groupsleft do
            ;;; (format t "Now examining ~S~%" group)
            (setq tempcopy (copy-array myarray)) ;;; myarray is a global variable and remove-group is about to mess with it. back it up!
            (remove-group group) ;;; remove this group. myarray (a global variable) is changed in this function!!!
            (setq processedelements (make-array (list griddimension griddimension))) ;;; must always call this before you call list-groups or id-groups
            (setq resultinggroups (list-groups)) ;;; see how many new groups result
            ;;; (format t "Resulting groups ~S~%" resultinggroups)
            (setq numresultinggroups (append numresultinggroups (list (length resultinggroups)))) ;;; collect the number of resulting groups into a list. The first element will be the number of resulting groups from removing the first group we tried to remove etc.
            (setq groupsizes (append groupsizes (list (length group)))) ;;; collect the size of the group we just removed
            (setq myarray (copy-array tempcopy)) ;;; restore myarray since it was changed in remove-group
          )
          (setq maxnumresultinggroups (reduce #'min numresultinggroups)) ;;; remove the group that leaves fewest groups behind
          (setq bestgrouptoremove (nth (position maxnumresultinggroups numresultinggroups :test #'equal) groupsleft)) ;;; pick out the best group (the group the removal of which results in the largest number of children) by first finding the maximum group size, determining its offset in groupsizelist and using the same offset to extract the corresponding group members from groupsleft
          (setq movelist (append movelist (list bestgrouptoremove)))
          (setq thisgroupsize (nth (position maxnumresultinggroups numresultinggroups :test #'equal) groupsizes)) ;;; used for scoring
          ;;; (format t "Groups left ~S~%" groupsleft) ;;; diagnostic
          ;;; (format t "maxnumresultinggroups ~S~%" maxnumresultinggroups) ;;; diagnostic
          ;;; (format t "bestgrouptoremove ~S~%" bestgrouptoremove) ;;; diagnostic
          (remove-group bestgrouptoremove)
          (setq thismovescore (* (- thisgroupsize 2) (- thisgroupsize 2))) ;;; calculate this move's score
          (setq totalscore (+ totalscore thismovescore)) ;;; calculate this move's score
          (setq processedelements (make-array (list griddimension griddimension))) ;;; must always call this before you call list-groups or id-groups
          (setq groupsleft (list-groups)) ;;; make a list of all groups currently on the board
        )
      )
      ((string= algorithm "DFID")
        (format t "----------------------------------------~%")
        (format t "Using Depth-first Iterative Deepening...~%")
        (format t "----------------------------------------~%")
        (setq processedelements (make-array (list griddimension griddimension))) ;;; must always call this before you call list-groups or id-groups
        (defvar dfiddone nil)
        (setq depth 1) ;;; start with a depth of 1
        (setq temp (copy-array myarray)) ;;; save the starting state so we can start with it every time we try a new depth
        (loop while (not dfiddone) do
          (setq myarray (copy-array temp)) ;;; start from the starting state every time you try a new depth
          (setq processedelements (make-array (list griddimension griddimension))) ;;; must always call this before you call list-groups or id-groups
          (loop for group in (list-groups)
            if (not dfiddone) do
            (setq myarray (copy-array temp)) ;;; start from the starting state every time you try a new depth
            ;;; (format t "Depth ~D, Starting DFID-DFS with ~S~%" depth group)
            ;;; (display-board)
            (setq movelist '())
            (dfid-dfs group depth myarray 0) ;;; totalscore = 0 is because we start from scratch every time

            ;;; count number of beads left
            (setq remainingbeads 0)
            (loop for row from 0 to (- griddimension 1) do
              (loop for column from 0 to (- griddimension 1) do
                (cond
                  ((aref myarray row column)
                    (setq remainingbeads (+ remainingbeads 1))
                  )
                )
              )
            )
          )
          (setq depth (1+ depth))
        )
      )
      ((string= algorithm "DFS")
        (format t "---------------------------------~%")
        (format t "Using Blind Depth-First Search...~%")
        (format t "---------------------------------~%")
        (defvar dfsresults '()) ;;; collect results of different branches of DFS. Format is ((remainingbeads1 totalscore1) (remainingbeads2 totalscore2)...)
        (setq temp (copy-array myarray))
        (setq processedelements (make-array (list griddimension griddimension))) ;;; must always call this before you call list-groups or id-groups
        (loop for group in (list-groups) do
          ;;; (format t "~%Starting DFS with ~S~%" group)
          (setq myarray (copy-array temp))
          (setq movelist '())
          (dfs group myarray 0) ;;; totalscore = 0 is because we start from scratch every time
        )
        ;;; choose the best results of all our runs
        (setq dfsresults (sort dfsresults #'> :key #'cadr)) ;;; first, sort in order of decreasing game scores
        (setq dfsresults (sort dfsresults #'< :key #'car))  ;;; second, sort in order of increasing remaining beads
        (setq remainingbeads (caar dfsresults))
        (setq totalscore (cadar dfsresults))
        ;(print dfsresults)
      )
    )
)


(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Implement depth-first search for depth-first iterative deepening
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun dfid-dfs (group depth currentarray score)
    (cond
      ((> depth 0)
        (setq myarray (copy-array currentarray))
        (format t "~%Depth ~D, Removing ~S~%" depth (mapcar #'(lambda (x) (mapcar #'1+ x)) group)) ;;; correct array indices for display
        (setq movelist (append movelist (list group)))
        (remove-group group)
        (setq thisgroupsize (length group))
        (setq thismovescore (* (- thisgroupsize 2) (- thisgroupsize 2))) ;;; calculate this move's score
        (setq score (+ score thismovescore)) ;;; calculate this move's score
        ;;; (format t "size of group just removed ~D, score ~D~%" thisgroupsize score)
        (display-board)
        (cond
          ((check-game-over)
            ;;; (format t "game over detected~%")
            (setq dfiddone t)
            (setq totalscore score)
          )
          (t
            (setq currentarray (copy-array myarray))
            (setq processedelements (make-array (list griddimension griddimension))) ;;; must always call this before you call list-groups or id-groups
            (setq newgroups (list-groups))
            ;;; (setq newgroups (sort newgroups #'> :key #'length))
            (loop for group in newgroups do
              (dfid-dfs group (1- depth) currentarray score)
            )
          )
        )
      )
    )
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Implement depth-first search
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun dfs (group currentarray score)
    (setq myarray (copy-array currentarray))
    (format t "~%Removing ~S~%" (mapcar #'(lambda (x) (mapcar #'1+ x)) group)) ;;; correct array indices for display
    (setq movelist (append movelist (list group)))
    (remove-group group)
    (display-board)
    (setq thisgroupsize (length group))
    (setq thismovescore (* (- thisgroupsize 2) (- thisgroupsize 2))) ;;; calculate this move's score
    ;;; (format t "Score before ~D~%" score)
    (setq score (+ score thismovescore)) ;;; calculate this move's score
    ;;; (format t "Score after ~D~%" score)
    (cond
      ((check-game-over)
        (setq totalscore score)

        ;;; count number of beads left
        (setq remainingbeads 0)
        (loop for row from 0 to (- griddimension 1) do
          (loop for column from 0 to (- griddimension 1) do
            (cond
              ((aref myarray row column)
                (setq remainingbeads (+ remainingbeads 1))
              )
            )
          )
        )
        ;(format t "INFO: Beads Left:~D, Total Score: ~D~%" remainingbeads totalscore)
        (setq dfsresults (append dfsresults (list (list remainingbeads totalscore)))) ;;; collect results into dfsresults
      )
      (t
        (setq currentarray (copy-array myarray))
        (setq processedelements (make-array (list griddimension griddimension))) ;;; must always call this before you call list-groups or id-groups
        (setq newgroups (list-groups))
        ;;; (setq newgroups (sort newgroups #'> :key #'length))
        (loop for group in newgroups do
          (dfs group currentarray score)
        )
      )
    )
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Returns a list of all the groups on the board
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun list-groups ()
    (progn
      (setq allgroups '()) ;;; holds a list of groups. The format is (((group1_member1) (group1_member2)...) ((group2_member1)(group2_member2)...)...)
      (loop for col from 0 to (- griddimension 1) do
        (loop for row from 0 to (- griddimension 1) do
          (progn
            (cond
              ((and (not (aref processedelements row col)) (aref myarray row col)) ;;; proceed only if we haven't already done this element
                ;;; (format t "Now checking (~D ~D)~%" row col) ;;; diagnostic
                (setq numingroup 0) ;;; used to count number of beads of this color in the group (if a group exists)
                (setq numingroup (id-groups (list row col (aref myarray row col) 'start))) ;;; start with (0 0)
                (cond
                  ((> numingroup 2) ;;; we have a group
                    (setq allgroups (append allgroups (list groupmembers)))
                    ;;; (setq allgroups (append allgroups (list (list (length groupmembers) groupmembers)))) ;;; diagnostic
                    ;;; (print allgroups) ;;; diagnostic
                    ;;; (format t "~%") ;;; diagnostic
                  )
                )
              )
            )
          )
        )
      )
      allgroups
    )
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Update game state
  ;;; Returns nil if game is not over
  ;;; Returns t   if game is over
  ;;; Note also that the fact that we're entering this function means that the game is incomplete. We must check
  ;;; whether the game is over just before we leave this function so that we don't call it again if the game is
  ;;; indeed over.
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun update-game (list)
    (let* ;;; we use let* and not let because bead must be set only after row and column have been assigned their values
      (
        (row (car list))
        (column (cadr list))
        (bead (aref myarray row column))
      )
      (cond
        ((not (aref myarray row column)) ;;; if element is nil, print a space
          (format t "~%INFO: There is no bead at position (~S ~S)!~%~%" (+ row 1) (+ column 1)) ;;; increment row and column to avoid confusing the user (let him see what he entered)
        )
        (t
          (format t "~%INFO: The bead at position (~S ~S) is ~C~%" (+ row 1) (+ column 1) (aref myarray row column)) ;;; increment row and column to avoid confusing the user (let him see what he entered)
        )
      )
      (cond
        ((eql bead nil) ;;; no bead was in the location chosen by the user
          nil
        )
        ((or (eql bead '#\A) (eql bead '#\B) (eql bead '#\C) (eql bead '#\D) (eql bead '#\E) (eql bead '#\F) (eql bead '#\G) (eql bead '#\H) (eql bead '#\I) (eql bead '#\J) (eql bead '#\K) (eql bead '#\L) (eql bead '#\O) (eql bead '#\P) (eql bead '#\R) (eql bead '#\Y)) ;;; valid bead was found. Now determine if bead selected is part of a group (group = set of > 2 adjacent beads of same color)
          ;;; (format t "Looking for groups of color ~C...~%" bead) ;diagnostic
          (setq numingroup 0) ;;; used to count number of beads of this color in the group (if a group exists)
          (setq processedelements (make-array (list griddimension griddimension))) ;;; must always call this before you call list-groups or id-groups
          (setq numingroup (id-groups (list row column bead 'start)))
          (cond
            ((> numingroup 2)
              (progn
                (format t "INFO: Group found! Number of beads in group = ~D~%INFO: Updating game state...~%" numingroup)
                ;;; (print groupmembers) ;;; diagnostic
                (setq movelist (append movelist (list groupmembers)))
                (remove-group groupmembers) ;;; remove all members of group
                (setq thismovescore (* (- (length groupmembers) 2) (- (length groupmembers) 2)))
                (setq totalscore (+ totalscore thismovescore))
                (format t "~%INFO: Tiles removed: ~D, Score of this move: ~D, Total Score: ~D~%" (length groupmembers) thismovescore totalscore)
                (check-game-over)
              )
            )
            (t
              (format t "INFO: No group found~%")
              nil ;;; return nil so that game can go on
            )
          )
        )
      )
    )
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Check if game is over. The game is over if there are
  ;;; a. either no beads remaining on the board OR
  ;;; b. no groups on the board.
  ;;; Returns
  ;;;   t   if game is over
  ;;;   nil if game is not over
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun check-game-over ()
    (progn
      (setq notempty nil)
      (loop for col from 0 to (- griddimension 1) while (not notempty) do
        (loop for row from 0 to (- griddimension 1) do
          (cond
            ((aref myarray row col)
              (setq notempty t)
            )
          )
        )
      )
      (cond
        (notempty ;;; there are still beads on the board. Now check if there are groups.
          (check-groups)
        )
        (t ;;; board is empty so game is over
          t
        )
      )
    )
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Check if there are any groups on the board.
  ;;; Returns
  ;;;   t   if game is over
  ;;;   nil if game is not over
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun check-groups ()
    (progn
      (setq groupfound nil)
      (setq processedelements (make-array (list griddimension griddimension))) ;;; must always call this before you call list-groups or id-groups
      (loop for col from 0 to (- griddimension 1) while (not groupfound) do
        (loop for row from 0 to (- griddimension 1) do
          (progn
            (cond
              ((and (not (aref processedelements row col)) (aref myarray row col)) ;;; proceed only if we haven't already done this element
                ;;; (format t "Now checking (~D ~D)~%" row col) ;;; diagnostic
                (setq numingroup 0) ;;; used to count number of beads of this color in the group (if a group exists)
                (setq numingroup (id-groups (list row col (aref myarray row col) 'start))) ;;; start with (0 0)
                (cond
                  ((> numingroup 2) ;;; we have a group, so the game is not over
                    (setq groupfound t)
                  )
                )
              )
            )
          )
        )
      )
      (not groupfound) ;;; return nil if a group was found since that means the game is NOT over. Return t if no group was found since that means the game IS over.
    )
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Remove group members
  ;;; Take care to move the remaining beads left or down if required.
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun remove-group (groupmembers)
    (let
      (
        (colwisegroupmembers (make-array griddimension))
        (minarray (make-array griddimension))
        (maxarray (make-array griddimension))
      )
      (loop for currentelement in groupmembers do ;;; continue until there are no elements left to process
        (progn
          (setq currentcolumn (cadr currentelement))
          (setq currentrow (car currentelement))
          (setf (aref colwisegroupmembers currentcolumn) (append (aref colwisegroupmembers currentcolumn) (list currentrow)))
        )
      )
      ;;; (print colwisegroupmembers) ;;; diagnostic
      ;;; (format t "~%")
      (loop for currentcolumn from (- griddimension 1) downto 0 do
        (progn
          (setq adjustrowby 0)
          (setq colgroupmembers (aref colwisegroupmembers currentcolumn)) ;;; list of row indices in this column that belong to the group
          ;;; (format t "Starting column ~D~%" currentcolumn) ;;; diagnostic
          (cond
            (colgroupmembers ;;; proceed only if this column actually has elements that are group members
              (cond
                ((and (= (reduce #'min colgroupmembers) 0) (= (reduce #'max colgroupmembers) (- griddimension 1)) (= (length colgroupmembers) griddimension)) ;;; if the entire column is empty (i.e., min = 0 and max = griddimension-1, shift the columns to the right one to the left
                  (progn
                    ;;; (print colgroupmembers) ;;; diagnostic
                    ;;; (format t "~%") ;;; diagnostic
                    (loop for col from currentcolumn to (- griddimension 2) do ;;; shift all elements left by one column
                      ;;; (format t "Copying col ~D to col ~D~%" (+ col 1) col)
                      (loop for row from 0 to (- griddimension 1) do ;;; shift all elements left by one column
                        (setf (aref myarray row col) (aref myarray row (+ col 1)))
                      )
                    )
                    (loop for row from 0 to (- griddimension 1) do ;;; rightmost column gets nil
                      (setf (aref myarray row (- griddimension 1)) nil)
                    )
                    ;;; (display-board) ;;; diagnostic
                  )
                )
                (t ;;; entire column is not empty but we do have elements that are group members
                  (loop while (> (length colgroupmembers) 0) do ;;; continue until there are no elements of the group left in this column
                    (setq currentrow (reduce #'max colgroupmembers)) ;;; go from bottom towards top
                    (setq currentrow (+ currentrow adjustrowby))
                    ;;; (format t "Now handling row ~D column ~D~%" currentrow currentcolumn) ;;; diagnostic
                    ;;; (print colgroupmembers) ;;; diagnostic
                    ;;; (read-line) ;;; diagnostic
                    (loop for row from currentrow downto 1 do ;;; shift all elements down by one row
                      (setf (aref myarray row currentcolumn) (aref myarray (- row 1) currentcolumn))
                      ;;; (format t "Copying row ~D to row ~D~%" (- row 1) row)
                    )
                    (setf (aref myarray 0 currentcolumn) nil) ;;; topmost row gets nil
                    (setq colgroupmembers (remove (- currentrow adjustrowby) colgroupmembers))
                    (setq adjustrowby (+ adjustrowby 1))
                    ;;; (display-board) ;;; diagnostic
                  )
                )
              )
            )
          )
        )
      )
      (loop for currentcolumn from (- griddimension 1) downto 0 do ;;; if any of the columns are newly empty, shift the remaining columns one to the left
        (setq colempty t)
        (loop for currentrow from 0 to (- griddimension 1) do ;;; if any of the columns are newly empty, shift the remaining columns one to the left
          (cond
            ((aref myarray currentrow currentcolumn)
              (setq colempty nil)
            )
          )
        )
        (cond
          (colempty
            ;;; (format t "Column ~D was empty~%" currentcolumn)
            (loop for col from currentcolumn to (- griddimension 2) do ;;; shift all elements left by one column
              ;;; (format t "Copying col ~D to col ~D~%" (+ col 1) col)
              (loop for row from 0 to (- griddimension 1) do ;;; shift all elements left by one column
                (setf (aref myarray row col) (aref myarray row (+ col 1)))
              )
            )
            (loop for row from 0 to (- griddimension 1) do ;;; rightmost column gets nil
              (setf (aref myarray row (- griddimension 1)) nil)
            )
          )
        )
      )
      ;;; (display-board)
      myarray
    )
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Display the board on the screen
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun display-board ()
  (progn
    (setq startprintingtime (get-internal-run-time))
    (format t "~%")
    (loop for j from 0 to (- griddimension 1) do ;;; print nice separator above board
      (format t " - ")
    )
    (format t " ~%")
    (loop for i from 0 to (- griddimension 1) do
      (loop for j from 0 to (- griddimension 1) do
        (cond
          ((not (aref myarray i j)) ;;; if element is nil, print a space
            (format t "   ")
          )
          (t
            (format t " ~C " (aref myarray i j))
          )
        )
      )
      (format t " | ~2D~%" (+ i 1)) ;;; print row number after each line
    )
    (loop for j from 0 to (- griddimension 1) do ;;; print nice separator below board
      (format t " - ")
    )
    (format t " | ^- Row~%")
    (loop for j from 0 to (- griddimension 1) do ;;; print column number after entire board
      (format t "~2D " (+ j 1))
    )
    (format t " | <- Col~%")
    (setq endprintingtime (get-internal-run-time))
    (setq printingtime (+ printingtime (/ (- endprintingtime startprintingtime) internal-time-units-per-second))) ;;; we do not want to count the time spent printing output
  )
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Check if the coordinates provided as input are part of a group
  ;;; A group is defined as a collection of > 2 adjacent beads of the same color.
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun id-groups (list) ;;; list is (row column bead direction)
    (let
      (
        (row (car list))
        (column (cadr list))
        (bead (caddr list))
        (direction (cadddr list))
      )
      (setq elementstoprocess (list (list row column))) ;;; add the current element we're processing to the list. The element itself is represented as a list (row column)
      (setq groupmembers '()) ;;; this list will contain only groupmembers, elementstoprocess will, by the end of this function, have contained all the beads on the board
      (loop while (> (length elementstoprocess) 0) do ;;; continue until there are no elements left to process
        (progn
          (setq currentelement (car elementstoprocess)) ;;; get next (row column) to process
          (setq currentrow (car currentelement))
          (setq currentcolumn (cadr currentelement))
          (cond
            ((not (aref processedelements currentrow currentcolumn)) ;;; needed if we're processing an element that's already been processed. This can happen because we can queue the same location multiple times.
              (progn
                ;;; (print elementstoprocess) ;;; diagnostic
                (setq numingroup (+ numingroup 1))
                (setq groupmembers (append groupmembers (list (list currentrow currentcolumn)))) ;;; add this element to the queue of elements to be processed
                ;;; (format t "Now processing (~D ~D), numingroup = ~D~%" currentrow currentcolumn numingroup) ;;; diagnostic
                (check-neighbors currentrow currentcolumn bead)
              )
            )
          )
          (setf (aref processedelements currentrow currentcolumn) t) ;;; we're done with this element; do not repeat it
          (setq elementstoprocess (cdr elementstoprocess)) ;;; first element we popped is done; discard it
        )
      )
      numingroup
    )
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Check the four cardinal directions for elements that are the same color as the
  ;;; one we are currently processing. 
  ;;; Adds (row column) elements that are yet to be processed to the list elementstoprocess
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun check-neighbors (row column bead)
    (progn
      ;;; (format t "row = ~D col = ~D bead = ~C~%" row column bead)
      ;;; (format t "row = ~D col = ~D processed? ~S~%" row column (aref processedelements row column))
      ;;; (setf blah (read-line))
      (cond
        ((and (> row 0) (eql (aref myarray (- row 1) column) bead) (not (aref processedelements (- row 1) column))) ;;; important that you put (> row 0) as the first condition to be tested in the and because if it fails, we'll never encounter the subsequent aref with a negative index
           (setq elementstoprocess (append elementstoprocess (list (list (- row 1) column)))) ;;; add this element to the queue of elements to be processed
        )
      )
      (cond
        ((and (< row (- griddimension 1)) (eql (aref myarray (+ row 1) column) bead) (not (aref processedelements (+ row 1) column))) ;;; important that you put (< row (- griddimension 1)) as the first condition to be tested in the and because if it fails, we'll never encounter the subsequent aref with a negative index
           (setq elementstoprocess (append elementstoprocess (list (list (+ row 1) column)))) ;;; add this element to the queue of elements to be processed
        )
      )
      (cond
        ((and (> column 0) (eql (aref myarray row (- column 1)) bead) (not (aref processedelements row (- column 1)))) ;;; important that you put (> column 0) as the first condition to be tested in the and because if it fails, we'll never encounter the subsequent aref with a negative index
           (setq elementstoprocess (append elementstoprocess (list (list row (- column 1))))) ;;; add this element to the queue of elements to be processed
        )
      )
      (cond
        ((and (< column (- griddimension 1)) (eql (aref myarray row (+ column 1)) bead) (not (aref processedelements row (+ column 1))))  ;;; important that you put (< column (- griddimension 1)) as the first condition to be tested in the and because if it fails, we'll never encounter the subsequent aref with a negative index
           (setq elementstoprocess (append elementstoprocess (list (list row (+ column 1))))) ;;; add this element to the queue of elements to be processed
        )
      )
    )
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; create 2d array from given list
  ;;; Source: http://stackoverflow.com/a/9549738
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun list-to-2d-array (list)
    (make-array (list (length list)
                      (length (first list)))
                :initial-contents list)
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; convert string to list
  ;;; Source: http://faculty.hampshire.edu/lspector/courses/string-to-list.lisp
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun string-to-list (string)
    "Returns a list of the data items represented in the given list."
    (let ((the-list nil) ;; we'll build the list of data items here
          (end-marker (gensym))) ;; a unique value to designate "done"
      (loop (multiple-value-bind (returned-value end-position)
                                 (read-from-string string nil end-marker)
              (when (eq returned-value end-marker)
                (return the-list))
              ;; if not done, add the read thing to the list
              (setq the-list
                    (append the-list (list returned-value)))
              ;; and chop the read characters off of the string
              (setq string (subseq string end-position)))))
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Copy array
  ;;; Source: https://groups.google.com/d/msg/comp.lang.lisp/TA2OTkXEBzg/SCsbXjvZ8ssJ
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun copy-array (array)
   (let ((storage (copy-seq (linearize-array array))))
     (make-array (array-dimensions array) :displaced-to storage))
)

(
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  ;;; Used by copy-array
  ;;; Source: https://groups.google.com/d/msg/comp.lang.lisp/TA2OTkXEBzg/SCsbXjvZ8ssJ
  ;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  defun linearize-array (array)
   (make-array (array-total-size array) :displaced-to array)
)

;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
;;; End of File
;;; *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
