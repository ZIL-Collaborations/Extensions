"WRITE | WRITE-IN | WRITE-ON"

;----------------------------------------------------------------

;"Define new FLAGS for WRITE"
<COND (<NOT <GASSIGNED? EXTRA-FLAGS>> <SETG EXTRA-FLAGS ()>)>
<SETG EXTRA-FLAGS (!,EXTRA-FLAGS WRITEBIT)>

;"Reinsert parser, even if it has been inserted before. 
  This to assure that the new FLAGS gets defined."
<BIND ((REDEFINE T)) INSERT-FILE "parser">

;"Define buffers for QUOTE"
<CONSTANT QUOTEBUF-SIZE 100>
<CONSTANT LEX2BUF-SIZE 59>

<GLOBAL QUOTEBUF <ITABLE NONE ,QUOTEBUF-SIZE (BYTE)>>
<GLOBAL LEX2BUF <ITABLE ,LEX2BUF-SIZE (LEXV) 0 #BYTE 0 #BYTE 0>>
<GLOBAL READBUF2 <ITABLE NONE ,QUOTEBUF-SIZE (BYTE)>>

;"Add token to TELL to print quotes"
<ADD-TELL-TOKENS
    Q *     <PRINT-QUOTE .X>
>
<ROUTINE PRINT-QUOTE (BUF "AUX" (I 1) (LEN <GETB .BUF 0>))
    <REPEAT ()
        <PRINTC <GETB .BUF .I>>
        <COND (<L? .I .LEN> <SET I <+ .I 1>>)
            (ELSE <RETURN>)>>>

;"Define SYNTAX for WRITE"
<SYNTAX WRITE IN OBJECT (FIND WRITEBIT) (HELD CARRIED ON-GROUND IN-ROOM) = V-WRITE PRE-WRITE>
<SYNTAX WRITE ON OBJECT (FIND WRITEBIT) (HELD CARRIED ON-GROUND IN-ROOM) = V-WRITE  PRE-WRITE>
<SYNTAX WRITE OBJECT (FIND WRITEBIT) (HELD CARRIED ON-GROUND IN-ROOM) = V-WRITE PRE-WRITE>

<ROUTINE PRE-WRITE ()
    <COND (<FSET? ,PRSO ,WRITEBIT>
        <COND (<0? <GETB ,QUOTEBUF 0>> 
            <TELL "What would you like to write on the " D ,PRSO "?" CR>
            <READQUOTE> <RFALSE>)>)>>

<ROUTINE V-WRITE ()
    <TELL "There's no good surface on the " D ,PRSO "." CR>>

;"Reads a string and stores it in QUOTEBUF. 
  READQUOTE leaves with 
    Byte 0 in QUOTEBUF = length of string
    Byte 1- = characters"
<ROUTINE READQUOTE ("AUX" (UCASE 100) W-CNT)
    <PRINTI "> ">
    <PUTB ,READBUF2 0 ,QUOTEBUF-SIZE>
    <PUTB ,READBUF2 1 0>
    <PUTB ,LEX2BUF 0 ,LEX2BUF-SIZE> ;"Max number of words to parse"
    <READ ,READBUF2 ,LEX2BUF>
    <COND (<==? <GETB ,READBUF2 1> 0>
            <TELL "I beg your pardon?" CR>
            <AGAIN>)>
    <SET W-CNT <GETB ,LEX2BUF 1>>
    <DO (I 1 .W-CNT)
        <COPY-WORD-TO-QUOTEBUF <+ .I .UCASE> ,READBUF2 ,LEX2BUF>
        <SET UCASE 0>
        <COND (<=? <GET ,LEX2BUF <- <* .I 2> 1>> W?PERIOD> <SET UCASE 100>)>>>

<ROUTINE COPYBUF (SRC DEST MAX)
    <REPEAT ((I 0) (LEN <+ <GETB .SRC 0> 1>))
        <PUTB .DEST .I <GETB .SRC .I>>
        <SET I <+ .I 1>>
        <COND (<EQUAL? .I .MAX .LEN> <RETURN>)>>>

;"If N>100 then N=N-100 and Uppercase"
<ROUTINE COPY-WORD-TO-QUOTEBUF (N RB LEX "AUX" I MAX (UCASE <>))
    <COND (<G? .N 100> <SET N <- .N 100>> <SET UCASE T>)>
    <SET I <GETB .LEX <+ <* .N 4> 1>>>
    <SET MAX <- <+ .I <GETB .LEX <* .N 4>>> 1>>
    ;"If buffer not empty add leading space"
    <COND (<NOT <OR <0? <GETB ,QUOTEBUF 0>>
                    <=? <GET .LEX <- <* .N 2> 1>> W?COMMA>
                    <=? <GET .LEX <- <* .N 2> 1>> W?PERIOD>>>
        <PUTB ,QUOTEBUF 0 <+ <GETB ,QUOTEBUF 0> 1>>
        <PUTB ,QUOTEBUF <GETB ,QUOTEBUF 0> !\ >)>
    <REPEAT (CNT C)
        <SET CNT <+ <GETB ,QUOTEBUF 0> 1>>
        <PUTB ,QUOTEBUF 0 .CNT>
        <SET C <GETB .RB .I>>
        <COND (<AND .UCASE <G=? .C !\a> <L=? .C !\z>> <SET C <- .C 32>>)>
        <SET UCASE <>>
        <PUTB ,QUOTEBUF .CNT .C>
        <AND <IGRTR? I .MAX> <RETURN>>>>

;"Redefine parser-routine to handle quotes"
<BIND ((REDEFINE T))
;"Reads and parses a command.
The primary outputs are PRSA, PRSO (+ PRSO-DIR), and PRSI, suitable
for passing to PERFORM.
If multiple objects are used for PRSO or PRSI, they will be set
to MANY-OBJECTS and PERFORM will have to read P-PRSOS or P-PRSIS.
Sets:
  P-LEN
  P-V
  P-NOBJ
  P-P1
  P-P2
  HERE
  PRSA
  PRSO
  PRSO-DIR
  P-PRSOS
  PRSI
  P-PRSIS
  P-BUTS
  P-EXTRA
  USAVE
  P-NP-DOBJ
  P-NP-IOBJ
  P-OOPS-DATA
  P-CONT
"
<ROUTINE PARSER ("AUX" NOBJ VAL DIR DIR-WN O-R KEEP OW OH OHL (QUOTE-FLAG <>) (QUOTE-UCASE 100))
    ;"Need to (re)initialize locals here since we use AGAIN"
    <SET OW ,WINNER>
    <SET OH ,HERE>
    <SET OHL ,HERE-LIT>
    <SET NOBJ <>>
    <SET VAL <>>
    <SET DIR <>>
    <SET DIR-WN <>>
    ;"Clear QUOTEBUF"
    <DO (I 0 ,QUOTEBUF-SIZE) <PUTB ,QUOTEBUF .I 0>>
    ;"Fill READBUF and LEXBUF"
    <COND (<L? ,P-CONT 0> <SETG P-CONT 0>)>
    <COND (,P-CONT
          <TRACE 1 "[PARSER: continuing from word " N ,P-CONT "]" CR>
          <ACTIVATE-BUFS "CONT">
          <COND (<1? ,P-CONT> <SETG P-CONT 0>)
                (<N=? ,MODE ,SUPERBRIEF>
                  ;"Print a blank line between multiple commands"
                  <COND (<NOT <VERB? TELL>> <CRLF>)>)>)
          (ELSE
          <TRACE 1 "[PARSER: fresh input]" CR>
          <RESET-WINNER>
          <SETG HERE <LOC ,WINNER>>
          <SETG HERE-LIT <SEARCH-FOR-LIGHT>>
          <READLINE T>)>

    <IF-DEBUG <SETG TRACE-INDENT 0>>
    <TRACE-DO 1 <DUMPBUFS> ;<DUMPLINE>>
    <TRACE-IN>

    <SETG P-LEN <GETB ,LEXBUF 1>>
    <COND (<0? ,P-LEN>
          <TELL "..." CR>
          <SETG P-CONT 0>
          <RFALSE>)>

    ;"Save undo state unless this looks like an undo command"
    <IF-UNDO
        <COND (<AND <G=? ,P-LEN 1>
                    <=? <GETWORD? 1> ,W?UNDO>
                    <OR <1? ,P-LEN>
                        <=? <GETWORD? 2> ,W?\. ,W?THEN>>>)
              (ELSE
              <TRACE 4 "[saving for UNDO]" CR>
              <BIND ((RES <ISAVE>))
                  <COND (<=? .RES 2>
                          <TELL "Previous turn undone." CR CR>
                          <SETG WINNER .OW>
                          <SETG HERE .OH>
                          <SETG HERE-LIT .OHL>
                          <V-LOOK>
                          <SETG P-CONT 0>
                          <AGAIN>)
                        (ELSE
                          <SETG USAVE .RES>)>>)>>

    <COND (<0? ,P-CONT>
          ;"Handle OOPS"
          <COND (<AND ,P-LEN <=? <GETWORD? 1> ,W?OOPS>>
                  <COND (<=? ,P-LEN 2>
                        <COND (<P-OOPS-WN>
                                <TRACE 2 "[handling OOPS]" CR>
                                <HANDLE-OOPS 2>
                                <SETG P-LEN <GETB ,LEXBUF 1>>
                                <TRACE-DO 1 <DUMPLINE>>)
                              (ELSE
                                <TELL "Nothing to correct." CR>
                                <RFALSE>)>)
                        (<=? ,P-LEN 1>
                        <TELL "It's OK." CR>
                        <RFALSE>)
                        (ELSE
                        <TELL "You can only correct one word at a time." CR>
                        <RFALSE>)>)>)>

    <SET KEEP 0>
    <P-OOPS-WN 0>
    <P-OOPS-CONT 0>
    <P-OOPS-O-REASON ,P-O-REASON>

    <COND (<0? ,P-CONT>
          ;"Save command in edit buffer for OOPS"
          <COND (<N=? ,READBUF ,EDIT-READBUF>
                  <COPY-TO-BUFS "EDIT">
                  <ACTIVATE-BUFS "EDIT">)>
          ;"Handle an orphan response, which may abort parsing or ask us to skip steps"
          <COND (<ORPHANING?>
                  <SET O-R <HANDLE-ORPHAN-RESPONSE>>
                  <COND (<N=? .O-R ,O-RES-NOT-HANDLED>
                        <SETG WINNER .OW>
                        <SETG HERE .OH>
                        <SETG HERE-LIT .OHL>)>
                  <COND (<=? .O-R ,O-RES-REORPHANED>
                        <TRACE-OUT>
                        <RFALSE>)
                        (<=? .O-R ,O-RES-FAILED>
                        <SETG P-O-REASON <>>
                        <TRACE-OUT>
                        <RFALSE>)
                        (<=? .O-R ,O-RES-SET-NP>
                        ;"TODO: Set the P-variables somewhere else? Shouldn't we fill in what
                          we know about the command-to-be when we ask the orphaning question, not
                          when we get the response?"
                        <SETG P-P1 <GETB ,P-SYNTAX ,SYN-PREP1>>
                        <COND (<ORPHANING-PRSI?>
                                <SETG P-P2 <GETB ,P-SYNTAX ,SYN-PREP2>>
                                <SETG P-NOBJ 2>
                                ;"Don't re-match P-NP-DOBJ when we've just orphaned PRSI. Use the saved
                                  match results. There won't be a NP to match if we GWIMmed PRSO."
                                <SET KEEP 1>)
                              (ELSE <SETG P-NOBJ 1>)>)
                        (<=? .O-R ,O-RES-SET-PRSTBL>
                        <COND (<ORPHANING-PRSI?> <SET KEEP 2>)
                              (ELSE <SET KEEP 1>)>)>
                  <SETG P-O-REASON <>>)>
          ;"If we aren't handling this command as an orphan response, convert it if needed
            and copy it to CONT bufs"
          <COND (<NOT .O-R>
                  ;"Translate order syntax (HAL, OPEN THE POD BAY DOOR or
                    TELL HAL TO OPEN THE POD BAY DOOR) into multi-command syntax
                    (\,TELL HAL THEN OPEN THE POD BAY DOOR)."
                  <COND (<CONVERT-ORDER-TO-TELL?>
                        <SETG P-LEN <GETB ,LEXBUF 1>>)>)>)>

    ;"Identify parts of speech, parse noun phrases"
    <COND (<N=? .O-R ,O-RES-SET-NP ,O-RES-SET-PRSTBL>
          <SETG P-V <>>
          <SETG P-NOBJ 0>
          <CLEAR-NOUN-PHRASE ,P-NP-DOBJ>
          <CLEAR-NOUN-PHRASE ,P-NP-IOBJ>
          <SETG P-P1 <>>
          <SETG P-P2 <>>
          ;"Identify the verb, prepositions, and noun phrases"
          <REPEAT ((I <OR ,P-CONT 1>) W V)
              <COND (<G? .I ,P-LEN>
                      ;"Reached the end of the command"
                      <SETG P-CONT 0>
                      <RETURN>)
                    (<=? <GETWORD? .I> W?QUOTE> 
                      <COND (.QUOTE-FLAG <SET QUOTE-FLAG <>>) (T <SET QUOTE-FLAG T>)>)
                    (.QUOTE-FLAG
                      ;"Copy word to QUOTE-BUFFER"
                      <COPY-WORD-TO-QUOTEBUF <+ .I .QUOTE-UCASE> ,READBUF ,LEXBUF>
                      <SET QUOTE-UCASE 0>
                      <COND (<=? <GETWORD? .I> W?PERIOD> <SET QUOTE-UCASE 100>)>)
                    (<NOT <OR <SET W <GETWORD? .I>>
                              <AND <PARSE-NUMBER? .I> <SET W ,W?\,NUMBER>>>>
                      ;"Word not in vocabulary"
                      <STORE-OOPS .I>
                      <SETG P-CONT 0>
                      <TELL "I don't know the word \"" WORD .I "\"." CR>
                      <RFALSE>)
                    (<=? .W ,W?THEN ,W?\.>
                      ;"End of command, maybe start of a new one"
                      <TRACE 3 "['then' word " N .I "]" CR>
                      <SETG P-CONT <+ .I 1>>
                      <COND (<G? ,P-CONT ,P-LEN> <SETG P-CONT 0>)
                            (ELSE <COPY-TO-BUFS "CONT">)>
                      <RETURN>)
                    (<AND <NOT ,P-V>
                          <SET V <WORD? .W VERB>>
                          <OR <NOT .DIR> <=? .V ,ACT?WALK>>>
                      ;"Found the verb"
                      <SETG P-V-WORD .W>
                      <SETG P-V-WORDN .I>
                      <SETG P-V .V>
                      <TRACE 3 "[verb word " N ,P-V-WORDN " '" B ,P-V-WORD "' = " N ,P-V "]" CR>)
                    (<AND <NOT .DIR>
                          <EQUAL? ,P-V <> ,ACT?WALK>
                          <SET VAL <WORD? .W DIRECTION>>>
                      ;"Found a direction"
                      <SET DIR .VAL>
                      <SET DIR-WN .I>
                      <TRACE 3 "[got a direction]" CR>)
                    (<SET VAL <CHKWORD? .W ,PS?PREPOSITION 0>>
                      ;"Found a preposition"
                      ;"Only keep the first preposition for each object"
                      <COND (<AND <==? .NOBJ 0> <NOT ,P-P1>>
                            <TRACE 3 "[P1 word " N .I " '" B .W "' = " N .VAL "]" CR>
                            <SETG P-P1 .VAL>)
                            (<AND <==? .NOBJ 1> <NOT ,P-P2>>
                            <TRACE 3 "[P2 word " N .I " '" B .W "' = " N .VAL "]" CR>
                            <SETG P-P2 .VAL>)>)
                    (<STARTS-NOUN-PHRASE? .W>
                      ;"Found a noun phrase"
                      <SET NOBJ <+ .NOBJ 1>>
                      <TRACE 3 "[NP start word " N .I ", now NOBJ=" N .NOBJ "]" CR>
                      <TRACE-IN>
                      <COND (<==? .NOBJ 1>
                            ;"If we found a direction earlier, try it as a preposition instead"
                            ;"This fixes GO IN BUILDING (vs. GO IN)"
                            <COND (<AND .DIR
                                        ,P-V
                                        <NOT ,P-P1>
                                        <SET V <GETWORD? .DIR-WN>>
                                        <SET VAL <CHKWORD? .V ,PS?PREPOSITION 0>>>
                                    <TRACE 3 "[revising direction word " N .DIR-WN
                                            " as P1: '" B .V "' = " N .VAL "]" CR>
                                    <SETG P-P1 .VAL>
                                    <SET DIR <>>
                                    <SET DIR-WN <>>)>
                            <SET VAL <PARSE-NOUN-PHRASE .I ,P-NP-DOBJ>>)
                            (<==? .NOBJ 2>
                            <SET VAL <PARSE-NOUN-PHRASE .I ,P-NP-IOBJ>>)
                            (ELSE
                            <SETG P-CONT 0>
                            <TELL "That sentence has too many objects." CR>
                            <RFALSE>)>
                      <TRACE 3 "[PARSE-NOUN-PHRASE returned " N .VAL "]" CR>
                      <TRACE-OUT>
                      <COND (.VAL
                            <SET I .VAL>
                            <AGAIN>)
                            (ELSE
                            <SETG P-CONT 0>
                            <RFALSE>)>)
                    (ELSE
                      ;"Unexpected word type"
                      <STORE-OOPS .I>
                      <SETG P-CONT 0>
                      <TELL "I didn't expect the word \"" WORD .I "\" there." CR>
                      <TRACE-OUT>
                      <RFALSE>)>
              <SET I <+ .I 1>>>

          <SETG P-NOBJ .NOBJ>

          <TRACE-OUT>
          <TRACE 1 "[sentence: V=" MATCHING-WORD ,P-V ,PS?VERB ,P1?VERB "(" N ,P-V ") NOBJ=" N ,P-NOBJ
                " P1=" MATCHING-WORD ,P-P1 ,PS?PREPOSITION 0 "(" N ,P-P1
                ") DOBJS=+" N <NP-YCNT ,P-NP-DOBJ> "-" N <NP-NCNT ,P-NP-DOBJ>
                " P2=" MATCHING-WORD ,P-P2 ,PS?PREPOSITION 0 "(" N ,P-P2
                ") IOBJS=+" N <NP-YCNT ,P-NP-IOBJ> "-" N <NP-NCNT ,P-NP-IOBJ> "]" CR>
          <TRACE-IN>

          ;"If we have a direction and nothing else except maybe a WALK verb, it's
            a movement command."
          <COND (<AND .DIR
                      <EQUAL? ,P-V <> ,ACT?WALK>
                      <0? .NOBJ>
                      <NOT ,P-P1>
                      <NOT ,P-P2>>
                  <SETG PRSO-DIR T>
                  <SETG PRSA ,V?WALK>
                  <SETG PRSO .DIR>
                  <SETG PRSI <>>
                  <COND (<NOT <VERB? AGAIN>>
                        <TRACE 4 "[saving for AGAIN]" CR>
                        <SAVE-PARSER-RESULT ,AGAIN-STORAGE>)>
                  <TRACE-OUT>
                  <RTRUE>)>
          ;"Otherwise, a verb is required and a direction is forbidden."
          <COND (<NOT ,P-V>
                  <SETG P-CONT 0>
                  <TELL "That sentence has no verb." CR>
                  <TRACE-OUT>
                  <RFALSE>)
                (.DIR
                  <STORE-OOPS .DIR-WN>
                  <SETG P-CONT 0>
                  <TELL "I don't understand what \"" WORD .DIR-WN "\" is doing in that sentence." CR>
                  <TRACE-OUT>
                  <RFALSE>)>
          <SETG PRSO-DIR <>>)>
    ;"Match syntax lines and objects"
    <COND (<NOT .O-R>
          <TRACE 2 "[matching syntax and finding objects, KEEP=" N .KEEP "]" CR>
          <COND (<NOT <AND <MATCH-SYNTAX> <FIND-OBJECTS .KEEP>>>
                  <TRACE-OUT>
                  <SETG P-CONT 0>
                  <RFALSE>)>)
          (<L? .KEEP 2>
          ;"We already found a syntax line last time, but we need FIND-OBJECTS to
            match at least one noun phrase."
          <TRACE 2 "[only finding objects, KEEP=" N .KEEP "]" CR>
          <COND (<NOT <FIND-OBJECTS .KEEP>>
                  <TRACE-OUT>
                  <SETG P-CONT 0>
                  <RFALSE>)>)>
    ;"Save command for AGAIN"
    <COND (<NOT <VERB? AGAIN>>
          <TRACE 4 "[saving for AGAIN]" CR>
          <SAVE-PARSER-RESULT ,AGAIN-STORAGE>)>
    ;"If successful PRSO, back up PRSO for IT"
    <SET-PRONOUNS ,PRSO ,P-PRSOS>
    <TRACE-OUT>
    <RTRUE>>
    >
    
