"F I N I S H I N G  -  A ZILF Library Extension"

"ACTIVITIES"

"Finishing the Game (Library Hook)"

"This was created to avoid calling the ZILF library routine JIGS-UP from action
routines. This is because doing so means that the final turn never completes,
causing the number of turns shown in the final score notification to be one less
than the true number."

<GLOBAL FINISH-CODE 0>

<ROUTINE FINISH-IF-CODE-SET ()
    <COND (,FINISH-CODE
        <COND (<1? ,FINISH-CODE>
            <PRINT-WIN-MESSAGE>)
        (ELSE
            <PRINT-LOSE-MESSAGE>)>
    )>
>

<ROUTINE PRINT-WIN-MESSAGE ()
    <JIGS-UP "    *** You have won ***">
>

<ROUTINE PRINT-LOSE-MESSAGE ()
    <JIGS-UP "    *** You have lost ***">
>
