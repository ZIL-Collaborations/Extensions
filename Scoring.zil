"S C O R I N G  -  A ZILF Library Extension"

"ACTIONS"

"Disabling Score Update Notification (Extra Game Verb)"

<SYNTAX NOTIFY OFF OBJECT (FIND KLUDGEBIT) = V-NOTIFY-OFF>

<ROUTINE V-NOTIFY-OFF ()
    <TELL "Score notification off." CR>
    <SETG SCORE-UPDATE-NOTIFICATION-ON <>>
>

<SETG EXTRA-GAME-VERBS (!,EXTRA-GAME-VERBS NOTIFY-OFF)>

"Enabling Score Update Notification (Extra Game Verb)"

<SYNTAX NOTIFY ON OBJECT (FIND KLUDGEBIT) = V-NOTIFY-ON>
<SYNTAX NOTIFY = V-NOTIFY-ON>

<ROUTINE V-NOTIFY-ON ()
    <TELL "Score notification on." CR>
    <SETG SCORE-UPDATE-NOTIFICATION-ON T>
>

<SETG EXTRA-GAME-VERBS (!,EXTRA-GAME-VERBS NOTIFY-ON)>

"On Demand Score Notification (Extra Game Verb)"

<SYNTAX SCORE = V-SCORE>

<ROUTINE V-SCORE ()
    <TELL "You have so far scored " N ,SCORE ", ">
    <TELL "out of a possible " N ,MAX-SCORE ", ">
    <TELL "in " N ,MOVES " turns." CR>
>

<SETG EXTRA-GAME-VERBS (!,EXTRA-GAME-VERBS SCORE)>


"ACTIVITIES"

"Score Update Notification (Library Hook)"

<GLOBAL PREV-SCORE 0>
<GLOBAL SCORE-UPDATE-NOTIFICATION-ON T>

<ROUTINE NOTIFY-IF-SCORE-UPDATED ("AUX" UPDATE)
    <SET UPDATE <- ,SCORE ,PREV-SCORE>>
    <COND (<AND .UPDATE <NOT ,FINISH-CODE>>
        <COND (,SCORE-UPDATE-NOTIFICATION-ON
            <TELL CR "[Your score has gone">
            <COND (<G? .UPDATE 0>
                <TELL " up">)
            (ELSE
                <SET UPDATE <- .UPDATE>>
                <TELL " down">)>
            <TELL " by " N .UPDATE " point">
            <COND (<NOT <1? .UPDATE>>
                <TELL "s">)>
            <TELL ".]" CR>)>
        <SETG PREV-SCORE ,SCORE>)>
>

"Final Score Notification"

<GLOBAL MAX-SCORE 0>

<ROUTINE PRINT-FINAL-SCORE ()
    <TELL "In that game you scored " N ,SCORE ", ">
    <TELL "out of a possible " N ,MAX-SCORE ", ">
    <TELL "in " N ,MOVES " turns." CR>
>

<BIND ((REDEFINE T))
    <ROUTINE PRINT-GAME-OVER ()
        <CRLF>
        <PRINT-FINAL-SCORE>
    >
>
