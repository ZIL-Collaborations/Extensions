"T E S T"

"THE PROJECT"

"Each ZILF game project must have a name, a description, a release number and a
z-machine version"

<CONSTANT GAME-TITLE "Test">
<CONSTANT GAME-DESCRIPTION
"A ZILF game test of WRITE-ON-IN by Alex Proudfoot & Henrik Åsman">

<CONSTANT RELEASEID 1> <VERSION ZIP>


"THE MECHANICS"

"Game mechanics are mostly provided by the ZILF Library but can be modified or
enhanced with custom interaction and behaviour which may also be reused as
library extensions."

"Startup"

<ROUTINE GO ()
    <SET-THE-SCENE>
    <INTRODUCE-THE-GAME>
    <MAIN-LOOP>
>

"Library and Extensions"

<INSERT-FILE "parser">
;<INSERT-FILE "../Extensions/Scoring">
;<INSERT-FILE "../Extensions/Finishing">
<INSERT-FILE "write-on">

"Game Specific Interaction"

"This consists of a collection of actions. It is not always necessary to define
 actions but there may, in principle, be as many as are required to support the
 game."

"Actions are constructed in ZIL from verb routines and their associated syntax
 definitions."

"Note: as at ZILF 0.9, there is no mechanism to replace syntax definitions in
 the ZILF library other than by editing a copy of verbs.zil. See ZILF Issue #42"

"Game Specific Behaviour"

"This consists of a set of activities. It is not always necessary to define
 activities but there may, in principle, be as many as are required to support
 the game."

"Activities are constructed in ZIL from utility routines, macros and global
 variables."

"Extra Game Verbs"

;<SETG EXTRA-GAME-VERBS '(NOTIFY-OFF NOTIFY-ON SCORE)>

"Library Hooks"

;<BIND ((REDEFINE T))
    <DEFMAC HOOK-END-OF-COMMAND ()
       '<BIND ()
            <NOTIFY-IF-SCORE-UPDATED> ;"Scoring"
            <FINISH-IF-CODE-SET>      ;"Finishing"
        >
    >
>


"THE GAME"

"ZILF games are a form of Interactive Fiction. They can be thought of as telling
 a story in which the player can take part. Expressed in the language of
 storytelling, each game has a cast, a setting, and a plot."

"Also, each game starts with setting the scene and an introduction."

"Setting the Scene"

<ROUTINE SET-THE-SCENE ()
    <SETG HERE ,LAIR>
    <MOVE PLAYER ,HERE>
    ;<SETG MAX-SCORE 0>
>

<>
"Introduction"

<ROUTINE INTRODUCE-THE-GAME ()
    <TELL CR CR "This game starts in the dark ..." CR>
    <CRLF> <V-VERSION>
    <CRLF> <V-LOOK>
>

"Cast"

"This consists of a collection of characters. There must be at least one
character in order to represent the player's role in the game. The ZILF Library
provides a standard player character which may be used by any game. A game may
have more characters and may even have alternate player characters. Each
character can carry property and wear clothing."

"Characters, their property and clothing are defined in ZIL as objects."

"Setting"

"This consists of a collection of locations. There must be at least one location
in order to represent what the player sees at the start of the game. A game may
have more locations connected to the first in which case the player can explore
the setting by moving from location to location. Each location can have scenery,
which may be shared with other locations, and items, which the player may be
able to take."

"Locations are defined in ZIL as rooms while their scenery and items are defined
in ZIL as objects."

<ROOM DARKNESS (DESC "In the Dark") (IN ROOMS)
    (LDESC "It's scary. You can't see anything ...")
>

<ROOM LAIR
    (DESC "Wizard's Lair")
    (IN ROOMS)
    (LDESC "You are in the Wizard's lair.")
    (FLAGS LIGHTBIT)>

<OBJECT BOOK
    (LOC LAIR)
    (SYNONYM BOOK)
    (ADJECTIVE OLD)
    (DESC "old book")
    (ACTION BOOK-F)
    (BOOKTEXT <ITABLE NONE ,QUOTEBUF-SIZE (BYTE)>)
    (FLAGS TAKEBIT VOWELBIT WRITEBIT)
>

<ROUTINE BOOK-F ()
    <COND (<VERB? WRITE>
        <TELL "You write the following, " Q ,QUOTEBUF ", in the " D ,PRSO "." CR>
        <COPYBUF ,QUOTEBUF <GETP ,PRSO ,P?BOOKTEXT> ,QUOTEBUF-SIZE>
        <RTRUE>)>
    <COND (<VERB? READ>
        <TELL "\"" Q <GETP ,PRSO ,P?BOOKTEXT> "\" is written in the " D ,PRSO "." CR>
        <RTRUE>)>>   

<OBJECT SPHERE
    (LOC LAIR)
    (SYNONYM SPHERE)
    (ADJECTIVE BLUE)
    (DESC "blue sphere")
    (FLAGS TAKEBIT)
>

"Plot"

"This consists of a collection of scenes. Each scene consists of a goal and a
puzzle or task to be completed to succeed or fail the goal. Some scenes complete
on success or failure, making it impossible to try again, while others complete
on success only. There must be at least one scene in order to represent the goal
of finishing the game, otherwise the quit command will be necessary. A game may
have more scenes, consecutive or concurrent with the first. It may also be
useful to regard exploration of the setting as a notional concurrent scene."

"Characters take part in scenes through conversation, persuasion, combat or
competition while locations take part in scenes through exploration,
investigation, observation or manipulation."

"Scenes are constructed in ZIL from action routines, interrupt routines and
global variables."
