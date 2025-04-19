#lang scribble/manual 

@(require (for-label racket ELEMENTS))

@title{ELEMENTS: A DSL for optimizing and calculating Genshin Impact damage rotations}

@defmodule[ELEMENTS]


@defform[(genshin-calc expr ...)]{

Used to compile every @deftech{expr} within it. Required for proper error checking. 

}

@defform[(define-weapon name damage attr buffs ...)]{

Temp Description

}

@defform[(define-skill name:skill-bind
           #:cooldown cd
           #:attr attr
           #:duration duration
           #:type type
           buffs ...)]{

Temp Description

}

@defform[(define-attack-sequence name
           ([attr duration type] ...
            #:charged  [attr2 duration2 type2]
            #:plunging [attr3 duration3 type3]))]{

 Temp Description

}

@defform[(define-artifact name
           set-name
           main-attr
           sub-attr ...)]{

 Temp Description

}

@defform[(define-character name
           #:hp hp #:def def #:atk atk
           #:em em #:critr critr #:critd critd
           #:attacks attacks #:weapon weapon #:skill skill
           #:burst burst #:artifacts artifacts ...)]{

 Temp Description

}

@defform[(define-enemy name
           #:def def
           #:res (#:pyro pyro
                  #:hydro hydro
                  #:electro electro
                  #:cryo cryo
                  #:geo geo
                  #:anemo anemo
                  #:dendro dendro
                  #:physical physical)
           #:reduction reduction)]{

 Temp Description

}

@defform[(define-team-lineup name (chars ...))]{

 Temp Description

}

@defform[(calculate-rotation-damage lineup enemy (attk ...))]{

 Temp Description

}

@defform[(calculate-raw-rotation-damage lineup enemy (attk ...))]{

 Temp Description

}

@section{Types}

@section{Types}



