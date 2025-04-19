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

@section{Stats}
Every character has a set of @deftech{stats}, which can either be modified, or be used to calculate and modify damage.
Additionally, characters can equip a weapon and multiple artifacts to further augment that stats they already have.

@subsection{Base stats}
A @deftech{base stat} is one of a characters main stat attributes. They consist of @racket[hp], @racket[def], @racket[atk], or @racket[em],
and represent a characters hit points, defense, attack, and elemental mastery respectively.

@subsection{Percent stats}
A @deftech{percent stat} is similar to a @tech{base stat}, but represent a percent scaling of the attribute, rather than the attribute itself.
They consist of @racket[hp%], @racket[def%], @racket[atk%], or @racket[em%].

@subsection{Flat stats}
A @deftech{flat stat} is similar to a @tech{base stat}, but does not have a @tech{percent stat} counterpart.
They consist of @racket[critr], @racket[critd], or @racket[dmg%], and represent a characters critical rate, critical damage, and percent damage bonus respectively. 

@section{Attributes}
An @deftech{attribute} is a calculation that depends on one or more @tech{stats}. There are multiple variations depending on the action that attribute is being used to perform.
@subsection{Buff attributes}
@(define buff-attr-syntax "buff attribute syntax")

A @deftech{buff attribute} indicates how to agument the stats of a character, and can be written in multiple ways.

@defform*[#:kind buff-attr-syntax
         ((buff attr value) (buff base-attr (sattr percent)))]{
 In the first usage, increases a @tech{base stat} @racket[attr] by @racket[value]. In the second usage, Increases a @tech{base stat} @racket[base-attr] by a percentage equal to (@racket[sattr] * (@racket[percentage]/100)), where @racket[sattr] is a @tech{percent stat}.
}

@subsection{Modifier attributes}

@subsection{Damage attributes}

@section{Attack keys}
Attack strings are composed of multiple different attacks, and each attack is represented with an @deftech{attack key}. Attack keys consist of @deftech{N}, @deftech{C},
@deftech{E}, @deftech{Q}, or @deftech{ND}, and represent a normal attack, charged attack, skill use, burst use, or a dash canceled normal attack respectively. It may also be a
@deftech{Swap}.
@(define attack-key-syntax "attack key syntax")
@defform[(Swap char-index)]{

 Performs a character swap, switching from the current character in the lineup to the character at index @racket[char-index] in the lineup. 

}


@section{Elements}

@section{Buffs}

@subsection{Trigger buffs}

@section{Triggers}

