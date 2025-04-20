#lang scribble/manual 

@(require (for-label racket ELEMENTS))

@title{ELEMENTS: A DSL for optimizing and calculating Genshin Impact damage rotations}

@defmodule[ELEMENTS]


@defform[(genshin-calc expr ...)]{

Used to compile every @deftech{expr} within it. Required for proper error checking. 

}

@defform[(define-weapon name damage attr buffs ...)]{

Defines a weapon with a damage number that buffs a characters stats based on the given @tech{modifier attribute} @racket[attr]. The weapon also has a list of @tech{buffs}.

}

@defform[(define-skill name
           #:cooldown cd
           #:attr attr
           #:duration duration
           #:type type
           buffs ...)]{

Defines a character skill/burst that has a cooldown, duration, and a list of @tech{buffs}. The skill also deals damage based on the given @tech{damage attribute} @racket[attr].
                                                                                                                   

}

@defform[(define-attack-sequence name
           ([attr duration type] ...
            #:charged  [attr2 duration2 type2]
            #:plunging [attr3 duration3 type3]))]{

 Defines a character's attack sequence. Each attack has its own unique duration and an @tech{element} @racket[type]. Attacks deal damage
 based on the given @tech{damage attribute} @racket[attr].                                                                                     
 An attack sequence also includes a charged and plunging attack, with their own corresponding duration, damage, and element type.                                                                                                

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

@section{Buffs}
@(define buff-syntax "buff syntax")

A @deftech{buff} augments characters when active, changing the values of a character's @tech{stats} by a specified amount. A buff can either be a @racket[unconditional-buff], or a @tech{trigger-buff}.
@defform[#:kind buff-syntax (unconditional-buff [name #:effect attr
                                                       #:party-wide party-wide])
         #:contracts ([attr buff-attribute?]
                      [party-wide boolean?])]{

 An unconditional buff that always applies the attribute given by @racket[attr]. If @racket[party-wide] is @racket[#t], this buff will also apply to all characters in the lineup,
 not just the character the buff is linked to.

}

@subsection{Trigger buffs}
@(define trigger-buff-syntax "trigger buff syntax")
A @deftech{trigger buff} augments characters, but unlike conditionals, is not always active. A trigger buff can be either a @racket[triggered-buff] or an @racket[applied-buff].

@defform[#:kind trigger-buff-syntax
         (triggered-buff [name #:effect attr
                                #:trigger t
                                #:limit limit
                                #:party-wide party-wide
                                #:duration duration])
         #:contracts ([attr buff-attribute?]
                      [t trigger?]
                      [limit integer?]
                      [party-wide boolean?]
                      [duration positive?])]{

 A triggered buff that applies the attribute given by @racket[attr], when a @tech{trigger} @racket[t] is performed by the character. The buff lasts for the specified time given by @racket[duration]
 before going inactive. Each triggered buff can stack up until the limit specified by @racket[limit], at which point any more activations of the buff will refresh the it at the max limit.
 If @racket[party-wide] is @racket[#t], this buff will also apply to all characters in the lineup, not just the character the buff is linked to.                                                     
}

@defform[#:kind trigger-buff-syntax
         (applied-buff [name #:effect attr
                              #:limit limit
                              #:party-wide party-wide
                              #:duration duration])
         #:contracts ([attr buff-attribute?]
                      [limit integer?]
                      [party-wide boolean?]
                      [duration positive?])]{

 An applied buff that operates similarly to a @racket[triggered-buff], except without a specific @tech{trigger}. Once activated, an applied buff applies the attribute given by @racket[attr]. The buff lasts for the specified time given by @racket[duration]
 before going inactive. Each triggered buff can stack up until the limit specified by @racket[limit], at which point any more activations of the buff will refresh the it at the max limit.
 If @racket[party-wide] is @racket[#t], this buff will also apply to all characters in the lineup, not just the character the buff is linked to.                                                      
}

@section{Stats}
Every character has a set of @deftech{stats}, which can either be modified, or be used to calculate and modify damage.
Additionally, characters can equip a weapon and multiple artifacts to further augment that stats they already have.

@subsection{Base stats}
A @deftech{base stat} is one of a characters main stat attributes. They consist of @racket[hp], @racket[def], @racket[atk], or @racket[em],
and represent a characters hit points, defense, attack, and elemental mastery respectively.

@subsection{Percent stats}
A @deftech{percent stat} a percent scaling of a @tech{base stat}, rather than the base stat itself.
They consist of @racket[hp%], @racket[def%], @racket[atk%], or @racket[em%].

@subsection{Flat stats}
A @deftech{flat stat} is similar to a @tech{base stat}, but does not have a @tech{percent stat} counterpart.
They consist of @racket[critr], @racket[critd], or @racket[dmg%], and represent a characters critical rate, critical damage, and percent damage bonus respectively. 

@section{Attributes}
An @deftech{attribute} is a calculation that depends on one or more @tech{stats}. There are multiple variations depending on the action that attribute is being used to perform.
@subsection{Buff attributes}
@(define buff-attr-syntax "buff attribute syntax")

A @deftech{buff attribute} indicates how a buff aguments the stats of a character, and can be written in multiple ways.

@defform*[#:kind buff-attr-syntax
         ((buff attr value) (buff base-attr (sattr percent)))]{
 In the first usage, increases a @tech{base stat} @racket[attr] by @racket[value].
 In the second usage, increases a @tech{base stat} @racket[base-attr] by a percentage equal to (@racket[sattr] * (@racket[percentage]/100)),
 where @racket[sattr] is a @tech{percent stat}.
}

@subsection{Modifier attributes}
@(define mod-attr-syntax "modifier attribute syntax")

A @deftech{modifier attribute} indicates how to agument the stats of a character. However, it is more restrictive compared to a @tech{buff attribute}, as it
does not allow stat increases to be dependent on other @tech{stats}.

@defform[#:kind mod-attr-syntax
         (mod attr value)]{
 Increases a @tech{stat} @racket[attr] by @racket[value]. 
}
@subsection{Damage attributes}
@(define dmg-attr-syntax "damage attribute syntax")

A @deftech{damage attribute} indicates what @tech{stat} to scale an instance of damage off of.

@defform[#:kind dmg-attr-syntax
         (dmg attr percent)]{
 Increases a @tech{percent stat} @racket[attr] by a percentage equal to (@racket[percent]/100).
}

@section{Attack keys}
Attack strings are composed of multiple different attacks, and each attack is represented with an @deftech{attack key}. Attack keys consist of @racketidfont{N}, @racketidfont{C},
@racketidfont{E}, @racketidfont{Q}, or @racketidfont{ND}, and represent a normal attack, charged attack, skill use, burst use, or a dash canceled normal attack respectively. It may also be a
@racket[Swap].
@(define attack-key-syntax "attack key syntax")
@defform[(Swap char-index)]{

 Performs a character swap, switching from the current character in the lineup to the character at index @racket[char-index] in the lineup. 

}


@section{Elements}
All attacks and damage instances have an @deftech{element} type associated with them. These elements mainly serve to amplify damage in the event one or more elements is applied to an enemy at a time.
An element can be @racketidfont{pyro}, @racketidfont{hydro}, @racketidfont{cryo}, @racketidfont{electro}, @racketidfont{geo}, @racketidfont{anemo}, @racketidfont{dendro}, or @racketidfont{physical},
each representing their respective in-game element. 

@section{Triggers}
A @deftech{trigger} is an action performed by a character that may activate a @racket[triggered-buff]. If a trigger matches the trigger of a trigger-buff, then the buff will activate.
Trigger actions include @racketidfont{normal-attack}, @racketidfont{charged-attack}, @racketidfont{skill},
or @racketidfont{burst}, which represent the character doing a normal attack, charged attack, skill activation, or burst activation respectively. 

