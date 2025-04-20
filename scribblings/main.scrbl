#lang scribble/manual 

@(require scribble/example (for-syntax racket/base) (for-label racket elements))

@; Create an evaluator to use for examples blocks with the DSL required.
@(define eval (make-base-eval '(require racket elements)))

@; A helper for creating references to forms defined in a binding space.
@(define-syntax (racket/space stx)
   (syntax-case stx ()
     [(_ id space)
      #'(racket #,((make-interned-syntax-introducer (syntax-e #'space)) #'id))]))

@title{Elements: A DSL for optimizing and calculating Genshin Impact damage rotations}
@author+email["Joshua Goldberg" "goldberg.josh@northeastern.edu"]@linebreak[]
@author+email["Will Du" "du.wi@northeastern.edu"]

@defmodule[elements]
@link["https://genshin.hoyoverse.com/en/"]{Genshin Impact} is an open world RPG released in 2020. In Genshin, you control a team of up to four characters, and engage in various levels of combat.
The goal is typically to clear content as fast as possible, which requires efficient team rotations with high levels of sustained damage.
This package implements a damage calculator as a hosted DSL. With @tt{Elements}, you can easily define the basics of a team lineup, including weapons, elemental skills, and characters.
From there, you can setup damage rotations against enemies of varying toughness, and calculate potential damage output. Additionally, @tt{Elements} compares old rotations to newer ones,
allowing you to easily keep track of what combination has provided the highest sustained damage. 

@defform[(genshin-calc expr ...)]{

 Used to compile every @deftech{expr} within it. Required for proper compile-time error checking. 

}

@defform[(define-weapon name atk attr buffs ...)
         #:contracts ([name identifier?]
                      [atk number?]
                      [attr modifier-attribute?]
                      [buffs buff?])]{

 Defines a weapon with a base attack number that buffs a characters stats based on the given @tech{modifier attribute} @racket[attr]. The weapon also has a list of @tech{buffs}.

}
@examples[#:eval eval #:label #f
          (define-weapon wolfs-gravestone
            550
            (mod critr 24.1)
            (triggered-buff
             [dmgup
              #:effect (buff atk% 20.0)
              #:trigger normal-attack
              #:limit 1
              #:party-wide #f
              #:duration 10.0])
            (unconditional-buff
             [crit-up
              #:effect (buff critd 20)
              #:party-wide #f])
            )]

@defform[(define-skill name
           #:cooldown cd
           #:attr attr
           #:duration duration
           #:type type
           buffs ...)
         #:contracts ([name identifier?]
                      [cd number?]
                      [attr dmg-attribute?]
                      [duration number?]
                      [type element?]
                      [buffs buff?])]{

 Defines a character skill/burst that has a cooldown, duration, and a list of @tech{buffs}. The skill also deals damage based on the given @tech{damage attribute} @racket[attr].
                                                                                                                   

}
@examples[#:eval eval #:label #f
          (define-skill all-attack-up
            #:cooldown 25.0
            #:attr (dmg atk% 125)
            #:duration 0.1
            #:type pyro
            (applied-buff
             [skill-atkup
              #:effect (buff atk 125)
              #:limit 1
              #:party-wide #t
              #:duration 10])
            )

          (define-skill basic-slash
            #:cooldown 5.0
            #:attr (dmg atk% 25)
            #:duration 1.0
            #:type pyro
            (applied-buff
             [skill-hpup
              #:effect (buff hp 20)
              #:limit 1
              #:party-wide #f
              #:duration 10.0])
            )]

@defform[(define-attack-sequence name
           ([attr duration type] ...
            #:charged  [attr2 duration2 type2]
            #:plunging [attr3 duration3 type3]))
         #:contracts ([name identifier?]
                      [attr dmg-attribute?]
                      [duration number?]
                      [type element?]
                      [attr2 dmg-attribute?]
                      [duration2 number?]
                      [type2 element?]
                      [attr3 dmg-attribute?]
                      [duration3 number?]
                      [type3 element?])]{

 Defines a character's attack sequence. Each attack has its own unique duration and an @tech{element} @racket[type]. Attacks deal damage
 based on the given @tech{damage attribute} @racket[attr].                                                                                     
 An attack sequence also includes a charged and plunging attack, with their own corresponding duration, damage, and element type.                                                                                                

}

@examples[#:eval eval #:label #f
          (define-attack-sequence attack-chain
            ([(dmg atk% 10) 0.5 physical]
             [(dmg atk% 25) 0.2 physical]
             [(dmg atk% 125) 0.8 physical]
             [(dmg atk% 250) 1.5 physical]
             #:charged [(dmg hp% 5) 3.5 pyro]
             #:plunging [(dmg hp% 10) 3.5 physical]))]

@defform[(define-artifact name
           set-name
           main-attr
           sub-attr ...)
         #:contracts ([name identifier?]
                      [set-name string?]
                      [main-attr modifier-attribute?]
                      [sub-attr modifier-attribute?])]{

 Defines an artifact piece. Artifacts belong to a set, specified by the string provided by @racket[set-name]. Each artifact also augments a character's @tech{stats} with a
 @tech{modifier attribute} @racket[main-attr], as well as a list of more @tech{modifier attribute} sub attributes.                                                                                        

}

@examples[#:eval eval #:label #f
          (define-artifact test-feather
            "cool feather collection"
            (mod atk 325)
            (mod atk 27)
            (mod em 42)
            )

          (define-artifact test-goblet
            "cool goblet collection" 
            (mod critr 46.6) 
            (mod critd 16.2) 
            (mod critr 3.0)
            (mod def 128)
            )]
 

@defform[(define-character name
           #:hp hp #:def def #:atk atk
           #:em em #:critr critr #:critd critd
           #:attacks attacks #:weapon weapon #:skill skill
           #:burst burst #:artifacts artifacts ...)
         #:contracts ([name identifier?]
                      [hp number?]
                      [def number?]
                      [atk number?]
                      [em number?]
                      [critr number?]
                      [critd number?]
                      [attacks identifier?]
                      [weapon identifier?]
                      [skill identifier?]
                      [burst identifier?]
                      [artifacts identifier?])]{

 Defines a character that has a list of @tech{base stats} numbers, and the name of an attack string, weapon, skill, burst, and list of artifacts.
 Names must be defined with their respective "define-" type in order to be properly assigned to a character.

}

@examples[#:eval eval #:label #f
          (define-character test-char
            #:hp 12000
            #:def 500
            #:atk 900
            #:em 20 
            #:critr 5
            #:critd 50
            #:attacks attack-chain
            #:weapon wolfs-gravestone
            #:skill basic-slash
            #:burst all-attack-up
            #:artifacts test-feather
            test-goblet
            )

          (define-character test-char2
            #:hp 9000
            #:def 5000
            #:atk 200
            #:em 10
            #:critr 80 
            #:critd 300 
            #:attacks attack-chain
            #:weapon wolfs-gravestone 
            #:skill basic-slash 
            #:burst all-attack-up 
            #:artifacts test-feather
            )]

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
           #:reduction reduction)
         #:contracts ([name identifier?]
                      [pyro number?]
                      [hydro number?]
                      [electro number?]
                      [geo number?]
                      [anemo number?]
                      [dendro number?]
                      [physical number?]
                      [reduction number?])]{

 Defines an enemy that has numbers representing their defense, resistances to @tech{elements}, and overall damage reduction.

}

@examples[#:eval eval #:label #f
          (define-enemy dummy
            #:def 1000
            #:res (#:pyro 50
                   #:hydro 10
                   #:electro 10
                   #:cryo 10
                   #:geo 10
                   #:anemo 10
                   #:dendro 10
                   #:physical -20)
            #:reduction 5
            )]

@defform[(define-team-lineup name (chars ...))
         #:contracts ([name identifier?]
                      [chars identifier?])]{

 Defines a team lineup that consists of a list of characters, which is used to calculate damage rotations.

}

@examples[#:eval eval #:label #f
          (define-team-lineup lone-member (test-char))
          (define-team-lineup two-members (test-char test-char2))]

@defform[(calculate-rotation-damage lineup enemy (attk ...))
         #:contracts ([lineup identifier?]
                      [enemy identifier?]
                      [attk attack-key?])]{

 Calculates and displays the amount of damage done by a team lineup against a specified enemy, given a list of @tech{attack keys} specifying character actions. Also
 compares the result to previous entries with the same lineup and enemy, and displays the best saved calculation.

}

@examples[#:eval eval #:label #f
          (calculate-rotation-damage lone-member dummy (N C C C N N N N N N ND C))
          (calculate-rotation-damage two-members dummy (E Q N N N N (Swap 1) N N N ND))]

@defform[(calculate-raw-rotation-damage lineup enemy (attk ...))
         #:contracts ([lineup identifier?]
                      [enemy identifier?]
                      [attk attack-key?])]{

 Calculates and displays the unformatted amount of damage done by a team lineup against a specified enemy, given a list of @tech{attack keys} specifying character actions.

}

@examples[#:eval eval #:label #f
          (calculate-raw-rotation-damage lone-member dummy (N C C C N N N N N N ND C))
          (calculate-raw-rotation-damage two-members dummy (E Q N N N N (Swap 1) N N N ND))]

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
                      [duration number?])]{

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
                      [duration number?])]{

 An applied buff that operates similarly to a @racket[triggered-buff], except without a specific @tech{trigger}. Once activated, an applied buff applies the attribute given by @racket[attr]. The buff lasts for the specified time given by @racket[duration]
 before going inactive. Each triggered buff can stack up until the limit specified by @racket[limit], at which point any more activations of the buff will refresh the it at the max limit.
 If @racket[party-wide] is @racket[#t], this buff will also apply to all characters in the lineup, not just the character the buff is linked to.                                                      
}

@section{Stats}
Every character has a set of stats, which can either be modified, or be used to calculate and modify damage.
Additionally, characters can equip a weapon and multiple artifacts to further augment that stats they already have.


@racketgrammar[
 stat
 base-stat
 flat-stat
 percent-stat
 ]
A @deftech{stat} is any attribute mentioned below.


@racketgrammar[
 char-stat
 bast-stat
 crit-stat
 ]
A @deftech{char-stat} is one of the 6 stats a character can have.


@subsection{Base stats}
@(define base-stat-syntax "base stat syntax")
@(define percent-stat-syntax "percent stat syntax")
@(define flat-stat-syntax "flat stat syntax")
@deftogether[(
              @defidform[#:kind base-stat-syntax hp]
               @defidform[#:kind base-stat-syntax atk]
               @defidform[#:kind base-stat-syntax def]
               @defidform[#:kind base-stat-syntax em]
               )]{
 A @deftech{base stat} is one of a characters main stat attributes. They represent a characters hit points, attack, defense, and elemental mastery respectively.
}

@subsection{Flat stats}
@deftogether[(
              @defthing[crit-stat crit-stat?]
               @defidform[#:kind flat-stat-syntax dmg%]
               )]{
 A @deftech{flat stat} is similar to a @tech{base stat}, but does not have a @tech{percent stat} counterpart.
 They represent a characters critical rate, critical damage, and percent damage bonus respectively.
}

@deftogether[(
              @defidform[#:kind flat-stat-syntax critr]
               @defidform[#:kind flat-stat-syntax critd]
               )]{
 A @deftech{crit stat} represents crit rate or crit percentage. They can only be increased by a flat value.
}

@subsection{Percent stats}
@deftogether[(
              @defidform[#:kind percent-stat-syntax hp%]
               @defidform[#:kind percent-stat-syntax atk%]
               @defidform[#:kind percent-stat-syntax def%]
               @defidform[#:kind percent-stat-syntax em%]
               )]{
 A @deftech{percent stat} is a percent scaling of a @tech{base stat}, rather than the base stat itself.
}

@section{Attributes}
An @deftech{attribute} is a calculation that depends on one or more @tech{stats}. There are multiple variations depending on the action that attribute is being used to perform.
@subsection{Buff attributes}
@(define buff-attr-syntax "buff attribute syntax")
@defform*[#:kind buff-attr-syntax
          ((buff attr value)
           (buff base-attr (scale-attr value)))
          #:contracts ([attr stat?]
                       [base-attr base-stat?]
                       [scale-attr percent-stat?]
                       [value number?])]{
 A @deftech{buff attribute} indicates how a buff augments the stats of a character, and can be written in multiple ways.
   
 In the first usage, increases a @tech{base stat} @racket[attr] by @racket[value]. If @racket[attr] is a @tech{percent stat}, then it increases the stat by a percentage equal to (@racket[value]/100).
 
 In the second usage, increases a @tech{base stat} @racket[base-attr] by (@racket[sattr] * (@racket[value]/100)),
 where @racket[sattr] is a @tech{percent stat}.
}

@subsection{Modifier attributes}
@(define mod-attr-syntax "modifier attribute syntax")

@defform*[#:kind mod-attr-syntax
          ((mod char-attr value)
           (mod percent-attr value))
          #:contracts ([char-attr char-stat?]
                       [percent-attr percent-stat?]
                       [value number?])]{
 A @deftech{modifier attribute} indicates how to augment the stats of a character. However, it is more restrictive compared to a @tech{buff attribute}, as it
 does not allow stat increases to be dependent on other @tech{stats} and does not allow @racket{dmg%} increases.
}

@subsection{Damage attributes}
@(define dmg-attr-syntax "damage attribute syntax")
@defform[#:kind dmg-attr-syntax
         (dmg percent-attr value)
         #:contracts ([percent-attr percent-stat?]
                      [value number?])]{
 A @deftech{damage attribute} indicates what @tech{percent-stat} to scale an instance of damage off of by a percentage equal to (@racket[value]/100).
}

@section{Attack keys}
@(define attack-key-syntax "attack key syntax")
@deftogether[(
              @defidform[#:kind attack-key-syntax N]
               @defidform[#:kind attack-key-syntax C]
               @defidform[#:kind attack-key-syntax E]
               @defidform[#:kind attack-key-syntax Q]
               @defidform[#:kind attack-key-syntax ND]
               )]{
 Attack strings are composed of multiple different attacks, and each attack is represented with an @deftech{attack key}. Attack keys can be one of @racket[N], @racket[C],
 @racket[E], @racket[Q], @racket[ND], or @racket[Swap].
}

@defform[(Swap char-index)]{

 Performs a character swap, switching from the current character in the lineup to the character at index @racket[char-index] in the lineup. 
                                                                                                         
}


@section{Elements}
@(define element-syntax "element syntax")
@deftogether[(
              @defidform[#:kind element-syntax pyro]
               @defidform[#:kind element-syntax hydro]
               @defidform[#:kind element-syntax cryo]
               @defidform[#:kind element-syntax electro]
               @defidform[#:kind element-syntax geo]
               @defidform[#:kind element-syntax anemo]
               @defidform[#:kind element-syntax dendro]
               @defidform[#:kind element-syntax physical]
               )]
All attacks and damage instances have an @deftech{element} type associated with them. These elements mainly serve to amplify damage in the event one or more elements is applied to an enemy at a time.
Elements can be one of @racket[pyro], @racket[hydro], @racket[cryo], @racket[electro], @racket[geo], @racket[anemo], @racket[dendro], or @racket[physical].

@section{Triggers}
@(define trigger-syntax "trigger syntax")
@deftogether[(
              @defidform[#:kind trigger-syntax normal-attack]
               @defidform[#:kind trigger-syntax charged-attack]
               @defidform[#:kind trigger-syntax skill]
               @defidform[#:kind trigger-syntax burst]
               )]{
 A @deftech{trigger} is an action performed by a character that may activate a @racket[triggered-buff]. If a trigger matches the trigger of a trigger-buff, then the buff will activate.
 Triggers can be one of @racket[normal-attack], @racket[charged-attack], @racket[skill], or @racket[burst].
}
