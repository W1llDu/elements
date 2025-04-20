1775
((3) 0 () 1 ((q lib "elements/main.rkt")) () (h ! (equal) ((c form c (c (? . 0) q geo)) q (4106 . 2)) ((c form c (c (? . 0) q C)) q (3915 . 2)) ((c form c (c (? . 0) q charged-attack)) q (4221 . 2)) ((c form c (c (? . 0) q pyro)) q (4022 . 2)) ((c form c (c (? . 0) q applied-buff)) q (3099 . 10)) ((c form c (c (? . 0) q anemo)) q (4125 . 2)) ((c form c (c (? . 0) q define-skill)) q (186 . 14)) ((c form c (c (? . 0) q electro)) q (4083 . 2)) ((c form c (c (? . 0) q normal-attack)) q (4192 . 2)) ((c form c (c (? . 0) q triggered-buff)) q (2701 . 12)) ((c form c (c (? . 0) q E)) q (3935 . 2)) ((c form c (c (? . 0) q dendro)) q (4146 . 2)) ((c form c (c (? . 0) q N)) q (3895 . 2)) ((c form c (c (? . 0) q base-dmg)) q (3779 . 5)) ((c form c (c (? . 0) q define-enemy)) q (1579 . 22)) ((c form c (c (? . 0) q hydro)) q (4042 . 2)) ((c form c (c (? . 0) q buff)) q (3417 . 8)) ((c form c (c (? . 0) q burst)) q (4272 . 2)) ((c form c (c (? . 0) q skill)) q (4251 . 2)) ((c form c (c (? . 0) q physical)) q (4168 . 2)) ((c form c (c (? . 0) q define-attack-sequence)) q (449 . 16)) ((c form c (c (? . 0) q unconditional-buff)) q (2517 . 6)) ((c form c (c (? . 0) q define-team-lineup)) q (2123 . 5)) ((c form c (c (? . 0) q Q)) q (3955 . 2)) ((c form c (c (? . 0) q calculate-raw-rotation-damage)) q (2369 . 6)) ((c form c (c (? . 0) q Swap)) q (3996 . 2)) ((c form c (c (? . 0) q cryo)) q (4063 . 2)) ((c form c (c (? . 0) q mod)) q (3613 . 7)) ((c form c (c (? . 0) q define-character)) q (1073 . 19)) ((c form c (c (? . 0) q ND)) q (3975 . 2)) ((c form c (c (? . 0) q calculate-rotation-damage)) q (2225 . 6)) ((c form c (c (? . 0) q define-weapon)) q (33 . 7)) ((c form c (c (? . 0) q genshin-calc)) q (0 . 2)) ((c form c (c (? . 0) q define-artifact)) q (869 . 10))))
syntax
(genshin-calc expr ...)
syntax
(define-weapon name atk attr buffs ...)
 
  name : identifier?
  atk : number?
  attr : modifier-attribute?
  buffs : buff?
syntax
(define-skill name
  #:cooldown cd
  #:attr attr
  #:duration duration
  #:type type
  buffs ...)
 
  name : identifier?
  cd : number?
  attr : dmg-attribute?
  duration : number?
  type : element?
  buffs : buff?
syntax
(define-attack-sequence name
  ([attr duration type] ...
   #:charged  [attr2 duration2 type2]
   #:plunging [attr3 duration3 type3]))
 
  name : identifier?
  attr : dmg-attribute?
  duration : number?
  type : element?
  attr2 : dmg-attribute?
  duration2 : number?
  type2 : element?
  attr3 : dmg-attribute?
  duration3 : number?
  type3 : element?
syntax
(define-artifact name
  set-name
  main-attr
  sub-attr ...)
 
  name : identifier?
  set-name : string?
  main-attr : modifier-attribute?
  sub-attr : modifier-attribute?
syntax
(define-character name
  #:hp hp #:def def #:atk atk
  #:em em #:critr critr #:critd critd
  #:attacks attacks #:weapon weapon #:skill skill
  #:burst burst #:artifacts artifacts ...)
 
  name : identifier?
  hp : number?
  def : number?
  atk : number?
  em : number?
  critr : number?
  critd : number?
  attacks : identifier?
  weapon : identifier?
  skill : identifier?
  burst : identifier?
  artifacts : identifier?
syntax
(define-enemy name
  #:def def
  #:res (#:pyro pyro
         #:hydro hydro
         #:electro electro
         #:cryo cryo
         #:geo geo
         #:anemo anemo
         #:dendro dendro
         #:physical physical)
  #:reduction reduction)
 
  name : identifier?
  pyro : number?
  hydro : number?
  electro : number?
  geo : number?
  anemo : number?
  dendro : number?
  physical : number?
  reduction : number?
syntax
(define-team-lineup name (chars ...))
 
  name : identifier?
  chars : identifier?
syntax
(calculate-rotation-damage lineup enemy (attk ...))
 
  lineup : identifier?
  enemy : identifier?
  attk : attack-key?
syntax
(calculate-raw-rotation-damage lineup enemy (attk ...))
 
  lineup : identifier?
  enemy : identifier?
  attk : attack-key?
buff syntax
(unconditional-buff [name #:effect attr
                     #:party-wide party-wide])
 
  attr : buff-attribute?
  party-wide : boolean?
trigger buff syntax
(triggered-buff [name #:effect attr
                 #:trigger t
                 #:limit limit
                 #:party-wide party-wide
                 #:duration duration])
 
  attr : buff-attribute?
  t : trigger?
  limit : integer?
  party-wide : boolean?
  duration : number?
trigger buff syntax
(applied-buff [name #:effect attr
               #:limit limit
               #:party-wide party-wide
               #:duration duration])
 
  attr : buff-attribute?
  limit : integer?
  party-wide : boolean?
  duration : number?
buff attribute syntax
(buff attr value)
(buff scaling-attr (scale-attr value))
 
  attr : stat?
  scaling-attr : scaling-stat?
  scale-attr : percent-stat?
  value : number?
modifier attribute syntax
(mod char-attr value)
(mod percent-attr value)
 
  char-attr : char-stat?
  percent-attr : percent-stat?
  value : number?
damage attribute syntax
(base-dmg percent-attr value)
 
  percent-attr : percent-stat?
  value : number?
attack key syntax
N
attack key syntax
C
attack key syntax
E
attack key syntax
Q
attack key syntax
ND
syntax
(Swap char-index)
element syntax
pyro
element syntax
hydro
element syntax
cryo
element syntax
electro
element syntax
geo
element syntax
anemo
element syntax
dendro
element syntax
physical
trigger syntax
normal-attack
trigger syntax
charged-attack
trigger syntax
skill
trigger syntax
burst
