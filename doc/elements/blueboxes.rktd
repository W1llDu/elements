2259
((3) 0 () 1 ((q lib "elements/main.rkt")) () (h ! (equal) ((c form c (c (? . 0) q geo)) q (4150 . 2)) ((c form c (c (? . 0) q C)) q (3959 . 2)) ((c form c (c (? . 0) q charged-attack)) q (4265 . 2)) ((c form c (c (? . 0) q pyro)) q (4066 . 2)) ((c form c (c (? . 0) q anemo)) q (4169 . 2)) ((c form c (c (? . 0) q applied-buff)) q (3167 . 10)) ((c form c (c (? . 0) q define-skill)) q (192 . 14)) ((c form c (c (? . 0) q electro)) q (4127 . 2)) ((c form c (c (? . 0) q normal-attack)) q (4236 . 2)) ((c form c (c (? . 0) q dendro)) q (4190 . 2)) ((c form c (c (? . 0) q triggered-buff)) q (2719 . 12)) ((c form c (c (? . 0) q E)) q (3979 . 2)) ((c form c (c (? . 0) q atk)) q (3543 . 2)) ((c form c (c (? . 0) q N)) q (3939 . 2)) ((c form c (c (? . 0) q dmg%)) q (3749 . 2)) ((c form c (c (? . 0) q define-enemy)) q (1585 . 22)) ((c form c (c (? . 0) q em%)) q (3679 . 2)) ((c form c (c (? . 0) q hydro)) q (4086 . 2)) ((c form c (c (? . 0) q buff)) q (3771 . 3)) ((c form c (c (? . 0) q def)) q (3564 . 2)) ((c form c (c (? . 0) q burst)) q (4316 . 2)) ((c form c (c (? . 0) q skill)) q (4295 . 2)) ((c form c (c (? . 0) q physical)) q (4212 . 2)) ((c form c (c (? . 0) q define-attack-sequence)) q (455 . 16)) ((c form c (c (? . 0) q unconditional-buff)) q (2523 . 6)) ((c form c (c (? . 0) q em)) q (3585 . 2)) ((c form c (c (? . 0) q hp%)) q (3605 . 2)) ((c form c (c (? . 0) q critd)) q (3726 . 2)) ((c form c (c (? . 0) q define-team-lineup)) q (2129 . 5)) ((c form c (c (? . 0) q Q)) q (3999 . 2)) ((c form c (c (? . 0) q calculate-raw-rotation-damage)) q (2375 . 6)) ((c form c (c (? . 0) q Swap)) q (4040 . 2)) ((c form c (c (? . 0) q dmg)) q (3894 . 2)) ((c form c (c (? . 0) q atk%)) q (3629 . 2)) ((c form c (c (? . 0) q cryo)) q (4107 . 2)) ((c form c (c (? . 0) q mod)) q (3849 . 2)) ((c form c (c (? . 0) q define-character)) q (1079 . 19)) ((c form c (c (? . 0) q ND)) q (4019 . 2)) ((c form c (c (? . 0) q calculate-rotation-damage)) q (2231 . 6)) ((c form c (c (? . 0) q define-weapon)) q (33 . 7)) ((c form c (c (? . 0) q def%)) q (3654 . 2)) ((c form c (c (? . 0) q critr)) q (3703 . 2)) ((c form c (c (? . 0) q genshin-calc)) q (0 . 2)) ((c form c (c (? . 0) q define-artifact)) q (875 . 10)) ((c form c (c (? . 0) q hp)) q (3523 . 2))))
syntax
(genshin-calc expr ...)
syntax
(define-weapon name damage attr buffs ...)
 
  name : identifier?
  damage : number?
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
  duration : positive?
trigger buff syntax
(applied-buff [name #:effect attr
                     #:limit limit
                     #:party-wide party-wide
                     #:duration duration])
 
  attr : buff-attribute?
  limit : integer?
  party-wide : boolean?
  duration : positive?
base stat syntax
hp
base stat syntax
atk
base stat syntax
def
base stat syntax
em
percent stat syntax
hp%
percent stat syntax
atk%
percent stat syntax
def%
percent stat syntax
em%
flat stat syntax
critr
flat stat syntax
critd
flat stat syntax
dmg%
buff attribute syntax
(buff attr value)
(buff base-attr (sattr percent))
modifier attribute syntax
(mod attr value)
damage attribute syntax
(dmg attr percent)
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
