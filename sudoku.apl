⎕IO←0
N←1+⍳n×n←3
c←{ I←i j k l←(⍴⍵)⊤(,⍵)⍳0 ⋄ ⍵∘{⍵@(⊂I)⊢⍺}¨N~∊(⍵[i;j;;])(⍵[;;k;l])(⍵[i;;k;]) }
s←{t⍣{~0∊⊃⌽⍺},⊂⍵}
t←{(¯1↓⍵),c⊃⌽⍵}
p←(4⍴3)⍴0⍎¨'200370009009200007001004002050000800008000900006000040900100500800007600400089001'
