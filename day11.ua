# Experimental!

I ← ⊜⋕⊸≠@  &fras "input/day11.txt"

HasEvenDigits ← =0◿2⧻base₁₀

Next ← ⍣(
  ⊟⊙1°[⊙0]
| [⊃(⊂:°base₁₀↘) (⊂:°base₁₀↙) ⊸(÷2⧻) base₁₀] ⊸(⍤1HasEvenDigits):°⊟
| ⊟ ⊙(×2024) °⊟)
NextR ← ⍉⊟⊃(⊕/+ ⊙⋅∘)(⊕(⊢)⊙⊙◌)⊸⊛ :°⊟⍉∧(⊂ Next): °△ 0_2

Count ← ⊙◌ /+ °⊟ ⍉ ⍥NextR
P₁    ← Count25 ⍉⊟=..
P₂    ← Count75 ⍉⊟=..

⊃(P₂)(P₁) I
