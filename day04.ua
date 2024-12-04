# "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
&fras "input/day04.txt"
⊜∘ ↧ ⊃(≠@\r)(≠@\n) . # parse input

/+⬚0 ≡(/+ ⊜(1◌) ⦷"XMAS" .) .      # count horizontal XMAS
+ /+ ⬚0 ≡(/+⊜(1◌) ⦷"XMAS" . ⇌) ,  # count horizontal SAMX
+ /+⬚0 ≡(/+⊜(1◌) ⦷"XMAS" .) ⍉ ,   # count vertical XMAS
+ /+⬚0 ≡(/+⊜(1◌) ⦷"XMAS" . ⇌) ⍉ , # count vertical SAMX

⊙(⍉ ≡(↻) ⇡ ⧻. ≡(⍜⇌ (⊂".")).) # shift all lines, and transpose to find diagonals
+ /+ ⬚0 ≡(/+⊜(1◌) ⦷"XMAS" .) ,
+ /+ ⬚0 ≡(/+⊜(1◌) ⦷"XMAS" . ⇌) ,
⊙⋅(⍉ ≡(↻¯) ⇡ ⧻. ≡(⍜⇌ (⊂".")).) # same thing but other direction
+ /+ ⬚0 ≡(/+⊜(1◌) ⦷"XMAS" .) ,
+ /+ ⬚0 ≡(/+⊜(1◌) ⦷"XMAS" . ⇌) :

:
≡₂♭ ◫ 3_3
/+♭ ≡₂(⍣(1◌[°$"M_M_A_S_S"]|1◌[°$"M_S_A_M_S"]|1◌[°$"S_M_A_S_M"]|1◌[°$"S_S_A_M_M"]|0 ◌))
