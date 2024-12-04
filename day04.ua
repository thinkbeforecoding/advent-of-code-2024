📜  ← "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
📄  ← &fras "input/day04.txt"
🏗️ ← ⊜∘ ↧ ⊃(≠@\r)(≠@\n) . # parse input

🎄! ← /+♭ ⬚@.⧈(↥∩(≍"XMAS")⟜⇌ ^0) 4_4
P₁ ← /+[⊃(🎄!⊢|🎄!(⊢⍉)|🎄!(≡⊡ ≡⊂ .⇡4 ¤)|🎄!(≡⊡ ≡⊂ ⊸⇌ ⇡4 ¤))]
P₂ ← (
  /+ ♭ ⧈(
    ⍣(1◌ [°$"M_M_A_S_S"]
    | 1◌ [°$"M_S_A_M_S"]
    | 1◌ [°$"S_S_A_M_M"]
    | 1◌ [°$"S_M_A_S_M"]
    | 0◌
    )♭
  ) 3_3)

[⊃P₁P₂] 🏗️📜
[⊃P₁P₂] 🏗️📄
