# make a list of 5x7 (keys or locks)
↯ ⊂ : 7_5 ⊸(÷35 ⧻) ▽ ↧ ⊸(⊃(≠@\r) (≠@\n)) &fras "input/day25.txt"
# indicates if key (first line is dots)
🗝️ ← ≍ "....." ⊢
# count '#' by column -1 to get heights
📊 ← -1/+(=@#)
# separate keys and locks
⊃(▽ ⊸≡🗝️) (▽ ⊸≡(¬ 🗝️))
# convert both to heights
∩(≡📊)
# take all combinations, add heights and check if all are <= 5, sum all
/+ ♭ ⊞(/↧ ≤5 +)
