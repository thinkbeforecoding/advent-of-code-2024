# "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
&fras "input/day03.txt"
# part 1
⊂ :"            "
IsMul! ← ⨬(0◌)(⨬(0◌)(^0↙⊗@).)/↥∈@).↘4)≍"mul("↙4 .
Mul    ← ⨬(0◌)(/×⊜(⍣⋕0) ⊸≠@,)=1/+∈@,.
/+≡(IsMul!Mul) ◫12

# "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
&fras "input/day03.txt"
⊂ :"            "

IsDo   ← ≍ "do()" ↙4
IsDont ← ≍ "don't()" ↙7

# the input is parsed like this
# [0 x] this is a mul of value x (x=0 when nothing match)
# [1 0] this is a do
# [1 1] this is a don't

# the fold (F) state is
# [d s] where d indicates if summing is disabled, and s is the running total 
# [0 s] [0 x] -> [0 (s+x)] // add x to sum
# [1 s] [0 x] -> [1 s]     // sum is disable, keep existing value
# [_ s] [1 0] -> [0 s]  // enable sum after do 
# [_ s] [1 1] -> [1 s]  // disable sum after don't

F ← ⨬(⨬(⊂ 0 +) (⊂ 1 ⊙◌) ⊃⊢⊣:)(⊂ ⊙(↘1)) ⊃⊢⊣ :
⊣/F ≡(⨬(⨬(⊂ 0 IsMul!Mul)(1_1) IsDont .)(1_0) IsDo .) ◫12
