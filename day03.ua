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

F ← ⨬(⨬(⊂ 0 +) (⊂ 1 ⊙◌) ⊃⊢⊣:)(⊂ ⊙(↘1)) ⊃⊢⊣ :
⊣/F ≡(⨬(⨬(⊂ 0 IsMul!Mul)(1_1) IsDont .)(1_0) IsDo .) ◫12
