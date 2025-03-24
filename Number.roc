module [
    Number,
    from_int,
    from_int_normalize,
    from_str,
    to_i8,
    to_u64,
    to_str,
    increment,
    full_set,
    one,
    two,
    three,
    four,
    five,
    six,
    seven,
    eight,
    nine,
    all,
]

## A number in the range 1-9, inclusive.
Number := I8 implements [Eq, Hash]

from_int : Int * -> Result Number [OutOfRange]
from_int = |n|
    if n >= 1 and n <= 9 then
        Ok(@Number(Num.to_i8(n)))
    else
        Err(OutOfRange)

from_int_normalize : Int * -> Number
from_int_normalize = |n|
    num =
        if n < 1 then
            1
        else if n > 9 then
            9
        else
            n

    @Number(Num.to_i8(num))

from_str : Str -> Result Number [InvalidStr]
from_str = |str|
    when str is
        "1" -> Ok(@Number(1))
        "2" -> Ok(@Number(2))
        "3" -> Ok(@Number(3))
        "4" -> Ok(@Number(4))
        "5" -> Ok(@Number(5))
        "6" -> Ok(@Number(6))
        "7" -> Ok(@Number(7))
        "8" -> Ok(@Number(8))
        "9" -> Ok(@Number(9))
        _ -> Err(InvalidStr)

to_i8 : Number -> I8
to_i8 = |@Number(n)| n

to_u64 : Number -> U64
to_u64 = |@Number(n)| Num.to_u64(n)

to_str : Number -> Str
to_str = |@Number(n)| Num.to_str(n)

increment : Number -> Result Number [MaxValue]
increment = |@Number(n)|
    if n < 9 then
        Ok(@Number((n + 1)))
    else
        Err(MaxValue)

full_set : Set Number
full_set = Set.from_list(all)

one = @Number(1)
two = @Number(2)
three = @Number(3)
four = @Number(4)
five = @Number(5)
six = @Number(6)
seven = @Number(7)
eight = @Number(8)
nine = @Number(9)

all = [one, two, three, four, five, six, seven, eight, nine]
