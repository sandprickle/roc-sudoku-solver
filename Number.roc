interface Number
    exposes [
        Number,
        fromInt,
        fromIntNormalize,
        fromStr,
        toI8,
        toU64,
        toStr,
        increment,
        fullSet,
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
    imports []

## A number in the range 1-9, inclusive.
Number := I8 implements [Eq, Hash]

fromInt : Int * -> Result Number [OutOfRange]
fromInt = \n ->
    if n >= 1 && n <= 9 then
        Ok (@Number (Num.toI8 n))
    else
        Err OutOfRange

fromIntNormalize : Int * -> Number
fromIntNormalize = \n ->
    num =
        if n < 1 then
            1
        else if n > 9 then
            9
        else
            n

    @Number (Num.toI8 num)

fromStr : Str -> Result Number [InvalidStr]
fromStr = \str ->
    when str is
        "1" -> Ok (@Number 1)
        "2" -> Ok (@Number 2)
        "3" -> Ok (@Number 3)
        "4" -> Ok (@Number 4)
        "5" -> Ok (@Number 5)
        "6" -> Ok (@Number 6)
        "7" -> Ok (@Number 7)
        "8" -> Ok (@Number 8)
        "9" -> Ok (@Number 9)
        _ -> Err InvalidStr

toI8 : Number -> I8
toI8 = \@Number n -> n

toU64 : Number -> U64
toU64 = \@Number n -> Num.toU64 n

toStr : Number -> Str
toStr = \@Number n -> Num.toStr n

increment : Number -> Result Number [MaxValue]
increment = \@Number n ->
    if n < 9 then
        Ok (@Number (n + 1))
    else
        Err MaxValue

fullSet : Set Number
fullSet = Set.fromList all

one = @Number 1
two = @Number 2
three = @Number 3
four = @Number 4
five = @Number 5
six = @Number 6
seven = @Number 7
eight = @Number 8
nine = @Number 9

all = [one, two, three, four, five, six, seven, eight, nine]
