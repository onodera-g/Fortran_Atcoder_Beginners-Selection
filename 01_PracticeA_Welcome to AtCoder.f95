program PracticeA
    !a,b,c：整数a,b,c
    !S    ：文字列S
    implicit none
    integer a, b, c
    character(100) S

    !入力
    read (*, *) a
    read (*, *) b, c
    read (*, *) S

    !結果の出力
    write (*, '(i0,1x,a)') a + b + c, S

end program PracticeA
