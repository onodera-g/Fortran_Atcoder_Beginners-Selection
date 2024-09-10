program ABC086A
    !a,b：正整数a,b
    implicit none
    integer a, b

    !入力
    read (*, *) a, b

    !偶数と奇数の判定
    if (mod(a*b, 2) == 0) then
        write (*, '(a)') 'Even'
    else
        write (*, '(a)') 'Odd'
    end if
end program ABC086A
