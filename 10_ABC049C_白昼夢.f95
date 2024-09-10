program ABC049C
    !S   ：文字列S
    !lenS：文字列Sの長さ
    !flag：結果の分岐判定(trueならYes)
    implicit none
    integer i, lenS
    character(len=100000) S
    logical flag

    ! 入力
    read (*, *) S
    lenS = len_trim(s)

    ! 文字列の再現判定(末尾から判定)
    i = lenS
    flag = .true.
    !文字列の再現判定
    do while (i > 0 .and. flag)
        if (i >= 7 .and. s(i - 6:i) == 'dreamer') then
            i = i - 7
        elseif (i >= 6 .and. s(i - 5:i) == 'eraser') then
            i = i - 6
        elseif (i >= 5 .and. s(i - 4:i) == 'dream') then
            i = i - 5
        elseif (i >= 5 .and. s(i - 4:i) == 'erase') then
            i = i - 5
        else
            flag = .false.
        end if
    end do

    ! 結果の出力
    if (flag .eqv. .true.) then
        print *, 'YES'
    else
        print *, 'NO'
    end if
end program ABC049C
