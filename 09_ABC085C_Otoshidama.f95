program ABC085C
    !N：お札の枚数
    !Y：合計金額
    !xx：10000円札の枚数
    !yy：5000円札の枚数
    !zz：1000円札の枚数
    implicit none
    integer :: N
    integer(16) Y, xx, yy, zz

    ! 入力
    read (*, *) N, Y

    !お札の枚数の検証
    do xx = 0, N
        do yy = 0, N
            zz = N - (xx + yy)
            if (zz >= 0) then
                if (10000_8*xx + 5000_8*yy + 1000_8*zz == Y) then
                    write (*, *) xx, yy, zz
                    stop
                end if
            end if
        end do
    end do
    write (*, *) - 1, -1, -1
end program ABC085C
