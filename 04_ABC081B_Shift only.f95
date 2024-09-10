program ABC081B
    !N ：正の整数Aの個数
    !A ：正の整数A
    !cnt：操作回数
    implicit none
    integer N, cnt
    integer, allocatable::A(:)

    !入力
    read (*, *) N
    allocate (A(N))
    read (*, *) A

    !Aを２で割って置き換える操作
    cnt = 0
    do
        if (all(mod(A, 2) == 0)) then
            A = A/2
            cnt = cnt + 1

        else
            exit
        end if
    end do

    !結果の出力
    write (*, *) cnt

end program ABC081B
