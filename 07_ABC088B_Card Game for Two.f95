program ABC088
    !N：カードの枚数
    !A：各カードの数字
    !Alice：アリスの合計得点
    !Bob：ボブの合計得点
    implicit none
    integer i
    integer N, Alice, Bob
    integer, allocatable::A(:)

    !入力
    read (*, *) N
    allocate (A(N))
    read (*, *) A

    !並び替え
    call margesort(A, N)

    !得点差の計算
    Alice = 0; Bob = 0
    do i = 1, N
        if (mod(i, 2) == 1) then
            Alice = Alice + A(i)
        else
            Bob = Bob + A(i)
        end if
    end do

    !結果の出力
    write (*, *) abs(Alice - Bob)

contains
    subroutine margesort(x, n)
        integer N
        integer x(N), tmp(N)
        integer start, end
        start = 1; end = N
        call loop_margesort(x, tmp, N, start, end)
    end subroutine
    recursive subroutine loop_margesort(x, tmp, N, left, right)
        integer left, right, mid
        integer N
        integer x(N), tmp(N)
        integer i, j, k
        if (left >= right) return
        mid = (left + right)/2
        call loop_margesort(x, tmp, N, left, mid)
        call loop_margesort(x, tmp, N, mid + 1, right)
        j = 0
        tmp(left:mid) = x(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
            j = j + 1
        end do
        i = left
        j = right
        do k = left, right
            if (tmp(i) < tmp(j)) then
                x(k) = tmp(i)
                i = i + 1
            else if (tmp(i) == tmp(j)) then
                x(k) = tmp(i)
                i = i + 1
            else
                x(k) = tmp(j)
                j = j - 1
            end if
        end do
    end subroutine loop_margesort
end program ABC088
