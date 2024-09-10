program ABC085B
    !N：餅の数
    !d：餅のサイズ
    !Mochi：最上段の餅のサイズ
    !cnt：餅の段数
    implicit none
    integer N, Mochi, i, cnt
    integer, allocatable ::d(:)

    !入力
    read (*, *) N
    allocate (d(N))
    read (*, *) d

    !ソート
    call margesort(d, N)

    !鏡餅の積み重ね
    Mochi = d(1); cnt = 1
    do i = 2, N
        if (Mochi /= d(i)) then
            cnt = cnt + 1
            Mochi = d(i)
        end if
    end do

    !結果の出力
    write (*, *) cnt

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
end program ABC085B
