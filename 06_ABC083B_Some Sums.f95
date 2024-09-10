program ABC083B
    !N：総和したい整数の上限
    !A,B：総和する条件(A 以上 B 以下)
    !cnt：A 以上 B 以下であるものの総和
    !digit_arr：1 ~ N を10 進法での各桁で分割した結果
    implicit none
    integer i
    integer N, A, B
    integer cnt
    integer, dimension(:), allocatable :: digit_arr
    !入力
    read (*, *) N, A, B

    !各桁の和が A 以上 B 以下であるものの総和
    do i = 1, N
        call get_digits(i, digit_arr)
        if (sum(digit_arr) >= A .and. sum(digit_arr) <= B) cnt = cnt + i
    end do

    !結果の出力
    write (*, *) cnt

contains
    ! 各桁を取り出すサブルーチン
    subroutine get_digits(N, digit_array)
        integer, intent(in) :: N ! 入力された整数
        integer, dimension(:), allocatable, intent(out) :: digit_array ! 各桁を格納する配列
        character(len=20) :: num_str ! 整数を文字列に変換するための変数
        integer :: i

        ! 整数を文字列に変換
        write (num_str, '(I0)') N

        ! 文字列の長さに基づいて配列を確保
        allocate (digit_array(len_trim(num_str)))

        ! 各桁を取り出して配列に格納
        do i = 1, len_trim(num_str)
            digit_array(i) = ichar(num_str(i:i)) - ichar('0')
        end do
    end subroutine get_digits
end program ABC083B
